-- Thought experiment.  Consider the 'traditional' idea of a parser as a
-- list transducer, and its 'traditional' realization as a parser combinator
-- library.  What would it look like to generalize 'list' here to an arbitrary
-- pattern functor, or even to a mutually recursive system of functors?
-- 
-- As a specific example, consider 'Data.Tree.Tree'...
-- 
-- This is actually motivated by a practical question - what is a good basis
-- for an "XML transducer combinators" library?
module TypeExperiments.TreeParser where

import Control.Applicative
import Data.Tree

-- simple generalization of ReadS:
type P t r = Forest t -> [(r, Forest t)]
-- For more realistic usage, maybe something like:
-- > type P t r = Forest t -> [Either (S.Set String) (r, Forest t)]
-- (where "Left" constructors are sets of "expected" symbols).  Or
-- a more Parsec-like type, using CPS and bounded backtracking with a 'try'
-- combinator.
newtype Parse t r = Parse { parse :: P t r }

instance Functor (Parse t) where
    fmap f (Parse p) = Parse (\i -> [(f r, rs) | (r,rs) <- p i])
instance Monad (Parse t) where
    return x = Parse (\i -> [(x, i)])
    Parse x >>= f = Parse $ \i ->
        [ (r2, rest2)
        | (r1, rest1) <- x i
        , let Parse p = f r1
        , (r2, rest2) <- p rest1
        ]
    fail x = Parse (const [])
instance Applicative (Parse t) where
    pure = return
    f <*> x = do
        f' <- f
        x' <- x
        return (f' x')
instance Alternative (Parse t) where
    empty = fail ""
    Parse x <|> Parse y = Parse (\i -> x i ++ y i)

-- Apparently this is the only primitive necessary...
nodeSatisfy :: (t -> Bool) -> Parse t r -> Parse t (t, r)
nodeSatisfy p (Parse f) = Parse node'
    where
        node' (Node l cs : rest)
            | p l = 
                [ ((l, x), rest)
                | x <- parseForest (Parse f) cs
                ]
        node' _ = []

node :: Eq t => t -> Parse t r -> Parse t r
node x f = fmap snd (nodeSatisfy (x==) f)

anyNode :: Parse t r -> Parse t (t, r)
anyNode = nodeSatisfy (const True)

leafSatisfy :: (t -> Bool) -> Parse t t
leafSatisfy p = fmap fst (nodeSatisfy p end)

anyLeaf :: Parse t t
anyLeaf = leafSatisfy (const True)

leaf :: Eq t => t -> Parse t t
leaf x = leafSatisfy (x==)

end :: Parse t ()
end = Parse (\i -> if null i then [((), i)] else [])

parseForest :: Parse t r -> Forest t -> [r]
parseForest (Parse p) f = map fst (p f)

parseTree :: Parse t r -> Tree t -> [r]
parseTree p t = parseForest (do r <- p; end; return r) [t]

sample :: Forest String
sample =
    [ Node "hello"
        [ Node "world"      []
        , Node "everybody"  []
        ]
    , Node "goodbye"
        [
        ]
    ]

-- Hypothesis:
-- in the general pattern functor case,
-- the primitive parsers for that pattern functor consist of:
--      one parser for each constructor, with one argument for each
--      parameter of that constructor.  That argument is a parser for the
--      type of the component.  The parser then returns a tuple of each
--      result of each sub-parser.  For example:
--
--  data Foo x y z
--      = A x y
--      | B x   z
--      | C   y z
--
--  The primitive parsers have the following types:
--  
--      a :: Parser x r1 -> Parser x r2 -> Parser (Foo x y z) (r1, r2)
--      b :: Parser x r1 -> Parser z r2 -> Parser (Foo x y z) (r1, r2)
--      c :: Parser y r1 -> Parser z r2 -> Parser (Foo x y z) (r1, r2)
--
--  In particular, the traditional "applicative" instance comes not from the 
--  fact that parsing is "naturally" applicative, but from the fact that the
--  traditional input (a list) is a monoid.  liftA2 (,) is the natural parser
--  corresponding to the (++) operator:
--      
--      nil :: Parser [x] ()
--      cons :: Parser x r1 -> Parser [x] r2 -> Parser [x] (r1, r2)
--
--      pure x = fmap (const x) empty
--      liftA2 (,) :: Parser [x] r1 -> Parser [x] r2 -> Parser [x] (r1, r2)
--      f <*> x = fmap (uncurry ($)) (liftA2 (,) f x)
--  
--  With this in mind, where does (>>=) come from?  From 'fmap' and 'join':
--      
--      fmap (as hinted above) just returned value.  That's pretty natural.
--      
--      join :: Parser t (Parser t r) -> Parser t r
-- 
--  This "join" doesn't really work, because in the generalized setting the
--  concept of "consumed" and "remaining" is not at all obvious.  In the list
--  case it works out pretty nicely because there's only one thread, but in
--  our 'Foo' case, it's not so pretty.  Perhaps we need to reconsider our
--  transformation.  What we want is that when matching one of the constructors,
--  say 'A', we ONLY match that constructor, and make its arguments somehow
--  available as "remaining" inputs.
-- 
--  Perhaps something like:
--  
--      a :: Parser (Foo x y z) (x, y)
--      b :: Parser (Foo x y z) (x, z)
--      c :: Parser (Foo x y z) (y, z)
--  
--  And the list version becomes:
--
--      nil :: Parser [x] ()
--      cons :: Parser [x] (x, [x])
--  
--  Now, what transformation is it that (naturally) makes the [x] component
--  into an implicit "remaining input"?  I've got a feeling there's nothing
--  natural about it.  I suspect it's more like a simple left-to-right
--  dependency added "arbitrarily" because it's convenient, making 'cons'
--  into:
--
--      cons :: Parser [x] r1 -> (r1 -> Parser [x] r2) -> Parser [x] r2
--
--  Applying the same transformation to the 'Foo' parsers gives:
--  
--      
--      a :: Parser x r1 -> (r1 -> Parser x r2) -> Parser (Foo x y z) r2
--      b :: Parser x r1 -> (r1 -> Parser z r2) -> Parser (Foo x y z) r2
--      c :: Parser y r1 -> (r1 -> Parser z r2) -> Parser (Foo x y z) r2
--  
--  Which is an interesting "parameterized monad" sort of structure.  
--  Unfortunately,  it exhibits a rather arbitrary directional bias.  After
--  all, why not the following?
-- 
--      a :: (r2 -> Parser x r1) -> Parser x r2 -> Parser (Foo x y z) r1
--      b :: (r2 -> Parser x r1) -> Parser z r2 -> Parser (Foo x y z) r1
--      c :: (r2 -> Parser y r1) -> Parser z r2 -> Parser (Foo x y z) r1
--  
--  Or for that matter, why not this?
-- 
--      cons :: (r2 -> Parser x r1) -> Parser [x] r2 -> Parser [x] r1
--
--  Other than the obvious ("that's pretty useless in practice"), there's
--  actually no reason it couldn't be this way.  There's actually also no
--  reason the same "basis" can't include both:
-- 
--      nil :: Parser [x] ()
--      consL :: Parser x r1 -> Parser [x] r2 -> Parser [x] (r1, r2)
--      consR :: (r2 -> Parser x r1) -> Parser [x] r2 -> Parser [x] r1
-- 
--  Are consL and consR interdefineable?  Probably not.
--  Both are defineable from the aforementioned 'cons :: Parser [x] (x, [x])',
--  though, if we take something like the following as generic primitives:
--
--      parse :: Parser x y -> Parser y z -> Parser x z
--      arr   :: (x -> y) -> Parser x y
--
--  I'm not sure how useful this is though.  At this point, have we watered down
--  the 'Parser' type so much that it's just another name for '->'?
-- 
--  Then again, is it a bad thing if so?  Perhaps what we have, in the end,
--  is actually just a useful reification of pattern matching?  Or, thought of
--  another way, perhaps all we really want is a version of '->' with an extra
--  "case composition" which just corresponds to the sort of transformation that
--  could take this:
--      
--      f = (\Nothing -> foo)
--      g = (\Just x  -> bar)
--  
--  and give this:
--
--      h = f <|> g
--        = \x -> case x of
--              Nothing -> foo
--              Just x  -> bar
--
--  Interesting.  To be honest, I find this quite appealing.  Parsers are just
--  reified pattern matches?  That actually makes a whole lot of sense.
--  And it even explains the "monadicity":  Pattern matches bind variables, 
--  which is, ultimately, what monads are all about - extending an arbitrary
--  language with variables.
--  
--  And applicative parsers are then just a restricted version where there's 
--  only one "top-level" pattern match (or the expression can be restructured
--  that way), as opposed to the general case where there can be different 
--  matching done in different branches of the overall expression.  
-- 
--  Fascinating.
--  
--  So, under this interpretation, we have for any arbitrary data type, one
--  "primitive parser" for each constructor.  That parser is precisely 
--  equivalent in spirit to @\(Con x y z) -> (x,y,z)@, etc.
-- 
--  In order to make a "monadic" or "applicative" parser, we additionally 
--  define a concept of "direction" of flow - that is, for each parser, we
--  distinguish one of its results as semantically "after" the others, and
--  promote this to a "hidden state", and call it "the rest of the input".
--  
--  That's still rather ad-hoc though, and I think we can do better.  This
--  actually makes me think of state monads in general from a new perspective.
--  Rather than a "sequence of stateful operations", objects in a state monad
--  can be thought of as an endofunction (on the state type) which happens to
--  have an extra "control channel" which dynamically assembles the final 
--  endofunction.
--  
--  The (>>) operation is, in this view, the most primitive/natural operation.
--  It's just concatenation of endofunctions, without any interaction of the
--  "control logic" that did the assembling of the pieces.  (>>=) is an
--  operation that does the same thing, except that it allows the controller
--  that built the first part of the function to pass information to the
--  controller that is going to build the second part.  It's, under a weird
--  sort of intuition, "twisting the function sideways".
--  
--  Which makes me wonder - can I apply this "twist" to things other than 
--  endofunctions?  Sure!  We can generalize from endofunctor-building to
--  general-function-building, giving a "parameterized state monad" which
--  would have @return :: t -> m x x t@ and @(>>=) :: m x y t -> (t -> 
--  m y z u) -> m x z u@.  In this case, the (>>)-as-(.) interpretation 
--  becomes even more obvious.  Incidentally, reverse-propagating state 
--  monads don't seem so weird anymore either - it's just a different choice
--  of the order of composition - either way is associative, so nothing bad
--  happens except that we confuse the heck out of ourselves when we try to
--  use (>>=) to violate causality (after all, (>>=) can only pass information
--  "forward") and our little universe explodes.
--  
--  We can generalize even further: from functions (arrows in so-called 
--  "Hask", the category of Haskell types of kind *) to arrows in any 
--  arbitrary category.  In particular, the category we just invented -
--  parsers!  In this case, we have _two_ useful "compositions" - the 
--  normal arrow composition and the parallel composition.  The latter yields
--  "Alternative" and "MonadPlus", which is not the least bit surprising.
--  
--  So, to sum up: Parsers can be thought of as a model of partial functions
--  with two composition operators: the normal function composition and
--  a "parallel" composition that evaluates one function and, if it is
--  "formally undefined" at the input (which must be distinguished as "formal"
--  to contrast it with "bottom" which is "uncomputably undefined"), it 
--  evaluates the other.
--  
--  A state monad can be thought of (somewhat informally - though I think it
--  can be formalized) as a functor from the category of types and endofunctions
--  on those types to "endofuntion-building-monads".  Generalizing the construction
--  to apply to arbitrary source categories, and applying that generalized
--  construction to the category of "parsers", aka "partial functions with two
--  kinds of composition", yields "monadic parsers", which have very nice
--  idiomatic properties.  In the particular case of the list, we can combine
--  the 'cons' primitive parser with a state-"monadoid" operation that separates
--  the head (returning it) and the tail (passing it on in the "state") to eliminate
--  the need to parameterize the monad-thing, so parsers on lists are thus
--  reduced to a "traditional" monad.
