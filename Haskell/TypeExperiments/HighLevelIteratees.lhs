> {-# LANGUAGE GADTs, TypeFamilies, GeneralizedNewtypeDeriving #-}
> module TypeExperiments.HighLevelIteratees where
> import Control.Monad.Trans
> import Control.Monad.Operational
> import Control.Monad.State
> import Control.Monad.Error

Every iteratee implementation and tutorial I've seen is at least moderately low-level.  I find the usual presentation does quite a bit to obscure the topic.  In particular, I had to read quite a few implementations and introductions before I ever even had a good idea of what an iteratee is and why i'd use one.

One of the biggest problems I find is that the various capabilities the iteratee concept is intended to provide are all jumbled up in the implementation, and often don't really disentangle very cleanly at all.  In Oleg Kiselyov's original implementations, he hints at a state-monad semantics, but rightly concludes that something is missing: the ability to suspend the iteratee when it requests more input.  The 'Program' monad (from the 'operational' package) does exactly that with its "instruction" type.  Iteratees can be viewed as 'Program' monads with a very simple instruction 'set' consisting of just one operation:  Get more input.  The rest of the features we have come to expect can also be added by other, more well-known, monad transformers.

Since I'll be introducing several different iteratee implementations, I'll start by defining the operations I expect them to support, along with a few type classes that express those operations:

    * Blocking input requests handled by enumerators, coroutine-style.  'getInput' is a primitive operation that asks an enumerator for more input.  'step' is the other side of the operation; it is the means by which an enumerator runs the iteratee until it requests more input.  We'll also include access to an underlying monad in the specification here.

> data Stream sym = EOF | Chunks [sym]
> class MonadTrans it => Iteratee it where
>     type Symbol it
>     getInput :: Monad m => it m (Stream (Symbol it))
>     step :: Monad m => it m a -> m (Either (Stream (Symbol it) -> it m a) a)

At this point, we can also define a convenient function to run an iteratee:

> evalIteratee :: (Iteratee it, Monad m) => it m a -> m (Maybe a)
> evalIteratee it = step it >>= return . either (const Nothing) Just

    * Lookahead - in particular, we want to be able to get a prefix of the available input without consuming all of it.  We also want to be able to see what input there is without consuming any of it.  Traditionally, an 'unget' operation is also provided, but although all of my implementations could support one, I prefer not to include it because it really doesn't seem like it ought to be possible to put back something that you never took in the first place.

> class Iteratee it => Lookahead it where
>     getSymbol :: Monad m => it m (Maybe  (Symbol it))
>     lookahead :: Monad m => it m (Stream (Symbol it))

Here, we can introduce another useful function to run an iteratee with lookahead and return the unused portion of the input stream:

> runIteratee :: (Lookahead it, Monad m, Monad (it m))
>             =>  it m a -> m (Maybe (a, Stream (Symbol it)))
> runIteratee it = evalIteratee $ do
>     res <- it
>     leftover <- lookahead
>     return (res, leftover)

    * (Traditional) exception handling:

> class Iteratee it => IterateeException it where
>     type Exc it
>     throw  :: Monad m => Exc it -> it m a
>     handle :: Monad m => (Exc it -> it m a) -> it m a -> it m a

A minimalist iteratee, without support for any of the 'fancy' stuff like lookahead, exceptions, etc:

> data Prefetch sym a where
>     Prefetch :: Prefetch sym (Stream sym)

> newtype Iter1 sym m a = Iter1 (ProgramT (Prefetch sym) m a)
>     deriving (Functor, Monad, MonadTrans)
> instance Iteratee (Iter1 sym) where
>     type Symbol (Iter1 sym) = sym
>     getInput = Iter1 (singleton Prefetch)
>     step (Iter1 p) = viewT p >>= \v -> case v of
>         Return x          -> return (Right x)
>         Prefetch :>>= k   -> return (Left (Iter1 . k))

We can easily add lookahead by throwing a state monad into the mix (note that Iter2's getInput doesn't call Iter1's getInput unless the 'Stream' state is EOF or Chunks []):

> newtype Iter2 sym m a = Iter2 (Iter1 sym (StateT (Stream sym) m) a)
>    deriving (Functor, Monad)
> instance MonadTrans (Iter2 sym) where
>     lift = Iter2 . lift . lift
> instance Iteratee (Iter2 sym) where
>     type Symbol (Iter2 sym) = sym
>     getInput = lookahead >>= \stashed -> case stashed of
>         Chunks (_:_) -> do
>             Iter2 (lift (put (Chunks [])))
>             return stashed
>         _         -> do
>             inp <- Iter2 getInput
>             case inp of
>                 Chunks _  -> Iter2 (lift (put (Chunks [])))
>                 EOF       -> Iter2 (lift (put EOF))
>             return inp
>     step (Iter2 i) = do
>         (res,s) <- runStateT (step i) (Chunks [])
>         case res of
>             Right x -> return (Right x)
>             Left k -> case s of
>                 Chunks (_:_) -> step (Iter2 (k s))
>                 _ -> return (Left (Iter2 . k))
> instance Lookahead (Iter2 sym) where
>     lookahead = Iter2 (lift get)
>     getSymbol = do
>         inp <- lookahead
>         case inp of
>             Chunks (c:cs) -> do
>                 Iter2 (lift (put (Chunks cs)))
>                 return (Just c)
>             EOF -> return Nothing
>             _         -> getSymbol

To that we can add exception handling with ErrorT:

> newtype Iter3 e sym m a = Iter3 (ErrorT e (Iter2 sym m) a)
>     deriving (Functor, Monad, MonadError e)
> instance Error e => MonadTrans (Iter3 e sym) where
>     lift = Iter3 . lift . lift
> instance Error e => Iteratee (Iter3 e sym) where
>     type Symbol (Iter3 e sym) = sym
>     getInput = Iter3 (lift getInput)
>     step (Iter3 (ErrorT i)) = do
>         res <- step i
>         case res of
>             Right (Right x) -> return (Right x)
>             Right (Left  e) -> fail "unhandled Error"
>             Left k -> return (Left (Iter3 . ErrorT . k))
> instance Error e => Lookahead (Iter3 e sym) where
>     lookahead = Iter3 (lift lookahead)
>     getSymbol = Iter3 (lift getSymbol)
> instance Error e => IterateeException (Iter3 e sym) where
>     type Exc (Iter3 e sym) = e
>     throw = throwError
>     handle = flip catchError

Note the wart in the 'step' implementation:  If the iteratee terminates with an error that was not handled, all information about it is lost.  This is due to my simplistic class's overly-restrictive type signature for the 'step' function, not any limitation of the 'Iter3' type itself.

There are many other interesting monad transformers that we could put into the stack.  There are also other interesting constructors we could add to our underlying "instruction" GADT.  For example, either technique could be used to implement resumable exceptions (either by adding another ProgramT layer or adding constructors to the 'Prefetch' GADT to reperesent exceptions). 

This construction of Iteratees requires a much broader knowledge base to digest, and a fair amount of syntactic noise with all the lifting and newtype wrapping and unwrapping, but ultimately I find it considerably simpler to understand.  And to be honest, I think that anyone that is able to really understand any of the existing expositions of iteratees probably can digest this one as well.  Much more importantly, it makes iteratees vastly simpler to use;  All of this code need be written once, and you never need to tell your user about how it's implemented, because it builds up the desired capabilities one at a time, introducing a full basis of primitives for each layer.  User code only needs to know how to invoke these primitives.  And this benefit comes merely from the existence of this code as a semantic basis;  your implementation can be as bare-metal as you want, as long as it provides the set of primitives induced by whatever layering of monads you choose as the underlying semantics.

The latter point raises an interesting question.  If you already have an implementation of iteratees, can you easily 'retrofit' a monad-transformer-stack semantics from which to derive an appropriate set of primitives?  Here is a somewhat informal procedure I have found useful for this purpose.

Start by looking at your existing implementation and rewriting it in a simple type-structural notation.  I'll use the implementation from Oleg Kiselyov's original IterateeM.hs as a (quite short) worked example:

  > type ErrMsg = SomeException
  > data Stream el = EOF (Maybe ErrMsg) | Chunk [el]
  > 
  > data Iteratee el m a 
  >     = IE_done a
  >     | IE_cont (Maybe ErrMsg)
  >               (Stream el -> m (Iteratee el m a, Stream el))

Which gives us the basic equations:

ErrMsg = SomeException
Stream(el) = 1 + ErrMsg + List(el)
Iteratee(el,m,a) = a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))

(we won't need to look inside lists, but for completeness recall that List(x) = 1 + x * List(x))

Now, considering this all as a symbolic algebra problem, combine them with some functions corresponding to known monad transformers and manipulate the equations till they reach a suitably simple form.  Here are some equations corresponding to a few monad transformers I've found useful:

StateT(s,m,a) = m(a * s) ^ s
ErrorT(e,m,a) = m(e + a)
ProgramT(instr,m,a) = m (ProgramViewT(instr,m,a))
ProgramViewT(instr,m,a) = a + Σt.(instr(t) * ProgramT(instr,m,a) ^ t)

The ProgramViewT type probably requires a bit of explanation.  Basically, the second component of the sum is a dependent sum:  viewing the 'instr' type as a function (from the codomain of the type's parameters to the structural equation of all parameters to all constructors that can yield that assignment of type parameters),  it says that for any type t, an instruction with type instr(t) and a continuation from t to ProgramT(instr,m,a) is an instance of that term.  For example, the following GADT:

data Foo a b where
    Bar :: Int -> String -> Foo X Y
    Baz :: Foo Z Z

would map to the following function:

Foo(X,Y) = Int * String
Foo(Z,Z) = 1
Foo(_,_) = 0

so "Σ a b.(Foo(a,b) * Bar(a))" would be any constructor of Foo along with a value of type "Bar a" with "a" equal to the first type parameter of Foo in that constructor's type.  TODO: make all this clearer.

Now, a derivation of the Iteratee equations above.  We first hypothesize, based on a hunch, that Iteratee(el,m,a) is isomorphic to ProgramViewT(f,n,b) for some f, n and b:

Iteratee(el,m,a) = ProgramViewT(f,n,b)

a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el)) 
= b + Σt.(f(t) * ProgramT(f,n,b) ^ t)
= b + Σt.(f(t) * n(ProgramViewT(f,n,b)) ^ t)

From here, a ~ b is an easy assumption, which leaves:

(1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))
= Σt.(f(t) * n(ProgramViewT(f,n,b)) ^ t)

Since there is only one continuation in the left expression, we conclude that 'f' has only one value in its range: Stream(t).  We can let f be either:

F(el, 1 + ErrMsg) = Stream(el)

or

F(el, 1)      = Stream(el)
F(el, ErrMsg) = Stream(el)

These assignments correspond to GADTs:

data F el t where F :: Maybe ErrMsg -> F el (Stream el)

or

data F el t where
    F1 :: F el (Stream el)
    F2 :: ErrMsg -> F el (Stream el)

respectively.  So (informally currying F):

(1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))
= (1 + ErrMsg) * n(ProgramViewT(F(el),n,b)) ^ Stream(el))
= (1 + ErrMsg) * n(Iteratee(el,m,a)) ^ Stream(el))

so n(Iteratee(el,m,a)) = m(Iteratee(el,m,a) * Stream(el))

Sounds like the Writer monad transformer (especially when we realize we can indeed make Stream(el) a monoid in a pretty sensible way!):

WriterT(w,m,a) = m(a * w)

So:
n(Iteratee(el,m,a)) = m(Iteratee(el,m,a) * Stream(el))
n = WriterT(Stream(el),m)
Iteratee(el,m,a) = ProgramViewT(F(el),WriterT(Stream(el),m), a)

This is by no means the only possible (or even the only useful) derivation, and it is also by no means rigorous - it essentially treats every type as a totally free algebra, which in the presence of monad and monoid laws, etc., is not the case.  It does, however, provide some direction in a search for meaning - it says at least that the underlying free types are isomorphic.

This derivation yields a monad transformer stack with a different bind operation from the one given in the original paper.  It shifts the responsibility for state passing to the implementation of the 'F' instruction.  Despite the difference, though, I think this derivation provides valid insight into what's really going on in this implementation, if for no other reason than that the 'ProgramViewT' unification was successful:  these Iteratees are (equivalent in some sense to) partially evaluated monadic programs which are either of the form 'return x' for some x or 'singleton (F mbErr) >>= k' for some mbErr and k.  Of course there are also still a few things that are not explained by this statement, but the extent to which this specification is incomplete is quite well-defined.  Semantics of the 'F' instruction, the Monoid instance for Stream el, and what is to be done with WriterT's 'Stream el' is exactly the set of things that remain to be specified.

A better derivation which preserves Oleg's concept of the iteratee as a kind of State monad (based on a different choice of f) would be:

TODO: make a derivation involving StateT: should be possible with F(1) = 1, F(_) = 0, n = StateT(Stream(el),m,a), and the semantics of F being to pause and refill the state.  Although perhaps it can't be derived in this manner, because what we really want is that it is only possible to invoke F when the state is empty.

It is an eye-opening (and highly recommended) exercise to perform this sort of derivation for several different implementations of iteratees and compare the resulting transformer stacks.  It's particularly interesting to note just how widely varied the semantics are.  Even the two main stand-alone implementations on Hackage (the "iteratee" and "enumerator" packages) are subtly different.  The practical consequences of those differences are not at all easy to see from a glance at their implementations, but when restated as monad transformer stacks the differences are quite obvious.  Doing so also sheds light on some past haskell-cafe discussions, especially one regarding error handling in the "enumerator" package.
