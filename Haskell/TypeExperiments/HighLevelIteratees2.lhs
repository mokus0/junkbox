First, let's get the boring stuff out of the way.  This is what GHC needs to be happy with the code in this file:

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, RankNTypes #-}
> module TypeExperiments.HighLevelIteratees2 where
> import Control.Monad.Trans
> import Control.Monad.Operational
> import Control.Monad.State
> import Control.Monad.Error

Every iteratee implementation and tutorial I've seen is at least moderately low-level.  I find the usual presentation does quite a bit to obscure the topic.  In particular, I had to read quite a few implementations and introductions before I ever even had a good idea of what an iteratee is and why i'd use one.  Since then, every serious use I've made of iteratees has involved writing at least one quite convoluted function that did something conceptually quite simple.

One of the biggest problems I find is that the various capabilities the iteratee concept is intended to provide are all jumbled up in the implementation, and often don't really disentangle very cleanly at all.  What I'd like to see is a formal semantics of some kind, starting from a specification of exactly what an iteratee is supposed to do.  This entails, among other things, a set of operations that form a 'basis' - some identifiable set of functions or primitive iteratees from which all other iteratees can be built.  With such a basis in mind, a user never needs to know or care about the implementation except insofar as it affects the performance of their code.

In Oleg Kiselyov's original implementations, he hints at a state-monad semantics, but rightly concludes that something is missing: the ability to suspend the iteratee when it requests more input.  The 'Program' monad (from the 'operational' package) does exactly that with its "instruction" type.  Iteratees can be viewed as 'Program' monads with a very simple instruction 'set' consisting of just one operation:  Get more input.  I believe this monad transformer provides the only previously-missing piece of the "iteratee semantics puzzle".  The rest of the features we have come to expect can be provided by other, more well-known, monad transformers.

With this in mind, I'd like to explore the semantics of iteratees from a different perspective than the usual one.  I'll approach it in two complementary ways:  First, a top-down design starting with a laundry list of features we want iteratees to have and gradually building up a monad transformer stack that implements them.  Second, a bottom up analysis looking at an existing iteratee implementation and reverse-engineering it into an equivalent monad transformer stack.

=== Top-down Iteratee Design === 

This section will be introducing several different iteratee implementations that incrementally add features, so I'll start by defining those features in terms of a few type classes that express the primitive operations associated with them.  This is not intended to be the one true specification or implementation, just an example of an approach I recommend for formally specifying the semantics, rather than the usual approach where the implementation is the only specification.  Once the semantics for any given iteratee implementation are specified and a corresponding set of high-level primitives derived from that specification, real user-level tutorials can be written.

    * First, the bare minimum:  As I see it, to be an iteratee is to provide blocking input requests handled by enumerators, coroutine-style.  'getInput' is a primitive operation that asks an enumerator for more input.  'step' is the other side of that operation; it is the means by which an enumerator runs the iteratee until it requests more input.  We'll also include access to an underlying monad in the specification here, since we know we're going to want that anyway and it'd be rather a waste to run through this whole presentation twice.

> data Stream sym = EOF | Chunks [sym] deriving (Eq, Show)
> isEOF EOF = True
> isEOF _   = False
> nullStream (Chunks (_:_)) = False
> nullStream _              = True
> 
> class MonadTrans it => Iteratee it where
>     type Symbol it
>     getInput :: Monad m => it m (Stream (Symbol it))
>     step :: Monad m => it m a -> it m (Either (Stream (Symbol it) -> it m a) a)
> 
> end :: (Iteratee it, Monad m, Monad (it m)) => it m a -> it m a
> end it = step it >>= either ($EOF) return

    * Second, for practical iteratees we'll also want lookahead - in particular, we want to be able to get a prefix of the available input without consuming all of it.  We also want to be able to see what input is available without consuming any of it or causing the enumerator to do any additional work.  Traditionally, an 'unget' operation is also provided, but I prefer not to include it even though all of the implementations here could support one.  It really doesn't seem like it ought to be possible to put "back" whatever you want.  In a real-world implementation I might expect to see an 'unget' operation in a separate "_.Internal" module or something, with its use discouraged and a tacit expectation that speed freaks will probably make use of it anyway.

> class Iteratee it => Lookahead it where
>     getSymbol :: Monad m => it m (Maybe  (Symbol it))
>     lookahead :: Monad m => it m (Stream (Symbol it))

    * (Traditional) exception handling:  The MonadError class is actually sufficient for this purpose, but let's make it explicitly about iteratees anyway just for emphasis.

> class Iteratee it => IterateeError it where
>     type Exc it
>     throw  :: Monad m => Exc it -> it m a
>     handle :: Monad m => (Exc it -> it m a) -> it m a -> it m a

Now for some implementations.  Here's a minimalist iteratee, without support for any of the 'fancy' stuff like lookahead, exceptions, etc:

> data Fetch sym a where
>     Fetch :: Fetch sym (Stream sym)

> newtype Iter1 sym m a = Iter1 (ProgramT (Fetch sym) m a)
>     deriving (Functor, Monad, MonadTrans)
> instance Iteratee (Iter1 sym) where
>     type Symbol (Iter1 sym) = sym
>     getInput = Iter1 (singleton Fetch)
>     step (Iter1 p) = lift (viewT p) >>= \v -> case v of
>         Return x      -> return (Right x)
>         Fetch :>>= k  -> return (Left (Iter1 . k))
> 
> runIter1 i = do
>     let Iter1 p = end i
>     v <- viewT p
>     case v of
>         Return x  -> return (Just x)
>         _         -> return Nothing

We can easily add lookahead by throwing a state monad into the mix (note that Iter2's getInput doesn't call Iter1's getInput unless the 'Stream' state is EOF or Chunks []):

> newtype Iter2 sym m a = Iter2 (Iter1 sym (StateT (Stream sym) m) a)
>    deriving (Functor, Monad)
> instance MonadTrans (Iter2 sym) where
>     lift = Iter2 . lift . lift
> 
> runIter2 (Iter2 i) = runStateT (runIter1 i) (Chunks [])
> 
> instance Iteratee (Iter2 sym) where
>     type Symbol (Iter2 sym) = sym
>     getInput = do
>         stashed <- lookahead
>         Iter2 $ if nullStream stashed
>             then do
>                 inp <- getInput
>                 lift (put (if isEOF inp then EOF else Chunks []))
>                 return inp
>             else do
>                 lift (put (Chunks []))
>                 return stashed
>     step (Iter2 i) = do
>         res <- Iter2 (step i)
>         case res of
>             Right x               -> return (Right x)
>             Left k    -> do
>                 s <- lookahead
>                 if nullStream s
>                     then return (Left (Iter2 . k))
>                     else do
>                         Iter2 (lift (put (Chunks [])))
>                         step (Iter2 (k s))
> 
> instance Lookahead (Iter2 sym) where
>     lookahead = Iter2 (lift get)
>     getSymbol = do
>         stashed <- lookahead
>         case stashed of
>             Chunks (c:cs) -> do
>                 Iter2 (lift (put (Chunks cs)))
>                 return (Just c)
>             Chunks [] -> do
>                 inp <- getInput
>                 Iter2 (lift (put inp))
>                 getSymbol
>             EOF -> return Nothing

To that we can add exception handling with ErrorT.  This is the reason we exposed the 'm' parameter in the Iteratee class - we need something sane to do with unhandled exceptions in the iteratee, so we require that they be throwable in the underlying monad too.  This requirement could easily be dropped by changing the type of 'step', I just chose this approach to keep things simple for these examples.

> newtype Iter3 e sym m a = Iter3 (ErrorT e (Iter2 sym m) a)
>     deriving (Functor, Monad, MonadError e)
> instance Error e => MonadTrans (Iter3 e sym) where
>     lift = Iter3 . lift . lift
> 
> runIter3 (Iter3 i) = runIter2 (runErrorT i)
> 
> instance Error e => Iteratee (Iter3 e sym) where
>     type Symbol (Iter3 e sym) = sym
>     getInput = Iter3 (lift getInput)
>     step (Iter3 i) = Iter3 $ do
>         res <- lift (step (runErrorT i))
>         case res of
>             Right (Right x) -> return (Right x)
>             Right (Left  e) -> throwError e
>             Left k -> return (Left (Iter3 . ErrorT . k))
> 
> instance Error e => Lookahead (Iter3 e sym) where
>     lookahead = Iter3 (lift lookahead)
>     getSymbol = Iter3 (lift getSymbol)
> 
> instance Error e => IterateeError (Iter3 e sym) where
>     type Exc (Iter3 e sym) = e
>     throw = throwError
>     handle = flip catchError

There are many other interesting monad transformers that we could put into the stack.  There are also other interesting constructors we could add to our underlying "instruction" GADT.  For example, either technique could be used to implement resumable exceptions (either by adding another ProgramT layer or by adding constructors to the 'Fetch' GADT to reperesent exceptions). 

This construction of Iteratees requires a much broader knowledge base to digest, and a fair amount of syntactic noise with all the lifting and newtype wrapping and unwrapping, but ultimately I find it considerably simpler to understand, because it involves combining a small number of already-well-understood concepts.  And to be honest, I think that anyone that is able to really understand any of the existing expositions of iteratees probably can digest this one as well.  Much more importantly, it makes iteratees vastly simpler to use;  All of this code need be written once, and you never need to tell your user about how it's implemented, because you can easily infer a full set of primitives for the whole construction - it's just the primitives of the various parts, minus any you don't wish to expose to the user.  This benefit comes merely from the existence of this kind of model - your implementation need not be the same.  It can be as bare-metal as you want, as long as it provides the set of primitives induced by whatever layering of monads you choose as the underlying semantics.

=== Bottom-up Iteratee Analysis === 

That last point raises an interesting question.  If you already have an implementation of iteratees, can you easily ``retrofit'' a monad-transformer-stack semantics from which to derive an appropriate set of primitives?  Here is a somewhat informal procedure I have found useful for this purpose.

Start by looking at your existing implementation and rewriting it in a simple type-structural notation.  I'll use the implementation from Oleg Kiselyov's original IterateeM.hs as a worked example:

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

ReaderT(r,m,a) = m(a) ^ r
WriterT(w,m,a) = m(a * w)
StateT(s,m,a) = ReaderT(s,m,WriterT(s,m,a)) = m(a * s) ^ s
ErrorT(e,m,a) = m(e + a)
ProgramT(instr,m,a) = m (ProgramViewT(instr,m,a))
ProgramViewT(instr,m,a) = a + Σ t.(instr(t) * ProgramT(instr,m,a) ^ t)

The ProgramViewT equation probably requires a bit of explanation.  The "Σ t." component corresponds to existential quantification over a new type variable "t".  Interpreting the 'instr' type as a function that maps each set of type parameters to the equation for all constructors that can yield that assignment of type parameters, the notation means exactly what one would expect from the summation operation in high-school algebra.  For example, the following GADT:

data Foo a b where
    Bar :: Int -> String -> Foo X Y
    Baz :: Foo Z Z

would map to the following function:

Foo(X,Y) = Int * String
Foo(Z,Z) = 1
Foo(_,_) = 0

so "Σ a b.(Foo(a,b) * Bar(a))" would expand to "Foo(X,Y) * Bar(X) + Foo(Z,Z) * Bar(Z)", which (by evaluating Foo) simplifies to "Int * String * Bar(X) + Bar(Z)".  Thus we can conclude that there is at least one isomorphism between "forall a b. (Foo a b, Bar a)" and "Either (Int,String,Bar X) (Bar Z)".

With all this in mind, here are two rewrites of the Iteratee equations above.  We first hypothesize, based on superficial similarities of the corresponding equations, that Iteratee(el,m,a) is isomorphic to ProgramViewT(f,n,b) for some f, n and b:

Iteratee(el,m,a) = ProgramViewT(f,n,b)

a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el)) 
= b + Σt.(f(t) * ProgramT(f,n,b) ^ t)
= b + Σt.(f(t) * n(ProgramViewT(f,n,b)) ^ t)

From here, a = b is an easy assumption, which leaves:

(1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))
= Σt.(f(t) * n(ProgramViewT(f,n,b)) ^ t)

There are at least two possible ways to unify these two expressions, arising from different possible definitions of the function 'f'.  One way is to declare that 'f' has only one value in its range: Stream(t).  We can let f be either:

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

n(Iteratee(el,m,a)) = m(Iteratee(el,m,a) * Stream(el))
n = WriterT(Stream(el),m)
Iteratee(el,m,a) = ProgramViewT(F(el),WriterT(Stream(el),m), a)

It is important to note that this is only an isomorphism of types, and in particular does _NOT_ say that the Monad operations that would be provided by library implementations of these monad transformers are the same as the original implementation's.  It is most certainly not; Oleg's implementation had state-passing machinery in the bind operation, whereas this one involves concatenating unused chunks and would require the "enumerator" machinery to feed unused chunks back into the iteratees.  The fact that we have an isomorphism of types, though, means that we can push the implementation's existing operations through to the new type - also a valuable exercise because it lets us restate exactly what the implementation was doing in a language of our choosing.

Going back to the choice of 'f', (which we'll start calling 'g' to emphasize that this is a different function to the one from before) another sensible choice would be to let the unit type be the only element of 'g''s range:

G(1 + ErrMsg) = 1
G(_) = 1

or, as a GADT:

data G where G :: Maybe ErrMsg -> G ()

In other words, G is an operation with _only_ side effects.  This reinforces Oleg's informal notion of the iteratee's continuation state as a 'resumable exception'.  It requests outside intervention to make things better - more input, handle some exception, etc.  Our equations are now:

(1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))
= Σt.(G(t) * n(ProgramViewT(G,n,a)) ^ t)
= (1 + ErrMsg) * n(ProgramViewT(G,n,a)) ^ 1
= (1 + ErrMsg) * n(ProgramViewT(G,n,a)

n(ProgramViewT(G,n,a) 
= (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el))
= StateT(Stream(el),m,Iteratee(el,m,a))

So putting it all together:

Iteratee(el,m,a)
= a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ Stream(el)) 
= a + (1 + ErrMsg) * StateT(Stream(el),m,Iteratee(el,m,a))
= a + Σt.(G(t) * StateT(Stream(el),m,ProgramViewT(G,n,a)) ^ t)
= ProgramViewT(G,StateT(Stream(el),m),a)

This is a nicer conclusion than the previous one because it formalizes Oleg's informal notion that an Iteratee is a kind of a state monad.  From this definition we can see that it really is.  Keep in mind, again, that the operations we get for free from the StateT type are not _exactly_ the same as the ones we get when we push the original type's capabilities through our isomorphism.  It's mostly a standard state monad, but due to the original implementation of (>>=), the "G Nothing" operation is effectively a no-op unless the state is empty.

Incidentally, the fact that this Iteratee implementation is equivalent to ProgramViewT and not to ProgramT exposes a subtle problem with the implementation (although it will have been obvious to some already):  This Iteratee type is _NOT_ a monad transformer.  It violates the law that requires "lift . return = return" - "lift (return x)" will ask the enumerator for input before passing on "x" to the rest of the program.  I'm not entirely sure whether it obeys the monad laws either, though I suspect it does.  I have checked the identity laws but not associativity.

It is an eye-opening (and highly recommended) exercise to perform this sort of derivation for several different implementations of iteratees and compare the resulting transformer stacks.  It's particularly interesting to note just how widely varied the semantics are.  Even the two main stand-alone implementations on Hackage (the "iteratee" and "enumerator" packages) are subtly different.  The practical consequences of those differences are not at all easy to see from a glance at their implementations, but when restated as monad transformer stacks the differences are quite obvious.  Doing so also sheds light on some past haskell-cafe discussions, especially one regarding error handling in the "enumerator" package.
