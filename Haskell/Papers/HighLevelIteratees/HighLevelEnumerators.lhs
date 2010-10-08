> {-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, GADTs #-}
> module HighLevelEnumerators where
> 
> import HighLevelIteratees
> import Control.Monad.Trans
> import Control.Monad.Error
> import Control.Monad.Prompt
> import qualified System.IO as IO
> import qualified Data.ByteString.Char8 as BS

Going back to the high-level development of Iteratees, I'd like to add what I find to be an elegant definition of enumerators.  Essentially, an Enumerator is a state monad over an arbitrary iteratee.  I would just use StateT but I also want to require my definition of Enumerator to be independent of the iteratee and its return type, which would require impredicative types, and those are deprecated these days.

I haven't yet written much of any explanation about this code, so take it as a brain-dump.  There is probably a lot of room for improvement and clarification.

Note that "feed enum iter1 >> iter2" /= "feed enum (iter1 >> iter2)" - This is unavoibable, and really should be expected:  if "iter2" asks for input, it's just too late in the first case for "enum" to respond.  Additionally, it is very much an open question whether the remaining input (if any) from iter1 should be available in iter2 or should be silently discarded.  Ultimately, I am of the opinion that the former pattern of calls really just ought to be discouraged.

> newtype Enum1 sym m a = Enum1 (forall it t. (Iteratee it, Monad m, Monad (it m), Symbol it ~ sym) => it m t -> it m (it m t, a))
> instance Functor m => Functor (Enum1 sym m) where
>     fmap f (Enum1 e) = Enum1 (liftM (fmap f) . e)
> instance Monad m => Monad (Enum1 sym m) where
>     return x = Enum1 (\it -> return (it, x))
>     Enum1 x >>= f = Enum1 (\it -> do
>         x_it <- x it
>         case x_it of
>             (it', x') -> (\(Enum1 e) -> e) (f x') it')
> instance MonadTrans (Enum1 sym) where
>     lift x = Enum1 (\it -> lift x >>= \r -> return (it, r))

> class Monad m => Enumerator enum m where
>     feed :: (Iteratee it, Symbol it ~ sym, Monad m, Monad (it m)) => enum sym m a -> it m t -> it m (it m t, Either (Stream sym) a)
>     yieldStream :: m (Stream sym) -> enum sym m ()

> instance Monad m => Enumerator Enum1 m where
>     feed (Enum1 e) it = do (it', x) <- e it; return (it', Right x)
>     yieldStream getSyms = Enum1 (\it -> do
>         it <- step it
>         case it of
>             Right x -> return (return x, ())
>             Left  k -> do
>                 syms <- lift getSyms
>                 return (k syms, ()))

> yield :: Enumerator enum m => [sym] -> enum sym m ()
> yield cs = yieldStream (return (Chunks cs))
> yieldEOF :: Enumerator enum m => enum sym m ()
> yieldEOF = yieldStream (return EOF)

The fact that we have to inspect and react to whether the iteratee did anything with our input suggests to me that we might prefer to do something smarter with an iteratee that isn't hungry:  preferably, short-circuit it with something like ErrorT or MaybeT so that once an iteratee is satisfied the whole enumerator can terminate immediately.  Either way, we also want to provide a way to react to the iteratee being done so that we can cleanup any open handles, etc.


I probably shouldn't try to shoehorn these into having the same type for 'yieldStream'.  In fact, I really don't think that's the right interface at all.  Oh well.  The real point is the enumerator types - I think they're a meaningful step in the right direction.

> data EnumError sym e
>     = IterateeFinished (Stream sym)
>     | EnumError e
> instance Error e => Error (EnumError sym e) where
>     noMsg  = EnumError    noMsg
>     strMsg = EnumError . strMsg
> 
> newtype Enum2 e sym m a = Enum2 (ErrorT (EnumError sym e) (Enum1 sym m) a)
>     deriving (Functor, Monad)
> instance Error e => MonadTrans (Enum2 e sym) where
>     lift = Enum2 . lift . lift
> instance (Monad m, Error e) => MonadError e (Enum2 e sym m) where
>     throwError e = Enum2 (throwError (EnumError e))
>     catchError (Enum2 x) h = Enum2 (catchError x h')
>         where
>             h' (EnumError e) = (\(Enum2 y) -> y) (h e)
>             h' other = throwError other

This 'EnumeratorError' class is poorly named, and also its operations arguably should be in the base 'Enumerator' class.

> class Enumerator enum m => EnumeratorError enum m where
>     catchIterateeFinished :: enum sym m a -> (Stream sym -> enum sym m a) -> enum sym m a
>     finally ::
>           enum sym m a
>        -> enum sym m b
>        -> enum sym m a
> 
> instance (MonadError e m, Error e) => EnumeratorError (Enum2 e) m where
>     catchIterateeFinished (Enum2 x) h = Enum2 (catchError x h')
>         where
>             h' (IterateeFinished str) = (\(Enum2 y) -> y) (h str)
>             h' other = throwError other
> 
>     Enum2 x `finally` Enum2 y = Enum2 
>         ((x >>= \r -> y >> return r) `catchError` h)
>         where h err = y >> throwError err
> 

Using `finally', we can implement a nice bracket function that opens a resource, runs all the code that needs it, and guarantees that it'll be safely closed (at least, insofar as it is possible to do so).

> bracket open use close = lift open >>= \rsrc -> (use rsrc `finally` lift (close rsrc))
> 
> instance (Error e, MonadError e m) => Enumerator (Enum2 e) m where
>     feed (Enum2 e) iter = do 
>         mbE <- feed (runErrorT e) iter
>         case mbE of
>             (it, Left s)                            -> return (it, Left s)
>             (it, Right (Left (IterateeFinished s))) -> return (it, Left s)
>             (it, Right (Left (EnumError e)))        -> lift (throwError e)
>             (it, Right (Right x))                   -> return (it, Right x)
>     yieldStream getSyms = Enum2 (ErrorT (Enum1 (\it -> do
>         it <- step it
>         case it of
>             Right x -> return (return x, Left (IterateeFinished (Chunks [])))
>             Left  k -> do
>                 syms <- lift getSyms
>                 return (k syms, Right ()))))

> enumFile path = bracket
>     (logIO "open" (IO.openFile path IO.ReadMode >>= \h -> IO.hSetBuffering h (IO.BlockBuffering (Just 256)) >> return h))
>     (enumHandle 16)
>     (logIO "close" . IO.hClose)
>
> enumHandle bufSiz h = do
>     isEOF <- lift (IO.hIsEOF h)
>     if isEOF then return ()
>         else do
>             buf <- lift (logIO "hGet" (BS.hGet h bufSiz))
>             yield (BS.unpack buf)
>             enumHandle bufSiz h

> logIO msg act = putStr msg >> act

Finally:  as you may have guessed by now (based on my choice of primitives or on Oleg's choice of names) an Enumerator really has nothing at all to do with iteratees except that an iteratee consumes one and through a peculiar inversion of control, the enumerator is given primary control of execution, pretty much for the sole purpose of allowing it to detect when the iteratee stops reading from it.  Aside from that twist, an enumerator is just like a Pythonic "generator" or a Ruby method with a "block" parameter.  So let's make an enumerator type that reflects that notion.  I won't bother making instances, just a function to tranlate this enumerator to any of the others.

> data Yield sym m t where
>     Yield :: m (Stream sym) -> Yield sym m ()
> newtype Enum3 sym m a = Enum3 (PromptT (Yield sym m) m a)
>     deriving (Functor, Monad)
>
> runEnum3 :: (Monad m1, Monad m2) => (m1 (Stream sym) -> m2 ()) -> (forall x. m1 x -> m2 x) -> Enum3 sym m1 t -> m2 t
> runEnum3 y l (Enum3 e) = runPromptT return (bindP y) (\x k -> l x >>= k) e
>     where
>         bindP :: (Monad m1, Monad m2) => (m1 (Stream sym) -> m2 ()) -> Yield sym m1 t -> (t -> m2 r) -> m2 r
>         bindP y (Yield s) k = y s >> k ()
> 
> enum3ToEnum e = runEnum3 yieldStream lift e


> feedAndRun enum iter = do
>     (iter, enumRes) <- feed enum iter
>     iterRes <- iter
>     return (enumRes, iterRes)
