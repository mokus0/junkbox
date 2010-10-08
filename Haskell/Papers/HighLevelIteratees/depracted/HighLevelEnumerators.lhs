> {-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
> module TypeExperiments.HighLevelEnumerators where
> 
> import TypeExperiments.HighLevelIteratees hiding (Enumerator, feed)
> import Control.Monad.Trans
> import Control.Monad.Error
> import qualified System.IO as IO
> import qualified Data.ByteString.Char8 as BS

Going back to the high-level development of Iteratees, I'd like to revisit and improve upon my definition of enumerators.  Essentially, an Enumerator is a state monad over an arbitrary iteratee.  I would just use StateT but I also want to require my definition of Enumerator to be independent of the iteratee and its return type, which would require impredicative types, and those are deprecated these days.

I haven't yet written much of any explanation about this code, so take it as a brain-dump.  There is probably a lot of room for improvement and clarification.

Note that "feed enum iter1 >>= (>> iter2)" /= "feed enum (iter1 >> iter2)" - This is unavoibable, and really should be expected:  if "iter2" asks for input, it's just too late in the first case for "enum" to respond.  This implementation has an additional weakness, though: due to the type of 'step', any yield operation based on it will lose any unused input in the first case if iter1 terminates.  More generally, any other nonlocal computational effects in the iteratee type are severely disrupted.  

One thought I had to remedy this is to change step's type to "it m a -> it m (Either (Stream (Symbol it) -> it m a) a)" (with the same context as before).  This would require an additional primitive operation to run the whole iteratee.  This approach, to me, seems preferable to the more-obvious idea of adding a 'Stream sym' output to "step", for two reasons:  First, it really is not the enumerator's responsibility to keep track of the iteratee's state (and doing so can make things really really complicated).  Second, that still doesn't address any other effects that might be ongoing in the iteratee, such as exception handling.

After experimenting a bit with those sorts of changes, I have not yet come up with any particular opinion about what a corner case like "feed enum iter1 >>= (>> iter2)" should do, as I have yet to come across or imagine a situation where I would ever want either proposed behaviour.

> newtype Enum1 sym m a = Enum1 (forall it t. (Iteratee it m, Symbol it ~ sym) => it m t -> m (it m t, a))
> instance Functor m => Functor (Enum1 sym m) where
>     fmap f (Enum1 e) = Enum1 (fmap (fmap f) . e)
> instance Monad m => Monad (Enum1 sym m) where
>     return x = Enum1 (\it -> return (it, x))
>     Enum1 x >>= f = Enum1 (\it -> do
>         x_it <- x it
>         case x_it of
>             (it', x') -> (\(Enum1 e) -> e) (f x') it')
> instance MonadTrans (Enum1 sym) where
>     lift x = Enum1 (\it -> x >>= \r -> return (it, r))

> class Monad m => Enumerator enum m where
>     feed :: (Iteratee it m, Symbol it ~ sym) => enum sym m a -> it m t -> m (it m t, Either (Stream sym) a)
>     yieldStream :: m (Stream sym) -> enum sym m ()

> instance Monad m => Enumerator Enum1 m where
>     feed (Enum1 e) it = do (it', x) <- e it; return (it', Right x)
>     yieldStream getSyms = Enum1 (\it -> do
>         it <- step it
>         case it of
>             Right x -> return (return x, ())
>             Left  k -> do
>                 syms <- getSyms
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
>
> catchIterateeFinished :: (Monad m, Error e)
>     => Enum2 e sym m a
>     -> (Stream sym -> Enum2 e sym m a)
>     -> Enum2 e sym m a
> catchIterateeFinished (Enum2 x) h = Enum2 (catchError x h')
>     where
>         h' (IterateeFinished str) = (\(Enum2 y) -> y) (h str)
>         h' other = throwError other
> 
> finally :: (Monad m, Error e)
>    => Enum2 e sym m a
>    -> Enum2 e sym m b
>    -> Enum2 e sym m a
> Enum2 x `finally` Enum2 y = Enum2 
>     ((x >>= \r -> y >> return r) `catchError` h)
>     where h err = y >> throwError err
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
>             (it, Right (Left (EnumError e)))        -> throwError e
>             (it, Right (Right x))                   -> return (it, Right x)
>     yieldStream getSyms = Enum2 (ErrorT (Enum1 (\it -> do
>         it <- step it
>         case it of
>             Right x -> return (return x, Left (IterateeFinished (Chunks [])))
>             Left  k -> do
>                 syms <- getSyms
>                 return (k syms, Right ()))))

> enumFile :: IO.FilePath -> Enum2 IOError Char IO ()
> enumFile path = bracket (IO.openFile path IO.ReadMode) (enumHandle 16) IO.hClose
>
> enumHandle :: Int -> IO.Handle -> Enum2 IOError Char IO ()
> enumHandle bufSiz h = do
>     isEOF <- lift (IO.hIsEOF h)
>     if isEOF then return ()
>         else do
>             buf <- lift (BS.hGet h bufSiz)
>             yield (BS.unpack buf)
>             enumHandle bufSiz h
