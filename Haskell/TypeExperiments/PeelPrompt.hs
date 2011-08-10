{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, ImpredicativeTypes #-}
module TypeExperiments.PeelPrompt where

import Control.Exception.Peel
import Control.Monad.Prompt
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Peel
import Control.Monad.IO.Peel

-- first, a useful isomorphism:
data PromptImpl p m where
    PromptImpl :: !(forall a. p a -> m a) -> PromptImpl p m

-- promptToReader :: forall p m a. Monad m => PromptT p m a -> ReaderT (forall n. PromptImpl p n) m a
-- promptToReader = runPromptT return bindP bindM
--     where
--         bindP :: forall t. p t 
--             -> (t -> ReaderT (forall n. PromptImpl p n) m a) 
--             -> ReaderT (forall n. PromptImpl p n) m a
--         bindP p k = ReaderT f >>= k
--             where
--                 f :: (forall n. PromptImpl p n) -> m t
--                 f (PromptImpl g) = g p
--         
--         bindM :: forall t. m t
--             -> (t -> ReaderT (forall n. PromptImpl p n) m a)
--             -> ReaderT (forall n. PromptImpl p n) m a
--         bindM m k = lift m >>= k

-- readerToPrompt :: forall p m a. Monad m => ReaderT (forall n. PromptImpl p n) m a -> PromptT p m a
-- readerToPrompt x = run x' impl
--     where
--         -- Crap.  bitten by quantifier order.
--         run :: ReaderT (forall n. PromptImpl p n) (PromptT p m) a -> PromptImpl p (PromptT p m) -> PromptT p m a
--         run (ReaderT f) x = g x
--             where
--                 g :: (forall n. PromptImpl p n) -> PromptT p m a
--                 g y = f y
--         
--         x' :: ReaderT (forall n. PromptImpl p n) (PromptT p m) a
--         x'   = mapReaderT lift x
--         
--         impl :: PromptImpl p (PromptT p m)
--         impl = PromptImpl (prompt :: forall t. p t -> PromptT p m t)

promptToReader :: forall p m a. Monad m => PromptT p m a -> ReaderT (PromptImpl p m) m a
promptToReader = runPromptT return bindP bindM
    where
        bindP :: forall t. p t 
            -> (t -> ReaderT (PromptImpl p m) m a) 
            -> ReaderT (PromptImpl p m) m a
        bindP p k = ReaderT f >>= k
            where
                f (PromptImpl g) = g p
        
        bindM :: forall t. m t
            -> (t -> ReaderT (PromptImpl p m) m a)
            -> ReaderT (PromptImpl p m) m a
        bindM m k = lift m >>= k

readerToPrompt :: forall p m a. Monad m => ReaderT (PromptImpl p (PromptT p m)) m a -> PromptT p m a
readerToPrompt x = runReaderT (mapReaderT lift x) impl
    where
        impl :: PromptImpl p (PromptT p m)
        impl = PromptImpl (prompt :: forall t. p t -> PromptT p m t)

peelPromptT 
    :: forall a n m o p. (Monad n, Monad o, Monad m) =>
     PromptT p n (PromptT p m a -> m (PromptT p o a))
peelPromptT = readerToPrompt $ do
    undefined
--    f <- peel
--    return (liftM readerToPrompt . f . promptToReader)

instance MonadTransPeel (PromptT p) where
    peel = peelPromptT

instance MonadIO m => MonadIO (PromptT p m) where
    liftIO = lift . liftIO

instance MonadPeelIO m => MonadPeelIO (PromptT p m) where
    peelIO = liftPeel peelIO

-- Now for some tests:
data TestP a where
    Barf :: String -> TestP a

testPIO :: forall t. TestP t -> IO t
testPIO (Barf msg) = fail msg

testP1 :: MonadPeelIO m => PromptT TestP m String
testP1 = do
    prompt (Barf "this should not be caught")

testP2 :: MonadPeelIO m => PromptT TestP m String
testP2 = handle (\(ErrorCall msg) -> return msg)
    (prompt (Barf "this should get caught"))
    
runPromptT' :: forall p m a. Monad m => (forall t. p t -> m t) -> PromptT p m a -> m a
runPromptT' runP = runPromptT return bindP (>>=)
    where
        bindP :: forall t. p t -> (t -> m a) -> m a
        bindP p = (runP p >>=)

