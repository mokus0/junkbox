{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, RankNTypes #-}
module TypeExperiments.AlternativeProgram where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Operational

data Alt p m a
    = Empty
    | Ask (p a)
    | Par (AltProgramT p m a) (AltProgramT p m a)

newtype AltProgramT p m a = AltProgramT { unAltProgramT :: ProgramT (Alt p m) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (AltProgramT p) where lift = AltProgramT . lift

runAltProgramT :: (Monad m, Alternative m) => (forall t. p t -> m t) -> AltProgramT p m a -> m a
runAltProgramT f (AltProgramT p) = do
    v <- viewT p
    case v of
        Return x        -> return x
        Ask p :>>= k    -> f p >>= runAltProgramT f . AltProgramT . k
        Par x y :>>= k  -> (runAltProgramT f x <|> runAltProgramT f y) >>= runAltProgramT f . AltProgramT . k

instance Monad m => Alternative (AltProgramT p m) where
    empty   = AltProgramT (singleton Empty)
    x <|> y = AltProgramT (singleton (Par x y))

data ViewP p m a where
    Done :: a -> ViewP p m a
    Blocked :: p a -> (a -> AltProgramT p m b) -> ViewP p m b
    Forked :: m (ViewP p m a) -> m (ViewP p m a) -> (a -> AltProgramT p m b) -> ViewP p m b

viewP :: (Monad m, Alternative m) => AltProgramT p m a -> m (ViewP p m a)
viewP (AltProgramT p) = do
    v <- viewT p
    case v of
        Return x        -> return (Done x)
        Empty :>>= k    -> empty
        Ask p :>>= k    -> return (Blocked p (AltProgramT . k))
        Par x y :>>= k  -> return (Forked (viewP x) (viewP y) (AltProgramT . k))

unViewP :: (Monad m, Alternative m) => ViewP p m a -> AltProgramT p m a
unViewP (Done x)        = return x
unViewP (Blocked p k)   = AltProgramT (singleton (Ask p)) >>= k
unViewP (Forked l r k)  = ((lift l >>= unViewP) <|> (lift r >>= unViewP)) >>= k
