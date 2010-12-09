{-# LANGUAGE
        GeneralizedNewtypeDeriving
  #-}
module Monads.StackTrace where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Sequence as Seq
import Data.Foldable (toList)

newtype StackTraceT f m a = StackTraceT (ReaderT (Seq f) m a)
    deriving (Monad)

runStackTraceT :: StackTraceT f m a -> m a
runStackTraceT (StackTraceT (ReaderT x)) = x Seq.empty

type StackTrace f = StackTraceT f Identity

runStackTrace :: StackTrace f a -> a
runStackTrace x = runIdentity (runStackTraceT x)

-- ReaderT has a pretty stupid Functor instance, so rolling my own:
instance Functor m => Functor (StackTraceT f m) where
    fmap f (StackTraceT (ReaderT g)) = StackTraceT (ReaderT (fmap f . g))

instance Applicative m => Applicative (StackTraceT f m) where
    pure x = StackTraceT (ReaderT (\_ -> pure x))
    StackTraceT (ReaderT f) <*> StackTraceT (ReaderT x)
        = StackTraceT (ReaderT (\e -> f e <*> (x e)))

instance MonadTrans (StackTraceT f) where
    lift = StackTraceT . lift

stack :: Applicative m => StackTraceT f m [f]
stack = StackTraceT (ReaderT (pure . toList))

getStack :: Monad m => StackTraceT f m [f]
getStack = StackTraceT (ReaderT (return . toList))

stackSeq :: Applicative m => StackTraceT f m (Seq f)
stackSeq = StackTraceT (ReaderT pure)

getStackSeq :: Monad m => StackTraceT f m (Seq f)
getStackSeq = StackTraceT (ReaderT return)

frame :: f -> StackTraceT f m a -> StackTraceT f m a
frame f (StackTraceT (ReaderT x)) = StackTraceT (ReaderT (\stk -> x (stk |> f)))

reset :: StackTraceT f m a -> StackTraceT f' m a
reset (StackTraceT (ReaderT x)) = StackTraceT (ReaderT (\_ -> x Seq.empty))

functionM :: Monad m => f -> (a -> m b) -> (a -> StackTraceT f m b)
functionM frm func = \x -> frame frm (lift (func x))