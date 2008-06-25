{-
 -	``ClassDict.hs''
 -	(c) 2008 James Cook
 -}
{-# LANGUAGE
        Rank2Types
  #-}

module ClassDict where

import Prelude hiding (Monad, (>>=), (>>), return, fail, maybe)
import qualified Prelude

--  Given:
-- > class Monad m where
-- >   (>>=) :: m a -> (a -> m b) -> m b
-- >   (>>) :: m a -> m b -> m b
-- >   return :: a -> m a
-- >   fail :: String -> m a

--  Consider that it can be represented as a "dictionary":
data Monad m = Monad
  { (>>=)       :: forall a b. m a    -> (a -> m b) -> m b
  , (>>)        :: forall a b. m a    -> m b        -> m b
  , return      :: forall a.   a      -> m a
  , fail        :: forall a.   String -> m a
  }

maybe :: Monad Maybe
maybe = Monad
  { (>>=) = (Prelude.>>=)
  , (>>) = (Prelude.>>)
  , return = Prelude.return
  , fail = Prelude.fail
  }