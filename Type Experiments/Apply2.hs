{-
 -      ``Apply2''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances,
        RankNTypes
  #-}

module Apply2 where

import Prelude hiding (($), (.), map, (>>=))
import qualified Prelude
import qualified Data.Map as M

infixr 0 $
infixl 1 >>=
infixr 9 .

-- base definitions of application and composition
class Apply a b where
        type Result a b :: *
        ($) :: a -> b -> Result a b

data Compose a b = Compose a b
(.) :: a -> b -> Compose a b
(.) = Compose

--instance (Apply a b, Apply (Result a b) c) => Apply (Compose a b) c where
--        type Result (Compose a b) c = Result (Result a b) c
--        Compose f g $ x = f $ (g $ x)
instance (Apply b c, Apply a (Result b c)) => Apply (Compose a b) c where
        type Result (Compose a b) c = Result a (Result b c)
        Compose f g $ x = f $ (g $ x)

instance (a ~ c) => Apply (a -> b) c where
        type Result (a -> b) c = b
        f $ x = f x

-- problematic: this should be able to have a simple polymorphic return type (Monad m => m v)
instance (Ord k, k ~ k') => Apply (M.Map k v) k' where
        type Result (M.Map k v) k' = Maybe v
        f $ x = M.lookup x f

newtype Map f = Map f
map f x = Map f $ x

instance (Functor f, Apply a b) => Apply (Map a) (f b) where
        type Result (Map a) (f b) = f (Result a b)
        Map f $ xs = fmap (f $) xs

newtype Bind x = Bind x
x >>= f         = Bind f $ x

instance (Monad m, Apply a b, Result a b ~ m c) => Apply (Bind a) (m b) where
        type Result (Bind a) (m b) = Result a b
        Bind f $ x = x Prelude.>>= (f $)
