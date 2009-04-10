{-
 -      ``CPrelude''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    NoImplicitPrelude,
    TypeOperators,
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction
  #-}

module CPrelude where

import qualified Prelude as P
import Prelude hiding ((>>=), (>>), return, ($), (.), id, curry, uncurry)

import Control.Category
import Control.Functor.Categorical
import Control.Category.Cartesian.Closed
import Control.Category.Hask
import Data.Void

instance CCC (->) (,) Hask Void where
    apply (f,x) = f x
    curry = P.curry
    uncurry = P.uncurry

infixl 0 $
($) = curry apply

-- instance Apply (->) where
--     f $ x = f x


-- infixl 1 >>=
-- (>>=) :: Apply (~>) => a -> (a ~> b) -> b
-- (>>=) = flip ($)
-- 
-- infixl 1 >>
-- (>>) :: a -> b -> b
-- x >> y = undefined
-- 
