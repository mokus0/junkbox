{-# LANGUAGE
        TypeFamilies,
        MultiParamTypeClasses,
        GeneralizedNewtypeDeriving
  #-}
module TypeExperiments.UnboxVector where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

newtype Foo = Foo (Int, Double)
    deriving (Eq, Ord, Show, 
              G.Vector U.Vector,
              M.MVector U.MVector,
              U.Unbox)
