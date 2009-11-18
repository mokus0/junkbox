{-
 -      ``Adj2''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        OverlappingInstances,
        FlexibleInstances,
        IncoherentInstances
  #-}

module TypeExperiments.Adj2 where

import qualified Data.Map as M

--class Adj f g where
--        left :: (f x -> y) -> (x -> g y)
--        right :: (x -> g y) -> (f x -> y)
--
-- vector space with a given set (type) as basis
newtype (Ord set, Num k) => VectorSpace k set = Vector (M.Map set k)
        deriving (Eq, Show)

vsMap :: (Ord x, Ord y, Num k) => (x -> y) -> (VectorSpace k x -> VectorSpace k y)
vsMap f (Vector m) = Vector (M.mapKeysWith (+) f m)

instance (Num k, Ord x, Show x) => Num (VectorSpace k x) where
        (Vector x) + (Vector y) = Vector (M.unionWith (+) x y)
        (Vector x) - (Vector y) = Vector (M.unionWith (-) x y)
        (Vector x) * (Vector y) = undefined
        fromInteger 0 = Vector (M.empty)
        fromInteger _ = undefined
        abs = undefined
        signum = undefined

basisVector :: (Ord x, Num k) => x -> VectorSpace k x
basisVector x = Vector (M.singleton x 1)

newtype ForgetVectorspace v = ForgetVectorspace v

fvsMap :: (VectorSpace k x -> VectorSpace k y) -> (ForgetVectorspace (VectorSpace k x)-> ForgetVectorspace (VectorSpace k y))
fvsMap f (ForgetVectorspace vs) = ForgetVectorspace (f vs)

class Scale k y where
        scale :: k -> y -> y

instance (RealFrac k) => Scale k Float where
        scale k y = y * realToFrac k

instance (Integral k, Num y) => Scale k y where
        scale k y = y * fromIntegral k

--instance Adj VectorSpace (ForgetVectorspace k) where
left :: (Ord x, Num k) => (VectorSpace k x -> y) -> (x -> ForgetVectorspace y)
left f element = ForgetVectorspace (f (basisVector element))

right :: (Ord x, Num k, Num y, Scale k y) => (x -> ForgetVectorspace y) -> (VectorSpace k x -> y)
right f (Vector v) = sum [ case f x of ForgetVectorspace y -> k `scale` y
                         | (x, k) <- M.assocs v
                         ]
