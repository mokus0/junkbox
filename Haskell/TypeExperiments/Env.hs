{-# LANGUAGE GADTs #-}
module TypeExperiments.Env where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Dynamic
import TypeExperiments.Uniq
import TypeExperiments.GCompare
import Control.Monad.Primitive
import TypeExperiments.Dependent

-- |Internal
data Key f where Key :: !(f a) -> Key f
instance GCompare f => Eq (Key f) where
    Key a == Key b = case gcompare a b of
        GEQ -> True
        _   -> False
instance GCompare f => Ord (Key f) where
    compare (Key a) (Key b) = case gcompare a b of
        GLT -> LT
        GEQ -> EQ
        GGT -> GT

-- |Typed-environment maps: f is a GADT-like thing with a facility for 
-- rediscovering its type parameter, elements of which function as identifiers
-- tagged with the type of the thing they identify.  Real GADTs are one
-- useful instantiation of @f@, as are 'Tag's.
newtype Env f = Env (M.Map (Key f) (DSum f))

-- Internal: just a standard error message indicating an indexing error.
envKeyErr str = error ("Env." ++ str ++ ": key not present or type error")

empty :: Env f
empty = Env M.empty

singleton :: GCompare f => f a -> a -> Env f
singleton key thing = insert key thing empty

null :: Env f -> Bool
null (Env m) = M.null m

size :: Env f -> Int
size (Env m) = M.size m

fromList :: GCompare f => [DSum f] -> Env f
fromList [] = empty
fromList (DSum k v : rest) = insert k v (fromList rest)

member :: GCompare f => f a -> Env f -> Bool
member k (Env m) = M.member (Key k) m

insert :: GCompare f => f a -> a -> Env f -> Env f
insert k thing (Env m) = Env (M.insert (Key k) (DSum k thing) m)

-- insertWith :: GCompare f => (a -> a -> a) -> f a -> a -> Env f -> Env f
-- insertWith f k thing (Env m) = Env (M.insertWith f' (Key k) (DSum k thing) m)
--     where
-- --        e = envKeyErr "insertWith"
--         f' :: GCompare f => DSum f -> DSum f -> DSum f
--         f' (DSum k1 x) (DSum k2 y) = case gcompare k1 k2 of
--             GEQ -> case gcompare k k1 of
--                 GEQ -> DSum k (f x y)

lookup :: GCompare f => f a -> Env f -> Maybe a
lookup k (Env m) = do
    DSum k1 v <- M.lookup (Key k) m
    GEQ <- geq k k1
    return v

delete :: GCompare f => f a -> Env f -> Env f
delete k (Env m) = Env (M.delete (Key k) m)

(!) :: GCompare f => Env f -> f a -> a
Env m ! k = case m M.! Key k of
    DSum k1 v -> case gcompare k k1 of
        GEQ -> v
        _ -> envKeyErr "!"

(\\) :: GCompare f => Env f -> Env f -> Env f
Env m1 \\ Env m2 = Env (m1 M.\\ m2)

intersection :: GCompare f => Env f -> Env f -> Env f
intersection (Env m1) (Env m2) = Env (M.intersection m1 m2)

difference :: GCompare f => Env f -> Env f -> Env f
difference (Env m1) (Env m2) = Env (M.difference m1 m2)

union :: GCompare f => Env f -> Env f -> Env f
union (Env m1) (Env m2) = Env (M.union m1 m2)

unions :: GCompare f => [Env f] -> Env f
unions envs = Env $ M.unions [ m | Env m <- envs]

-- update :: GCompare f => (a -> Maybe a) -> f a -> Env f -> Env f
-- update f k (Env m) = Env (M.update f' (Key k) m)
--     where
--         f' = fmap toDyn . f . flip fromDyn (envKeyErr "update")
