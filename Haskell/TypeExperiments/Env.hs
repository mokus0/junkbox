{-# LANGUAGE GADTs #-}
module TypeExperiments.Env
    ( Env
    , KVPair(..)
    , empty, null, size
    , singleton
    , fromList, toList
    , member
    , insert
    , lookup, (!)
    , delete, (\\)
    , intersection
    , difference
    , union, unions
    ) where

import Prelude hiding (null, lookup)
import qualified Data.Map as M
import Data.Dynamic
import TypeExperiments.Uniq
import TypeExperiments.GCompare
import Control.Monad.Primitive

-- |Dependent-typed key-value pairs.  The key determines the type of the value.
-- For example, given a key type:
-- 
-- > data Foo v where
-- >     X :: Foo String
-- >     Y :: Foo Double
-- 
-- An @Env Foo@ can then be built by: @fromList [X :=> "hello", Y :=> pi]@
data KVPair k where
    (:=>) :: k v -> v -> KVPair k

-- |A type class you can implement for your GADT to make 'KVPair's showable.
-- 'showsPrecVal' would typically be implemented by matching against the key
-- to access the value type's Show instance and just returning showsPrec:
-- 
-- > instance ShowKey Foo where
-- >     showsPrecKey p X = showString "X"
-- >     showsPrecKey p Y = showString "Y"
-- >     showsPrecVal X = showsPrec
-- >     showsPrecVal Y = showsPrec
class ShowKey k where
    showsPrecKey :: Int -> k a -> ShowS
    showsPrecVal :: k a -> Int -> a -> ShowS

instance ShowKey k => Show (KVPair k) where
    showsPrec p (k :=> v) = showParen (p > 10)
        ( showsPrecKey 10 k
        . showString " :=> "
        . showsPrecVal k 10 v
        )

-- |Internal: a 'Key' is just a wrapper for the true key type @f@ which hides
-- the associated value type and presents the key's GADT-level 'GCompare' 
-- instance as a vanilla 'Ord' instance so it can be used as the key in a
-- 'M.Map'.
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
newtype Env f = Env (M.Map (Key f) (KVPair f))

-- |Internal: just a standard error message indicating a fundamental programming error.
envKeyErr str = error ("Env." ++ str ++ ": key not present or type error")

empty :: Env f
empty = Env M.empty

singleton :: GCompare f => f a -> a -> Env f
singleton key thing = insert key thing empty

null :: Env f -> Bool
null (Env m) = M.null m

size :: Env f -> Int
size (Env m) = M.size m

fromList :: GCompare f => [KVPair f] -> Env f
fromList [] = empty
fromList ((k :=> v) : rest) = insert k v (fromList rest)

toList :: Env f -> [KVPair f]
toList (Env m) = map snd (M.toList m)

member :: GCompare f => f a -> Env f -> Bool
member k (Env m) = M.member (Key k) m

insert :: GCompare f => f a -> a -> Env f -> Env f
insert k thing (Env m) = Env (M.insert (Key k) (k :=> thing) m)

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
    k1 :=> v <- M.lookup (Key k) m
    GEQ <- geq k k1
    return v

delete :: GCompare f => f a -> Env f -> Env f
delete k (Env m) = Env (M.delete (Key k) m)

(!) :: GCompare f => Env f -> f a -> a
Env m ! k = case m M.! Key k of
    k1 :=> v -> case gcompare k k1 of
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
