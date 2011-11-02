{-# LANGUAGE
        GADTs,
        RankNTypes, 
        StandaloneDeriving,
        GeneralizedNewtypeDeriving,
        DeriveDataTypeable,
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts
  #-}
module Susie.Env.Var
    ( Var, declare, declareAs, create
    , varName
    , Id, varToId, idToVar
    ) where

import Control.Monad.Primitive
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Show
import Data.Maybe
import Data.Unique.Tag
import Data.Typeable
import Unsafe.Coerce

-----------------------------
-- variables and identifiers

-- variables:
-- A variable is either a wrapped GADT constructor or a runtime-created Tag
data Var s a where
    -- | Dynamically-created variable.  The string is for human consumption; it
    -- is irrelevant to the identity of the variable.  Create using 'create'.
    TagVar :: String -> !(Tag s a) -> Var s a
    -- | Statically-declared variable.  The string is for human consumption; it
    -- is irrelevant to the identity of the variable.  Declare using 'declare' or 'declareAs'.
    ExtVar :: (Typeable1 t, GCompare t) => String -> !(t a) -> Var s a
    deriving Typeable

instance Eq (Var s a) where
    k1 == k2 = isJust (geq k1 k2)
instance Ord (Var s a) where
    compare k1 k2 = weakenOrdering (gcompare k1 k2)

instance Show (Var s a) where
    showsPrec = gshowsPrec

instance GEq (Var s) where
    geq (TagVar _ k1) (TagVar _ k2) = geq k1 k2
    geq (TagVar  _ _) (ExtVar _  _) = Nothing
    geq (ExtVar _  _) (TagVar _  _) = Nothing
    geq (ExtVar _ k1) (ExtVar _ k2) = do
        k2' <- cast1 k2
        geq k1 k2'
        where cast1 = fmap runIdentity . gcast1 . Identity

instance GCompare (Var s) where
    gcompare (TagVar _ k1) (TagVar _ k2) = gcompare k1 k2
    gcompare (TagVar _  _) (ExtVar _ _) = GLT
    gcompare (ExtVar _  _) (TagVar _  _) = GGT
    gcompare (ExtVar _ k1) (ExtVar _ k2) = case compare (show t1) (show t2) of
            LT -> GLT
            EQ | t1 == t2   -> gcompare k1 (unsafeCoerce k2)
               | otherwise  -> error "gcompare{Var}: typereps differ but have the same string representation"
            GT -> GGT
        where t1 = typeOf1 k1; t2 = typeOf1 k2

instance GShow (Var s) where
    gshowsPrec p k = showParen (p>10)
        ( showString "Var "
        . showsPrec 11 (varName k)
        )

-- and an identifier is just a var without its type index
data Id s where
    Id :: !(Var s a) -> Id s
    deriving Typeable

instance Eq (Id s) where
    Id k1 == Id k2 = case gcompare k1 k2 of
        GEQ -> k1 == k2; _ -> False

instance Ord (Id s) where
    compare (Id k1) (Id k2) = case gcompare k1 k2 of
        GEQ -> compare k1 k2; GLT -> LT; GGT -> GT

instance Show (Id s) where
    showsPrec p (Id k) = showParen (p>10)
        ( showString "Id "
        . showsPrec 11 (varName k)
        )

declare :: (Typeable1 t, GCompare t, GShow t) => t a -> Var s a
declare t = ExtVar (gshow t) t

declareAs :: (Typeable1 t, GCompare t) => String -> t a -> Var s a
declareAs = ExtVar

create :: PrimMonad m => String -> m (Var (PrimState m) a)
create name = do
    tag <- newTag
    return (TagVar name tag)

varToId key = Id key

idToVar :: Id s -> (forall a. Var s a -> b) -> b
idToVar (Id key) f = f key

varName (TagVar name _) = name
varName (ExtVar name _) = name
