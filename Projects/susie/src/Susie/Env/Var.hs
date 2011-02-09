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
    ( Var, declare, create
    , varName
    , Id, varToId, idToVar
    ) where

import Control.Monad.Primitive
import qualified Data.Dependent.Map as M
import Data.GADT.Compare
import Data.GADT.Tag
import Data.Typeable
import Unsafe.Coerce

-----------------------------
-- variables and identifiers

-- variables:
-- A variable is either a wrapped GADT constructor or a runtime-created Tag
data Var s a where
    TagVar :: !(Tag s a) -> String             -> Var s a
    ExtVar :: (Typeable1 t, GCompare t) => t a -> Var s a

instance Eq (Var s a) where
    k1 == k2 = case gcompare k1 k2 of
        GEQ -> True; _ -> False
instance Ord (Var s a) where
    compare k1 k2 = case gcompare k1 k2 of
        GLT -> LT; GEQ -> EQ; GGT -> GT

instance Show (Var s a) where
    showsPrec p k = showParen (p>10)
        ( showString "Var "
        . showsPrec 11 (varName k)
        )

deriving instance Typeable2 Var

instance GCompare (Var s) where
    gcompare (TagVar k1 _) (TagVar k2 _) = gcompare k1 k2
    gcompare (TagVar  _ _) (ExtVar    _) = GLT
    gcompare (ExtVar    _) (TagVar  _ _) = GGT
    gcompare (ExtVar   k1) (ExtVar   k2) = case compare (show t1) (show t2) of
            LT -> GLT
            EQ | t1 == t2   -> gcompare k1 (unsafeCoerce k2)
               | otherwise  -> error "gcompare{Var}: typereps differ but have the same string representation"
            GT -> GGT
        where t1 = typeOf1 k1; t2 = typeOf1 k2


-- and an identifier is just a var without its type index
data Id s where
    Id :: Var s a -> Id s

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

deriving instance Typeable1 Id

declare :: (Typeable1 t, GCompare t) => t a -> Var s a
declare = ExtVar

create :: PrimMonad m => String -> m (Var (PrimState m) a)
create name = do
    tag <- newTag
    return (TagVar tag name)

varToId key = Id key

idToVar :: Id s -> (forall a. Var s a -> b) -> b
idToVar (Id key) f = f key

varName (TagVar   _ name) = name
varName (ExtVar      key) = show (typeOf1 key)