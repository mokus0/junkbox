{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, TypeOperators,
        ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module ConstraintKinds.Dynamic
    ( Dynamic
    , toDyn, fromDyn, fromDynamic'
    , dynTypeRep
    , module Data.Typeable
    ) where

import ConstraintKinds.Dict
import ConstraintKinds.Typeable
import ConstraintKinds.Pred 
import Data.Maybe
import Data.Tagged
import Data.Typeable

data Dynamic cxt where Dynamic :: (Typeable t, cxt t) => t -> Dynamic cxt

-- Whoa... made GHC's head explode a couple times before getting it to like
-- this, but can now say things like:
--
--  λ> toDyn (42 :: Int) :: Dynamic (Show :&: Read)
--  Dynamic 42
--  λ> toDyn (42 :: Int) :: Dynamic (Show)
--  Dynamic 42
--  λ> toDyn (42 :: Int) :: Dynamic (Read :&: Show)
--  Dynamic 42

instance (cxt :<: Show) => Show (Dynamic cxt) where
    showsPrec p d = case weakenDyn d :: Dynamic Show of
        Dynamic it -> showParen (p > 10)
            ( showString "toDyn "
            . showsPrec 11 it
            ) 

instance TypeableCxt1 cxt => Typeable (Dynamic cxt) where
    typeOf it = mkTyConApp con [untag (typeOfCxt1 :: Tagged (Dict (cxt ())) TypeRep)]
        where
            con = mkTyCon3 "junkbox" "ConstraintKinds.Dict" "Dynamic"

toDyn :: (Typeable a, cxt a) => a -> Dynamic cxt
toDyn = Dynamic

fromDyn :: (Typeable a, cxt a) => Dynamic cxt -> a -> a
fromDyn (Dynamic it) def = fromMaybe def (cast it)

fromDynamic :: forall a cxt. (Typeable a) => Dynamic cxt -> Maybe a
fromDynamic (Dynamic it) = cast it

fromDynamic' :: forall a cxt. (Typeable a) => Dynamic cxt -> Maybe (Dict (cxt a), a)
fromDynamic' (Dynamic (it :: t)) = 
    case eqDict :: Maybe (Dict (t ~ a)) of
        Just Dict   -> Just (Dict, it)
        Nothing     -> Nothing

dynTypeRep :: Dynamic cxt -> TypeRep
dynTypeRep (Dynamic it) = typeOf it

weakenDyn :: forall c d. (c :<: d) => Dynamic c -> Dynamic d
weakenDyn (Dynamic (it :: t)) = case weaken (Dict :: Dict (c t)) of
    (Dict :: Dict (d t)) -> Dynamic it

weakenDyn' :: Dict (c :<: d) -> Dynamic c -> Dynamic d
weakenDyn' Dict = weakenDyn