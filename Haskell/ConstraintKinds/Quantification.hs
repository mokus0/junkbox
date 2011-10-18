{-# LANGUAGE ConstraintKinds, TypeFamilies, UndecidableInstances, 
        ScopedTypeVariables, TypeOperators #-}

module ConstraintKinds.Quantification where

import ConstraintKinds.Dict
import ConstraintKinds.Typeable
import ConstraintKinds.Pred
import Data.Tagged
import Data.Typeable

class p (Witness p) => Exists p where
    type Witness p

class Forall p where
    witness :: Dict (p a)

instance TypeableCxt1 p => TypeableCxt (Exists p) where
    typeOfCxt = Tagged (mkTyConApp con [untag (typeOfCxt1 :: Tagged (Dict (p ())) TypeRep)])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Quantification" "Exists"

instance TypeableCxt1 p => TypeableCxt (Forall p) where
    typeOfCxt = Tagged (mkTyConApp con [untag (typeOfCxt1 :: Tagged (Dict (p ())) TypeRep)])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Quantification" "Forall"

instance Forall True where witness = Dict
instance (Forall p, Forall q) => Forall (p :&: q) where
    witness = strengthen (witness :: Dict (p a)) (witness :: Dict (q a))

instance Exists True where type Witness True = ()
instance Exists ((~) a) where type Witness ((~) a) = a
instance Exists Show where type Witness Show = ()
instance Exists Read where type Witness Read = ()
instance Exists Eq   where type Witness Eq   = ()

-- and so on...

-- sadly, this is not possible, for obvious reasons:
-- instance Exists p => Exists (p :&: q) where type Witness (p :&: q) = Witness p
-- instance Exists q => Exists (p :&: q) where type Witness (p :&: q) = Witness q

