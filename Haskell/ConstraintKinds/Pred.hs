{-# LANGUAGE ConstraintKinds, TypeOperators, MultiParamTypeClasses, UndecidableInstances,
        FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
module ConstraintKinds.Pred where

import ConstraintKinds.Dict
import ConstraintKinds.Typeable
import Data.Tagged
import Data.Typeable

-- kind Pred = * -> Constraint
type Dict1 p t = Dict (p t)

-- logical implication...
class c1 :<: c2 where
    weaken :: Dict (c1 t) -> Dict (c2 t)

instance c :<: c where weaken = id

weaken' :: Dict (c1 :<: c2) -> Dict (c1 t) -> Dict (c2 t)
weaken' Dict = weaken

class    True t
instance True t

-- Conflicts with c :<: c ... not sure what to do about that.
instance c :<: True where weaken Dict = Dict

-- not sure this actually makes any sense as a general concept
-- of "False", but it is a natural choice for bottom element of 
-- the :<: order.
class False t where
    pandora :: forall c. Dict (c t)

-- Also conflicts with c :<: c and with c :<: True.
instance False :<: c where weaken Dict = pandora

class    (p :<: False) => Not p
instance (p :<: False) => Not p

class    (c1 t, c2 t) => (:&:) c1 c2 t
instance (c1 t, c2 t) => (:&:) c1 c2 t

instance (c :&: d) :<: c where weaken Dict = Dict
instance (c :&: d) :<: d where weaken Dict = Dict

-- Things are not as quite as nice as they "ought" to be, though...
-- instance (c :<: e) => (c :&: d) :<: e
-- instance (c :<: d) => (c :&: d) :<: e

-- the following work, though (so at least "small" cases can be covered, but
-- with an exponential explosion of instances):
instance ((c :&: d) :&: e) :<: c where weaken Dict = Dict

instance ((c :&: d) :&: e) :<: d where weaken Dict = Dict
instance (c :&: (d :&: e)) :<: d where weaken Dict = Dict

instance (c :&: (d :&: e)) :<: e where weaken Dict = Dict


instance (((c :&: d) :&: e) :&: f) :<: c where weaken Dict = Dict

instance (((c :&: d) :&: e) :&: f) :<: d where weaken Dict = Dict
instance ((c :&: (d :&: e)) :&: f) :<: d where weaken Dict = Dict
instance (c :&: ((d :&: e) :&: f)) :<: d where weaken Dict = Dict

instance ((c :&: (d :&: e)) :&: f) :<: e where weaken Dict = Dict
instance (c :&: ((d :&: e) :&: f)) :<: e where weaken Dict = Dict
instance (c :&: (d :&: (e :&: f))) :<: e where weaken Dict = Dict

instance (c :&: (d :&: (e :&: f))) :<: f where weaken Dict = Dict


strengthen :: Dict1 c t -> Dict1 d t -> Dict1 (c :&: d) t
strengthen Dict Dict = Dict

instance (TypeableCxt1 c1, TypeableCxt1 c2) => TypeableCxt (c1 :<: c2) where
    typeOfCxt = Tagged (mkTyConApp con
        [ untag (typeOfCxt1 :: Tagged (Dict (c1 ())) TypeRep)
        , untag (typeOfCxt1 :: Tagged (Dict (c2 ())) TypeRep)
        ])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Pred" "(:<:)"

instance TypeableCxt1 True where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Pred" "True"

instance TypeableCxt1 False where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Pred" "False"

instance TypeableCxt1 p => TypeableCxt (Not p) where
    typeOfCxt = Tagged (mkTyConApp con [untag (typeOfCxt1 :: Tagged (Dict (p ())) TypeRep)])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Pred" "Not"

instance (TypeableCxt1 c1, TypeableCxt1 c2) => TypeableCxt1 (c1 :&: c2) where
    typeOfCxt1 = Tagged (mkTyConApp con
        [ untag (typeOfCxt1 :: Tagged (Dict (c1 ())) TypeRep)
        , untag (typeOfCxt1 :: Tagged (Dict (c2 ())) TypeRep)
        ])
        where con = mkTyCon3 "junkbox" "ConstraintKinds.Pred" "(:&:)"

