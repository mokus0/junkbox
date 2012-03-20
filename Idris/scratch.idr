module scratch

Rel : Set -> Set
Rel A = A -> A -> Set

Reflexive : Rel A -> Set
Reflexive {A} R = ({x : A} -> R x x)

Symmetric : Rel A -> Set
Symmetric {A} R = ({x : A} -> {y : A} -> R x y -> R y x)

Transitive : Rel A -> Set
Transitive {A} R = ({x : A} -> {y : A} -> {z : A} -> R x y -> R y z -> R x z)

data IsEquivalence : {A : Set} -> (R : A -> A -> Set) -> Set where
    isEquivalence : 
            Reflexive {A} R
         -> Symmetric {A} R
         -> Transitive {A} R
         -> IsEquivalence {A} R

data Setoid : Set where
    setoid : (Carrier : Set) -> (Eq : Rel Carrier) -> IsEquivalence Eq -> Setoid

PropEq : A -> A -> Set
PropEq x y = (x = y)

-- propEqIsEquivalence : IsEquivalence PropEq
-- propEqIsEquivalence = isEquivalence refl sym trans
