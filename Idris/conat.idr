module conat

data CoNat 
    = Co Nat
    | Infinity

total S : CoNat -> CoNat
S (Co n)   = Co (S n)
S Infinity = Infinity

total absurdCoNatDischarge : (n : Nat) -> Co n = Infinity -> _|_
total Co_inj   : (n : Nat) -> (m : Nat) -> Co n = Co m -> n = m

total S_inj : (n : CoNat) -> (m : CoNat) -> S n = S m -> n = m
S_inj (Co n)   (Co m)   p = ?sInj_Co_Co
S_inj (Co n)   Infinity p = FalseElim (absurdCoNatDischarge (S n) p)
S_inj Infinity (Co m)   p = FalseElim (absurdCoNatDischarge (S m) (sym p))
S_inj Infinity Infinity _    = refl

absurdCoNatDischarge n p = replace {P = disjoint} p ()
  where
    total disjoint : CoNat -> Set
    disjoint (Co n)   = ()
    disjoint Infinity = _|_

Co_inj n m refl = refl

sInj_Co_Co = proof {
    intros;
    rewrite succInjective n m (Co_inj n m p);
    trivial;
}

-- this version is accepted
