module p2 where

open import Prelude
open import Data.Nat hiding (even; odd)
open import Data.Bool
open import Logic.Base
open import Logic.Identity
open import Data.Maybe
open import Data.Tuple

2x : Nat -> Nat
2x zero = zero
2x (suc n) = suc (suc (2x n))

2x-suc : {d : Nat} -> (suc (suc (2x d)) ≡ suc (suc (2x d)))
2x-suc = refl

data Even (n : Nat) : Set where
    even : {d : Nat} -> 2x d ≡ n -> Even n

data Odd (n : Nat) : Set where
    odd : {d : Nat} -> suc (2x d) ≡ n -> Odd n

div2-even : {n : Nat} -> Even n -> Nat
div2-even (even {d} refl) = d

div2-odd : {n : Nat} -> Odd n -> Nat
div2-odd (odd {d} refl) = d

parity : (n : Nat) -> Even n \/ Odd n
parity zero         = \/-IL (even {zero} refl)
parity (suc n) with parity n
parity (suc n) | \/-IR (odd  {d} q) = \/-IL (even {suc n}{suc d} (trans 2x-suc (cong suc q)))       -- grr... why won't 'refl' here introduce (suc (2x d) = n)?
parity (suc n) | \/-IL (even {d} q) = \/-IR (odd {suc n}{d} (cong suc q))

divMod2 : Nat -> Nat × Bool
divMod2 n with parity n
... | \/-IL e = < div2-even e , false >
... | \/-IR o = < div2-odd  o , true  >

div2 : Nat -> Nat
div2 n = fst (divMod2 n)

mod2 : Nat -> Bool
mod2 n = snd (divMod2 n)

data p2 : Nat -> Set where
    p2-one : p2 1
    p2-suc : {n : Nat} -> p2 n -> p2 (2 * n)

log2 : {n : Nat} -> p2 n -> Nat
log2 p2-one         = zero
log2 (p2-suc p)   = suc (log2 p)

nfunc : {A B : Set} -> (n : Nat) -> Set
nfunc {_}{B}  zero   = B
nfunc {A}{B} (suc n) = A -> nfunc {A}{B} n

nprod : {A : Set} -> (n : Nat) -> Set
nprod {_}  zero   = True
nprod {A} (suc n) = A × nprod {A} n

nconst : {A B : Set} -> (n : Nat) -> B -> nfunc{A}{B} n
nconst {A}{B} zero    b = b
nconst {A}{B} (suc n) b = const (nconst {A}{B} n b)

ncurry : {A B : Set}{n : Nat} -> (nprod{A} n -> B) -> nfunc {A}{B} n
ncurry {A}{B}{zero } f = f tt
ncurry {A}{B}{suc n} f = \a -> ncurry {A}{B}{n} (\p -> f (< a , p >))

nuncurry : {A B : Set} (n : Nat) -> nfunc {A}{B} n -> (nprod{A} n -> B)
nuncurry {A}{B}  zero   f = \tt -> f
nuncurry {A}{B} (suc n) f = \p -> nuncurry {A}{B} n (f (fst p)) (snd p)

infix 30 _=>_
infixr 20 _::_

data Assoc (K V : Set) : Set where
    _=>_ : K -> V -> Assoc K V
data Map (K V : Set){_>_ : K -> K -> Bool} : Set where
    []      : Map K V
    _::_ : Assoc K V -> Map K V -> Map K V

insert : {K V : Set}{_>_ : K -> K -> Bool} -> Assoc K V -> Map K V {_>_} -> Map K V {_>_}
insert kv [] = kv :: []
insert {K}{V}{_>_} (ik => iv) (k => v :: rest) with ik > k
... | true = []