------------------------------------------------------------------------
-- A bunch of properties
------------------------------------------------------------------------

module Data.Bool.Properties where

open import Data.Bool as Bool
open import Data.Fin
open import Function
open import Function.Equality using (_⟨$⟩_)
open import Function.Equivalence
  using (_⇔_; equivalent; module Equivalent)
open import Algebra
open import Algebra.Structures
import Algebra.RingSolver.Simple as Solver
import Algebra.RingSolver.AlmostCommutativeRing as ACR
open import Relation.Binary.PropositionalEquality
  hiding (proof-irrelevance)
open ≡-Reasoning
import Algebra.FunctionProperties as P; open P _≡_
open import Data.Product
open import Data.Sum
open import Data.Empty

------------------------------------------------------------------------
-- Duality

-- Can we take advantage of duality in some (nice) way?

------------------------------------------------------------------------
-- (Bool, ∨, ∧, false, true) forms a commutative semiring

private

  ∨-assoc : Associative _∨_
  ∨-assoc true  y z = refl
  ∨-assoc false y z = refl

  ∧-assoc : Associative _∧_
  ∧-assoc true  y z = refl
  ∧-assoc false y z = refl

  ∨-comm : Commutative _∨_
  ∨-comm true  true  = refl
  ∨-comm true  false = refl
  ∨-comm false true  = refl
  ∨-comm false false = refl

  ∧-comm : Commutative _∧_
  ∧-comm true  true  = refl
  ∧-comm true  false = refl
  ∧-comm false true  = refl
  ∧-comm false false = refl

  distrib-∧-∨ : _∧_ DistributesOver _∨_
  distrib-∧-∨ = distˡ , distʳ
    where
    distˡ : _∧_ DistributesOverˡ _∨_
    distˡ true  y z = refl
    distˡ false y z = refl

    distʳ : _∧_ DistributesOverʳ _∨_
    distʳ x y z =
                      begin
       (y ∨ z) ∧ x
                      ≡⟨ ∧-comm (y ∨ z) x ⟩
       x ∧ (y ∨ z)
                      ≡⟨ distˡ x y z ⟩
       x ∧ y ∨ x ∧ z
                      ≡⟨ cong₂ _∨_ (∧-comm x y) (∧-comm x z) ⟩
       y ∧ x ∨ z ∧ x
                      ∎

isCommutativeSemiring-∨-∧
  : IsCommutativeSemiring _≡_ _∨_ _∧_ false true
isCommutativeSemiring-∨-∧ = record
  { +-isCommutativeMonoid = record
    { isSemigroup = record
      { isEquivalence = isEquivalence
      ; assoc         = ∨-assoc
      ; ∙-cong        = cong₂ _∨_
      }
    ; identityˡ = λ _ → refl
    ; comm      = ∨-comm
    }
  ; *-isCommutativeMonoid = record
    { isSemigroup = record
      { isEquivalence = isEquivalence
      ; assoc         = ∧-assoc
      ; ∙-cong        = cong₂ _∧_
      }
      ; identityˡ = λ _ → refl
    ; comm      = ∧-comm
    }
  ; distribʳ = proj₂ distrib-∧-∨
  ; zeroˡ    = λ _ → refl
  }

commutativeSemiring-∨-∧ : CommutativeSemiring _ _
commutativeSemiring-∨-∧ = record
  { _+_                   = _∨_
  ; _*_                   = _∧_
  ; 0#                    = false
  ; 1#                    = true
  ; isCommutativeSemiring = isCommutativeSemiring-∨-∧
  }

module RingSolver =
  Solver (ACR.fromCommutativeSemiring commutativeSemiring-∨-∧)

------------------------------------------------------------------------
-- (Bool, ∧, ∨, true, false) forms a commutative semiring

private

  distrib-∨-∧ : _∨_ DistributesOver _∧_
  distrib-∨-∧ = distˡ , distʳ
    where
    distˡ : _∨_ DistributesOverˡ _∧_
    distˡ true  y z = refl
    distˡ false y z = refl

    distʳ : _∨_ DistributesOverʳ _∧_
    distʳ x y z =
                          begin
       (y ∧ z) ∨ x
                          ≡⟨ ∨-comm (y ∧ z) x ⟩
       x ∨ (y ∧ z)
                          ≡⟨ distˡ x y z ⟩
       (x ∨ y) ∧ (x ∨ z)
                          ≡⟨ cong₂ _∧_ (∨-comm x y) (∨-comm x z) ⟩
       (y ∨ x) ∧ (z ∨ x)
                          ∎

isCommutativeSemiring-∧-∨
  : IsCommutativeSemiring _≡_ _∧_ _∨_ true false
isCommutativeSemiring-∧-∨ = record
  { +-isCommutativeMonoid = record
    { isSemigroup = record
      { isEquivalence = isEquivalence
      ; assoc         = ∧-assoc
      ; ∙-cong        = cong₂ _∧_
      }
    ; identityˡ = λ _ → refl
    ; comm      = ∧-comm
    }
  ; *-isCommutativeMonoid = record
    { isSemigroup = record
      { isEquivalence = isEquivalence
      ; assoc         = ∨-assoc
      ; ∙-cong        = cong₂ _∨_
      }
    ; identityˡ = λ _ → refl
    ; comm      = ∨-comm
    }
  ; distribʳ = proj₂ distrib-∨-∧
  ; zeroˡ    = λ _ → refl
  }

commutativeSemiring-∧-∨ : CommutativeSemiring _ _
commutativeSemiring-∧-∨ = record
  { _+_                   = _∧_
  ; _*_                   = _∨_
  ; 0#                    = true
  ; 1#                    = false
  ; isCommutativeSemiring = isCommutativeSemiring-∧-∨
  }

------------------------------------------------------------------------
-- (Bool, ∨, ∧, not, true, false) is a boolean algebra

private

  absorptive : Absorptive _∨_ _∧_
  absorptive = abs-∨-∧ , abs-∧-∨
    where
    abs-∨-∧ : _∨_ Absorbs _∧_
    abs-∨-∧ true  y = refl
    abs-∨-∧ false y = refl

    abs-∧-∨ : _∧_ Absorbs _∨_
    abs-∧-∨ true  y = refl
    abs-∧-∨ false y = refl

  not-∧-inverse : Inverse false not _∧_
  not-∧-inverse =
    ¬x∧x≡⊥ , (λ x → ∧-comm x (not x) ⟨ trans ⟩ ¬x∧x≡⊥ x)
    where
    ¬x∧x≡⊥ : LeftInverse false not _∧_
    ¬x∧x≡⊥ false = refl
    ¬x∧x≡⊥ true  = refl

  not-∨-inverse : Inverse true not _∨_
  not-∨-inverse =
    ¬x∨x≡⊤ , (λ x → ∨-comm x (not x) ⟨ trans ⟩ ¬x∨x≡⊤ x)
    where
    ¬x∨x≡⊤ : LeftInverse true not _∨_
    ¬x∨x≡⊤ false = refl
    ¬x∨x≡⊤ true  = refl

isBooleanAlgebra : IsBooleanAlgebra _≡_ _∨_ _∧_ not true false
isBooleanAlgebra = record
  { isDistributiveLattice = record
      { isLattice = record
          { isEquivalence = isEquivalence
          ; ∨-comm        = ∨-comm
          ; ∨-assoc       = ∨-assoc
          ; ∨-cong        = cong₂ _∨_
          ; ∧-comm        = ∧-comm
          ; ∧-assoc       = ∧-assoc
          ; ∧-cong        = cong₂ _∧_
          ; absorptive    = absorptive
          }
      ; ∨-∧-distribʳ = proj₂ distrib-∨-∧
      }
  ; ∨-complementʳ = proj₂ not-∨-inverse
  ; ∧-complementʳ = proj₂ not-∧-inverse
  ; ¬-cong        = cong not
  }

booleanAlgebra : BooleanAlgebra _ _
booleanAlgebra = record
  { _∨_              = _∨_
  ; _∧_              = _∧_
  ; ¬_               = not
  ; ⊤                = true
  ; ⊥                = false
  ; isBooleanAlgebra = isBooleanAlgebra
  }

------------------------------------------------------------------------
-- (Bool, xor, ∧, id, false, true) forms a commutative ring

private

  xor-is-ok : ∀ x y → x xor y ≡ (x ∨ y) ∧ not (x ∧ y)
  xor-is-ok true  y = refl
  xor-is-ok false y = sym $ proj₂ CS.*-identity _
    where module CS = CommutativeSemiring commutativeSemiring-∨-∧

commutativeRing-xor-∧ : CommutativeRing _ _
commutativeRing-xor-∧ = commutativeRing
  where
  import Algebra.Props.BooleanAlgebra as BA
  open BA booleanAlgebra
  open XorRing _xor_ xor-is-ok

module XorRingSolver =
  Solver (ACR.fromCommutativeRing commutativeRing-xor-∧)

------------------------------------------------------------------------
-- Miscellaneous other properties

not-involutive : Involutive not
not-involutive true  = refl
not-involutive false = refl

not-¬ : ∀ {x y} → x ≡ y → x ≢ not y
not-¬ {true}  refl ()
not-¬ {false} refl ()

¬-not : ∀ {x y} → x ≢ y → x ≡ not y
¬-not {true}  {true}  x≢y = ⊥-elim (x≢y refl)
¬-not {true}  {false} _   = refl
¬-not {false} {true}  _   = refl
¬-not {false} {false} x≢y = ⊥-elim (x≢y refl)

⇔→≡ : {b₁ b₂ b : Bool} → b₁ ≡ b ⇔ b₂ ≡ b → b₁ ≡ b₂
⇔→≡ {true } {true }         hyp = refl
⇔→≡ {true } {false} {true } hyp = sym (Equivalent.to hyp ⟨$⟩ refl)
⇔→≡ {true } {false} {false} hyp = Equivalent.from hyp ⟨$⟩ refl
⇔→≡ {false} {true } {true } hyp = Equivalent.from hyp ⟨$⟩ refl
⇔→≡ {false} {true } {false} hyp = sym (Equivalent.to hyp ⟨$⟩ refl)
⇔→≡ {false} {false}         hyp = refl

T-≡ : ∀ {b} → T b ⇔ b ≡ true
T-≡ {false} = equivalent (λ ())       (λ ())
T-≡ {true}  = equivalent (const refl) (const _)

T-∧ : ∀ {b₁ b₂} → T (b₁ ∧ b₂) ⇔ (T b₁ × T b₂)
T-∧ {true}  {true}  = equivalent (const (_ , _)) (const _)
T-∧ {true}  {false} = equivalent (λ ())          proj₂
T-∧ {false} {_}     = equivalent (λ ())          proj₁

T-∨ : ∀ {b₁ b₂} → T (b₁ ∨ b₂) ⇔ (T b₁ ⊎ T b₂)
T-∨ {true}  {b₂}    = equivalent inj₁ (const _)
T-∨ {false} {true}  = equivalent inj₂ (const _)
T-∨ {false} {false} = equivalent inj₁ [ id , id ]

proof-irrelevance : ∀ {b} (p q : T b) → p ≡ q
proof-irrelevance {true}  _  _  = refl
proof-irrelevance {false} () ()
