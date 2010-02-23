{-# LANGUAGE 
    EmptyDataDecls, 
    TypeOperators
  #-}

-- |Experimentation with ideas from "Abstract Stone Duality"
-- (an abstract algebraic topological calculus for Real analysis)
module Math.ASD where

import Prelude hiding (Real, Ord(..), Eq(..))
import qualified Prelude as P

import Data.Tagged

-- Axiom 4.1: Base types
--   (types in this context are "locally compact topological spaces")
--   ∅, 1, 2, 3, ..., ℕ, ℤ, ℚ, ℝ, I = [0,1] ⊂ ℝ, and Σ
--   If X and Y are types then so are:
--      X × Y
--      X + Y
--      ℘X ≣ X → Σ

-- |The class of locally compact topological spaces
class Space s

-- |The empty set
data Zero
instance Space Zero
class Finite s where
    size :: Tagged s () -> Integer
instance Finite Zero where
    size _ = 0

-- |Finite numerals
data Succ a
instance Finite s => Finite (Succ s) where
    size s = 1 + size (dec s)
        where dec = retag :: Tagged (Succ s) () -> Tagged s ()
instance Finite s => Space (Succ s)

-- |ℕ: the Natural numbers
data ℕ
instance Space ℕ

-- |ℤ: the Integers
data ℤ
instance Space ℤ

-- |ℚ: the Rational numbers
data ℚ
instance Space ℚ

-- |ℝ: the Real numbers
data ℝ
instance Space ℝ

-- |I = [0,1] ⊂ ℝ
data I
instance Space I

-- |Topological Sierpinski space
newtype Σ = Σ Bool
instance Space Σ

-- |Continuous functions from any space to the sierpinski space
newtype P x = P (x -> Σ)
instance Space s => Space (P s)

-- Axiom 4.2: Primitive recursion over ℕ


-- Axiom 4.3: Relations on base types:
--   ℕ, ℤ and ℚ have (==), (≠), (>), (≥), (<), (≤)
--   ℝ and I only have (≠), (>), (<)

-- Axiom 4.4: These relations have type P(t,t)
--      There are also constants:
--        (⊤), (⊥) :: Σ
--      And operations:
--        (∧), (∨) :: P(Σ,Σ),
--        (∃) :: Overt   s => (P(Ps))
--        (∀) :: Compact k => (P(Pk))
--  (Overt and Compact will be defined later)

-- Definition 4.5: Statements are built using the operators:
--      (⇒), (⇔) :: (Σ, Σ) -> Stmt
--      (≡) :: Space s => (s, s) -> Stmt
--      (&) :: (Stmt, Stmt) -> Stmt
--
--  Propositions with free variables give rise to predicates by abstraction,
--  and every predicate defines an open and closed subspace
--      classify   :: Space s => P s -> (exists u. Open   u => u)
--      coclassify :: Space s => P s -> (exists c. Closed c => c)
--  (using pseudo-haskell where "exists t. Cxt => t" is equivalent
--   to "(forall t. Cxt => t -> a) -> a")
-- 
--  These are defined such that:
--    ɸx ⇔ ⊤    means   x ∈ classify   ɸ
--    ɸx ⇔ ⊥    means   x ∈ coclassify ɸ


-- Definition 4.7: a type is Discrete iff these 2 statements are interchangeable:
--      n ≡ m 
--      (n == m) ⇔ ⊤
-- Definition 4.8: a type is Hausdorff iff these 2 statements are interchangeable:
--      h ≡ k
--      (h ≠ k) ⇔ ⊥
--
-- These 2 definitions state that:
--  * If a type s is Discrete then its image under the diagonal map (s -> (s,s))
--    is open (its classifier is then (==))
--  * If a type is Hausdorff then its image under the diagonal map (s -> (s,s))
--    is closed (its coclassifier is then (≠))
-- (not clear from text: are the converses true?  Or perhaps the converses can
-- only be meaningfully stated in cases where they are essentially true by definition?)

-- Axiom 4.9: ℚ and ℝ are totally-ordered Hausdorff fields
--      x ≠ y   ≣   (x < y) ∨ (x > y)
--      recip x is defined iff (x ≠ 0)
-- And (<) is transitive and located:
--      (x < y) ∧ (y < z)   ≣   (x < z)