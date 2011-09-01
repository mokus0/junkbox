module ack where

open import Data.Nat

-- Ackermann function
ack : ℕ → ℕ → ℕ
ack  0       n      = suc n
ack (suc m)  0      = ack m 1
ack (suc m) (suc n) = ack m (ack (suc m) n)

-- Knuth's up-arrow notation
_↑[_]_ : ℕ → ℕ → ℕ → ℕ
a ↑[     0 ] b = a * b
a ↑[ suc n ] 0 = 1
a ↑[ suc n ] (suc b) = a ↑[ n ] (a ↑[ suc n ] b)

-- A helper - 3 ↑[   n ] 3 iterated on 'n' starting at n = 4.
-- "g 0" is already incomprehensibly large.  On the scale of "g 1", I believe
-- the number of distinquishable configurations of all the matter in the
-- observable universe would be approximately zero.
g : ℕ → ℕ
g      0  = 4
g (suc n) = 3 ↑[ g n ] 3

-- And finally, Graham's number.
G = g 64

