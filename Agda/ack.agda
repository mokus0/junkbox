module ack where

open import Data.Nat

-- I don't quite "get" why termination is such a big deal.
-- It's actually a pretty weak condition.
-- But is that really all that useful?
-- If a function terminates in a googolplex times the lifespan
-- of the universe, does it *really* terminate?
-- ("If a tree falls in the woods...")
--
-- So the situation is this:
-- 
--   Agda requires proof of termination in order to guarantee
--   that type-checking will complete.  BUT:
-- 
--   (1) it's EASY to prove "termination" for functions that have no
--       hope of completing in the lifetime of the universe, and
-- 
--   (b) it's HARD to prove termination for many useful functions which
--       "obviously" do terminate very quickly.
-- 
-- So it seems to me that totality is really not about termination.
-- Presumably the real benefit is in taming equational reasoning.
-- But it's fun to see just how _much_ it's not about termination ;)
-- 
-- The Ackermann function, for example, terminates for all inputs and
-- is accepted without fuss by Agda's termination checker.
-- 
-- The arguments don't actually have to be all that large to
-- reach unimaginably long run times either.  'ack 4 5' should take 
-- well over a googolplex times the age of the universe, even if
-- every computation step took just one planck time interval.
-- 
ack : ℕ → ℕ → ℕ
ack  0       n      = suc n
ack (suc m)  0      = ack m 1
ack (suc m) (suc n) = ack m (ack (suc m) n)

-- For that matter, one can also write a (provably terminating)
-- function to compute the value of Graham's number.
-- 
-- First, we define Knuth's up-arrow notation (which already has
-- mind-bogglingly huge runtimes for pretty small values of its inputs):

_↑[_]_ : ℕ → ℕ → ℕ → ℕ
a ↑[     0 ] b = a * b
a ↑[ suc n ] 0 = 1
a ↑[ suc n ] (suc b) = a ↑[ n ] (a ↑[ suc n ] b)

-- Then, we define a helper - 3 ↑[   n ] 3 iterated on 'n' starting at n = 4.
-- "g 0" is already incomprehensibly large.  On the scale of "g 0", the number of
-- distinquishable configurations of all the matter in the observable universe 
-- would be approximately zero.
g : ℕ → ℕ
g      0  = 3 ↑[   4 ] 3
g (suc n) = 3 ↑[ g n ] 3

-- And finally, Graham's number.
-- This passes the termination checker!
G = g 63

-- Note that this means that although totality forces you to give up Turing
-- completeness, you haven't actually lost anything.  Given two languages 
-- A and B such that A is Turing complete and B is a total version of A (i.e., 
-- the same language but with the requirement of provable totality), B has 
-- exactly the same "effective computational power" as A.
-- 
-- In fact, there is a fully-mechanical translation of the program source
-- which only requires time linear in the size of the program source.  So
-- B can even run *exactly the same programs* as A.  
-- 
-- The transformation is simple.  Just parameterize every computation in 
-- <your total language of choice> by a "number of steps allowed", and lift
-- every function to the 'Maybe' monad.  Then your program provably terminates
-- for every input, by construction.  Pass a very big number such as "G" to
-- the top level function and you'll be able to compute everything in B
-- that anyone else could compute in A.
-- 
-- For concreteness, consider the assignment:
--   A = agda --no-termination-check
--   B = agda
-- Or A = Haskell; B = Agda, with slight modifications to the argument as needed.
-- 
-- So it seems an interesting alternative to Turing completeness would be the
-- class of languages (into which Agda falls) where, for any Turing-complete
-- language "Prog", there exists an interpreter "eval : ℕ → Prog τ → Maybe τ"
-- such that for any program "p : Prog τ", if "p" terminates with "x : τ" in 
-- its native language then there exists "n : ℕ" such that "eval n p" 
-- evaluates to "Just x".  I think this would be the closest thing to Turing
-- completeness that could be achieved in any total language.
-- 
-- Is there already a name for this class of languages?  "Recursive complete", maybe?
-- 
