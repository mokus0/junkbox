-- Port of type equality witness type implementation from
-- http://okmij.org/ftp/ML/GADT.ml
-- 
-- Uses a simple but clever trick with mutable references.  Upon construction,
-- fills both fields with the same reference, at the same type (because only "Refl a a" can be constructed legally).  Upon use, it is not known (by the type system) that the types are the same, but the reference gives a tunnel through which the value can be pushed to safely coerce it from one type to the "other" (which of course is actually the same one even though the type system doesn't know it).
module TypeExperiments.Refl where

import Control.Concurrent.MVar

-- (hidden implementation)
data Refl a b = Refl !(MVar a) !(MVar b) 

refl :: IO  (Refl a a)
refl = do
    mv <- newEmptyMVar
    return (Refl mv mv)

symm :: Refl a b -> Refl b a
symm (Refl x y) = Refl y x

apply_eq :: Refl a b -> a -> IO b
apply_eq (Refl mx my) x = do
    putMVar mx x
    takeMVar my