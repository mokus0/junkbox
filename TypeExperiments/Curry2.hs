{-# LANGUAGE 
        MultiParamTypeClasses, FunctionalDependencies,
        FlexibleInstances, FlexibleContexts, UndecidableInstances, 
        EmptyDataDecls, TypeOperators, TemplateHaskell
  #-}
module TypeExperiments.Curry2 where

{- 
 - It's quite ugly, but I finally managed to make my "varargs" curry
 - and uncurry operations...
 - 
 - usage:
 -   curry d2 :: ((a, b) -> c) -> a -> b -> c
 -   curry d5 :: ((a, a1, a2, a3, b) -> c) -> a -> a1 -> a2 -> a3 -> b -> c
 - 
 - etc.
 -}

import Prelude hiding (curry, uncurry, succ, pred, (+), (-))
import qualified Prelude as P
import Data.TypeLevel.Num hiding (Succ, succ, pred)

import Language.Haskell.TH
import qualified GHC.Exts
import Control.Monad

-- The implementation is indexed by peano numbers because we need a recursive
-- construction and Data.TypeLevel's high-level decimal representation just
-- won't do:
data Zero
data Succ n
succ :: n -> Succ n
succ = undefined
pred :: Succ n -> n
pred = undefined

-- Functions for extending or dissecting tuples of arbitrary sizes:
class ConsTuple a t1 t2 | a t1 -> t2, t2 -> a t1 where
    consTuple :: a -> t1 -> t2
    unConsTuple :: t2 -> (a, t1)

$(sequence
    -- ConsTuple instances for all supported tuple types, from (,)->(,,) up
    [ do
        tyVars <- replicateM n (newName "t" >>= varT)
        names <- replicateM n (newName "x")
        
        let -- types and pieces of the instance decl
            tyVarsQ = map return tyVars
            
            hd  = head tyVarsQ
            tl  = foldl1 appT (tupleT (n P.- 1) : tail tyVarsQ)
            tup = foldl1 appT (tupleT  n    : tyVarsQ)
            
            cxt = return []
            cls = foldl1 appT [conT ''ConsTuple, hd, tl, tup]
        
            -- exprs and pieces of the method definitions
            wh = [cons, uncons]
                where
                    pats = map varP names
                    exps = map varE names
                    
                    patHd  = head pats
                    patTl  = tupP (tail pats)
                    patTup = tupP pats
                    
                    expHd  = head exps
                    expTl  = tupE (tail exps)
                    expTup = tupE exps
                    
                    cons = funD 'consTuple
                        [ clause [patHd, patTl] (normalB expTup) [] ]
                    uncons = funD 'unConsTuple
                        [ clause [patTup] (normalB (tupE [expHd, expTl])) [] ]
        
        instanceD cxt  cls wh

    | n <- [3 .. GHC.Exts.maxTupleSize]
    ])

-- The curry and uncurry operations, indexed by Peano numbers encoded in
-- types.  Zero is the smallest case, corresponding to currying
-- and uncurrying 2-arg functions.
class CurryType n f g | n -> f g where
    curryP   :: n -> f -> g
    uncurryP :: n -> g -> f

instance CurryType Zero ((a,b)->c) (a->b->c) where
    curryP   d2 = P.curry
    uncurryP d2 = P.uncurry

instance (CurryType n (t1->r) g, ConsTuple a t1 t2) 
    => CurryType (Succ n) (t2->r) (a->g) where
        curryP n f = \x -> curryP (pred n) (f . consTuple x)
        uncurryP n g = \tup -> case unConsTuple tup of
            (x, tup2) -> uncurryP (pred n) (g x) tup2


-- we want the user to be able to use Data.TypeLevel's nice friendly
-- numbers, especially the value-level constants such as 'd4', etc.
-- So, here's a type class for translating them into our Peano algebra:
--
-- (the b -> a fundep would be proper, but can't actually be used because of 
-- the form of the multi-digit induction instance)
class Peano a b | a -> b where
    toPeano :: a -> b
    toPeano _ = undefined
    fromPeano :: b -> a
    fromPeano _ = undefined

-- base cases for the decimal constructors:
instance Peano D0 Zero
instance Peano D1 (Succ Zero)
instance Peano D2 (Succ (Succ Zero))
instance Peano D3 (Succ (Succ (Succ Zero)))
instance Peano D4 (Succ (Succ (Succ (Succ Zero))))
instance Peano D5 (Succ (Succ (Succ (Succ (Succ Zero)))))
instance Peano D6 (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
instance Peano D7 (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
instance Peano D8 (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
instance Peano D9 (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

-- need to be able to add and multiply peano numbers in order to
-- convert the decimal representation correctly:
class AddP a b c | a b -> c where
  addP :: a -> b -> c
  addP _ _ = undefined
  
instance               AddP  Zero    b  b
instance AddP a b c => AddP (Succ a) b (Succ c)

class MulP a b c | a b -> c where
  mulP :: a -> b -> c
  mulP _ _ = undefined

instance                             MulP  Zero    b Zero
instance (MulP a b c, AddP b c d) => MulP (Succ a) b d
type P10 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

-- induction case for multiple digits:
instance (Peano n p, Peano m q, MulP p P10 p10, AddP p10 q r) => Peano (n :* m) r


-- finally, the "user-friendly" curry and uncurry (user-friendly as long as the user
-- doesn't see the types GHC infers and have a heart attack!):
curry
  :: (CurryType b f g,
      Peano z b,
      Sub x D2 z) =>
     x -> f -> g
curry n   = curryP (toPeano (n - d2))
uncurry
  :: (CurryType b f g,
      Peano z b,
      Sub x D2 z) =>
     x -> g -> f
uncurry n = uncurryP (toPeano (n - d2))
