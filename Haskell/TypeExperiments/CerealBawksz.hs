{-# LANGUAGE TemplateHaskell, MagicHash #-}
module TypeExperiments.CerealBawksz where

import Data.Ratio
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

$( fmap concat $ mapM deriveLift
    [ ''Exp
    , ''Type
    , ''Range
    , ''Stmt
    , ''Match
    , ''Dec
    , ''Pat
    , ''Lit
    , ''Kind
    , ''Pred
    , ''TyVarBndr
    , ''Body
    , ''FamFlavour
    , ''Pragma
    , ''Foreign
    , ''FunDep
    , ''Con
    , ''Clause
    , ''Guard
    , ''InlineSpec
    , ''Safety
    , ''Callconv
    , ''Strict
    ])

-- instance (Lift a, Integral a) => Lift (Ratio a) where
--     lift x = [| p % q |]
--         where p = numerator x; q = denominator x
    

-- All that to say this:

-- | Take a TH 'ExpQ' and transform it to include
-- its own source.  For example:
-- 
-- > foo :: Num a => a -> a
-- > (fooCode, foo) = $(code [| \x -> x^2 - 1 |])
code expr = do
    expr' <- expr
    [| (expr', $(expr)) |]