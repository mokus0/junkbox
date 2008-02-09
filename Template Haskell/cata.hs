{-# OPTIONS -fth #-}
{-
 -	"cata.hs"
 -	(c) 2008 James Cook
 -      
 -      First attempt at a generic catamorphism...
 -}

module Cata where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Monad

-- some fun: |$(cata ''Void)| panics ghc 6.6.1; check 6.8.x later
data Void

replaceAt :: Integral a => a -> b -> [b] -> [b]
replaceAt _     _ []     = []
replaceAt 0     y (_:xs) = y : xs
replaceAt (n+1) y (x:xs) = x : replaceAt n y xs

arity :: (Num a) => Type -> a
arity (ForallT _ _ t)           = arity t
arity (AppT (AppT ArrowT _) t)  = 1 + arity t
arity _                         = 0

conName :: Con -> Name
conName (NormalC name _)        = name
conName (RecC    name _)        = name
conName (InfixC _ name _)       = name
conName (ForallC _ _ con)       = conName con

conArity :: (Num a) => Name -> Q a
conArity con = do
        DataConI _ conType _ _ <- reify con
        return (arity conType)

conArgTypes :: Con -> [Type]
conArgTypes (NormalC _ args)    = map snd args
conArgTypes (RecC    _ args)    = map (\(_,_,ty) -> ty) args
conArgTypes (InfixC t1 _ t2)    = [snd t1, snd t2]
conArgTypes (ForallC _ _ con)   = conArgTypes con

typeDecDataCons :: Dec -> [Con]
typeDecDataCons (DataD _ _ _ cons _)    = cons
typeDecDataCons (NewtypeD _ _ _ con _)  = [con]
typeDecDataCons (TySynD _ _ ty)         = error "typeDecDataCons doesn't support type synonyms"

cataClause :: ExpQ -> Name -> [Name] -> Int -> Con -> Int -> ClauseQ
cataClause self ty funcNames nCons con conN = do
        let cName = conName con
        conArity <- conArity cName
        
        let funcName = funcNames !! conN
        let funcE = varE funcName
--        let funcP = varP funcName
        
        conArgs <- replicateM conArity (newName "x")
        let conArgsPs = map varP conArgs
        let conArgsEs = map varE conArgs
        let conArgP = conP cName conArgsPs
        
        let addRecursion argType argE = case argType of
                ConT x
                        | x == ty       -> appE self argE
                AppT (ConT x) _ 
                        | x == ty       -> appE self argE
                _                       -> argE
        let argTypes = conArgTypes con
        let conArgsEsWithRecursion = zipWith addRecursion argTypes conArgsEs
        
        let funArgsPs = map varP funcNames
        
        let pats = funArgsPs ++ [conArgP]
        let body = normalB (appsE (funcE : conArgsEsWithRecursion) )
        clause pats body []

cataDec :: Name -> ExpQ -> Name -> [Name] -> [Con] -> DecQ
cataDec fName self ty funcNames cons = funD fName clauses
        where
                nCons = length cons
                clauses = zipWith (cataClause self ty funcNames nCons) cons [0..]

cata :: Name -> ExpQ
cata ty = do
        TyConI (typeDec) <- reify ty
        let cons = typeDecDataCons typeDec
        fName <- newName "cata"
        let fE = varE fName
        
        let nCons = length cons
        funcNames <- replicateM nCons (newName "f")
        
        let self = appsE (fE : map varE funcNames)
        
        let fn = cataDec fName self ty funcNames cons
        letE [fn] fE
