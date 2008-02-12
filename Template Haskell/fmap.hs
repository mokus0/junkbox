{-# OPTIONS -fth #-}
{-
 -	"fmap.hs"
 -	(c) 2008 James Cook
 -}

module Fmap where

import Prelude hiding (fmap)
import Language.Haskell.TH

import qualified Data.Map as M

import Control.Monad hiding (fmap)

typeVars :: Type -> [Name]
typeVars (ForallT vars _ t)     = vars
typeVars _                      = []

typeName :: Type -> Name
typeName (ForallT _ _ t)        = typeName t
typeName (VarT name)            = name
typeName (ConT name)            = name
typeName (TupleT n)             = tupleTypeName n
typeName ListT                  = ''[]
typeName (AppT t1 t2)           = typeName t1
typeName ArrowT                 = ''(->)

typeConsAndVars :: Name -> Q ([Con], [Name])
typeConsAndVars ty = do
        TyConI (typeDec) <- reify ty
        return (typeDecDataConsAndVars typeDec)

typeDecDataConsAndVars :: Dec -> ([Con], [Name])
typeDecDataConsAndVars (DataD _ _ tVars cons _)         = (cons, tVars)
typeDecDataConsAndVars (NewtypeD _ _ tVars con _)       = ([con], tVars)
typeDecDataConsAndVars (TySynD _ _ ty)                  = error "typeDecDataCons doesn't support type synonyms"

conName :: Con -> Name
conName (NormalC name _)        = name
conName (RecC    name _)        = name
conName (InfixC _ name _)       = name
conName (ForallC _ _ con)       = conName con

arity :: (Num a) => Type -> a
arity (ForallT _ _ t)           = arity t
arity (AppT (AppT ArrowT _) t)  = 1 + arity t
arity _                         = 0

conArity :: (Num a) => Name -> Q a
conArity con = do
        DataConI _ conType _ _ <- reify con
        return (arity conType)

conArgTypes :: Con -> [Type]
conArgTypes (NormalC _ args)    = map snd args
conArgTypes (RecC    _ args)    = map (\(_,_,ty) -> ty) args
conArgTypes (InfixC t1 _ t2)    = [snd t1, snd t2]
conArgTypes (ForallC _ _ con)   = conArgTypes con

fmapClause :: M.Map Name (ExpQ -> ExpQ) -> [Name] -> Con -> ClauseQ
fmapClause env fNames con = do
        let cName = conName con
        
        let funArgsPs = map varP fNames
        
        cArity <- conArity cName
        conArgs <- replicateM cArity (newName "x")
        let conArgsEs = map varE conArgs
        let conArgsPs = map varP conArgs
        let conArgP = conP cName conArgsPs
        
        let cArgTypes = conArgTypes con
        let process argType = M.findWithDefault id (typeName argType) env
        let conArgsProcessedEs = zipWith process cArgTypes conArgsEs
        
        let pats = funArgsPs ++ [conArgP]
        let body = normalB (appsE (conE cName : conArgsProcessedEs))
        
        clause pats body []

fmapDec :: Name -> M.Map Name (ExpQ -> ExpQ) -> [Name] -> [Con] -> DecQ
fmapDec fName env fNames cons = do
        let clauses = map (fmapClause env fNames) cons
        funD fName clauses

fmap :: Name -> ExpQ
fmap ty = do
        fName <- newName "fmap"
        
        (cons, tVars) <- typeConsAndVars ty
        
        let nTypeVars = length tVars
        fNames <- replicateM nTypeVars (newName "f")
        let process fName = appE (varE fName)
        
        let fE = varE fName
        let self = appsE (fE : map varE fNames)
        
        let env = M.fromList ((ty, appE self) : zip tVars (map process fNames))
        
        let fn = fmapDec fName env fNames cons
        letE [fn] fE