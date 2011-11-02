{-# LANGUAGE TemplateHaskell #-}
module Susie.Env.Var.TH
    ( declareVars
    ) where

import Control.Monad
import Data.Char
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.Typeable
import Language.Haskell.TH
import Susie.Env.Var

insert x xs = x : filter (/= x) xs

tyVarBndrName (PlainTV name)    = name
tyVarBndrName (KindedTV name _) = name

-- loses any qualifications on the original name
lcFirst = mkName . onFirst toLower . nameBase
onFirst f [] = []
onFirst f (x:xs) = f x : xs

nameOfCon (NormalC  name _) = name
nameOfCon (RecC     name _) = name
nameOfCon (InfixC _ name _) = name
nameOfCon (ForallC _ _ con) = nameOfCon con

argCountOfCon (NormalC  _ args) = length args
argCountOfCon (RecC     _ args) = length args
argCountOfCon (InfixC _ _ _)    = 2
argCountOfCon (ForallC _ _ con) = argCountOfCon con

-- given a quoted GADT declaration, derive instances 
-- of Typeable, Show, GShow, GEq and GCompare, and declare
-- a Var for each constructor.
declareVars decsQ = do
    decs <- decsQ
    DataD dataCxt dataName dataTyVars dataCons dataDeriving <- case decs of 
        [dec@DataD{}] -> return dec
        _ -> fail "declareVars: expecting a single data type declaration"
    
    let dec = DataD dataCxt dataName dataTyVars dataCons (insert ''Typeable $ dataDeriving)
    
    geqInst      <- deriveGEq      dec
    gcompareInst <- deriveGCompare dec
    gshowInst    <- deriveGShow    dec
    let showInst = InstanceD dataCxt
            (AppT (ConT ''Show) (foldl AppT (ConT dataName) [ VarT (tyVarBndrName tyVar) | tyVar <- dataTyVars ]))
            [ ValD (VarP 'showsPrec) (NormalB (VarE 'gshowsPrec)) [] ]
        
    declarations <- sequence
            [ do
                argNames <- replicateM (argCountOfCon con) (newName "x")
                funD varName
                    [ clause
                        (map varP argNames)
                        (normalB [| declare $(appsE (conE conName : map varE argNames)) |])
                        []
                    ]
            | con <- dataCons
            , let conName = nameOfCon con
                  varName = lcFirst conName
            ]
    
    return (dec : showInst : geqInst ++ gcompareInst ++ gshowInst ++ declarations)
