{-# LANGUAGE FlexibleInstances #-}
module Math.NAnd where

import Prelude(Read(..), Show(..), undefined, error)
import Text.PrettyPrint.HughesPJClass
import Data.Unamb

data B = T | F
    deriving (Read, Show)

instance Pretty B where
    pPrint T = char 'T'
    pPrint F = char 'F'

instance Pretty a => Pretty (B -> a) where
    pPrint f = vcat
        [ hsep [pPrint T, text "->", pPrint (f T)]
        , hsep [pPrint F, text "->", pPrint (f F)]
        ]

id, not, const_T, const_F :: B -> B
id  x = x
not x = nand x x

const_T x = nand x notX
    where
        notX = nand x x

const_F x = nand t t
    where
        t    = nand x notX
        notX = nand x x

and, or, nand, nor, xor, xnor :: B -> B -> B

and x y = nand z z
    where z = nand x y

or x y = nand notX notY
    where
        notX = nand x x
        notY = nand y y

nand x y = unamb (nand' x y) (nand' y x)
    where
        nand' T T = F
        nand' _ _ = T

nor x y = nand xOrY xOrY
    where
        xOrY = nand notX notY
        notX = nand x x
        notY = nand y y
        
xor x y = nor (nor x y) (and x y)

xnor x y = or (nor x y) (and x y)
