{-
 -      ``RomanNumerals''
 -      (c) 2008 James Cook
 -}

module RomanNumerals where

import Text.ParserCombinators.Parsec

groupRnDigits :: [a] -> [(a,a,a)]
groupRnDigits (a:b:rest@(c:_)) = (a,b,c) : groupRnDigits rest
groupRnDigits _ = []

fmtRomanNumeral 0 = error "Zero is not a number"
fmtRomanNumeral n = fmtWith (groupRnDigits "IVXLCDM\x2181\x2182") n
        where
                fmtWith _ 0             = ""
                fmtWith [] _            = error "out of digits"
                fmtWith (digits:rest) n
                        | n < 0         = error "Negative numbers don't exist"
                        | otherwise     = case n `quotRem` 10 of
                                (q, 0)  -> fmtWith rest q
                                (q, r)  -> fmtWith rest q ++ fmtDigit digits r
                
                -- n must be in range 1..9
                fmtDigit (one, five, ten) 9         = one  : ten  : ""
                fmtDigit (one, five, ten) (n+5)     = five : replicate n one
                fmtDigit (one, five, ten) 4         = one  : five : ""
                fmtDigit (one, five, ten) n         = replicate n one

parseDigit c v = do
        char c
        return v
        
parseRomanNumeral = parseWith (groupRnDigits (map char "IVXLCDMↁↂ"))
        where
                parseWith (digits:rest) = undefined