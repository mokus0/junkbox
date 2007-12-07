#!runhaskell
{-
 -	"proddigits.hs"
 -	(c) 2007 James Cook
 -	
 -	[03:05am|mokus> 95657055187 digits in 1e10! (128.53 TB)
 -	[03:07am|mokus> 90 min cpu time to compute that
 -	[03:07am|mokus> (on my 1.66 GHz G4)
 -
 -	about 1.15657e8 digits in 1e12!
 - 	about 9.9e101 digits in 10e100!
 -}


module Main where

prodDigits base xs = (prodDigitsE xs) / (log base) 
prodDigitsE xs = foldl (\x y -> x + log y) 0 xs

main = putStrLn (show $ prodDigits 10 [1, 1e90 .. 1e100])