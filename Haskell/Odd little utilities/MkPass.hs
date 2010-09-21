#!/usr/bin/env runhaskell
{-# LANGUAGE ParallelListComp #-}
module Main where

import Data.Random
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.DevRandom

import Control.Monad
import Data.Maybe
import qualified Data.Bimap as B
import qualified Data.Set as S

shifts = B.fromList $ zip unshifted shifted

unshifted = "`1234567890[],.pyfgcrl/=\\aoeuidhtns-;qjkxbmwvz'"
shifted   = "~!@#$%^&*(){}<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWVZ\""

dvQw = B.fromList (zip dv qw ++ zip ds qs)

dv="[]',.pyfgcrl/=\\aoeuidhtns-;qjkxbmwvz1234567890"
qw="-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./1234567890"
ds="{}\"<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWVZ!@#$%^&*()"
qs="_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?!@#$%^&*()"

dvLeft  = S.fromList "`123456',.pyfaoeuid;qjkx"
dvRight = S.fromList "7890[]gcrl/=\\htns-bmwvz"

qwLeft  = S.map dvToQw dvLeft
qwRight = S.map dvToQw dvRight

ll = S.intersection dvLeft  qwLeft
lr = S.intersection dvLeft  qwRight
rl = S.intersection dvRight qwLeft
rr = S.intersection dvRight qwRight
fixed = S.fromList [k | (k,v) <- B.assocs dvQw , k == v]

segmentize :: Int -> RVar Int -> Bool -> RVar [Int]
segmentize n len clip = do
    m <- len
    if m >= n
        then return [if clip then min m n else m]
        else if m > 0 
            then do
                rest <- segmentize (n - m) len clip
                if clip 
                    then shuffle (m : rest)
                    else return  (m : rest)
            else segmentize n len clip
    
mkPass :: Int -> Float -> [S.Set Char] -> RVar String
mkPass n shiftProb sets = do
    subSeqs <- segmentize n (poisson (4::Float)) True
    keys <- fmap concat $ sequence
        [ do
            s <- randomElement sets
            replicateM len (randomElement (S.toList s))
        | len <- subSeqs
        ]
    shiftMask <- replicateM n (bernoulli shiftProb) 
    return [if s then shift k else unshift k | k <- keys | s <- shiftMask]
    
main = do
    
    return ()

dvToQw  c = fromMaybe c (B.lookup  c dvQw)
qwToDv  c = fromMaybe c (B.lookupR c dvQw)

shift   c = fromMaybe c (B.lookup  c shifts)
unshift c = fromMaybe c (B.lookupR c shifts)

adjustSet :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
adjustSet f = S.fold (\i o -> maybe o (flip S.insert o) (f i)) S.empty 