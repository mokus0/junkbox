{-# LANGUAGE ViewPatterns #-}
module NR.Ch5.S2 
    ( CF(..)
    , expand, expandFrac
    , evalCF
    
    , expandInPlace, expandInPlaceCD
    ) where

import Math.Converge
import Data.Ratio
import Control.Arrow

data CF a 
    = CF    [a]
    | GCF   [(a,a)]
    deriving (Read, Show)

instance Functor CF where
    fmap f (CF  cf)  = CF  (map f cf)
    fmap f (GCF gcf) = GCF (map (f *** f) gcf)

asCF  :: Fractional a => CF a -> [a]
asCF (CF   cf) = cf
-- TODO: extend to finite case if possible
asCF (GCF ((b0,a1):(b1,a2):rest)) = go b0 b1 a1 a2 rest
    where go b0 b1 a1 a2 ((b2,a3):rest) = b0 : go (b1*s) b2 (a2*s) a3 rest
            where
                s = recip a1

asGCF :: Num a => CF a -> [(a,a)]
asGCF (CF   cf) = [(b, 1) | b <- cf]
asGCF (GCF gcf) = gcf

cfNull (CF  xs) = null xs
cfNull (GCF xs) = null xs
cfHead (CF  xs) = Left  $! head xs
cfHead (GCF xs) = Right $! head xs
cfTail (CF  xs) = CF  $! tail xs
cfTail (GCF xs) = GCF $! tail xs
cfNil = CF []
cfCons (Left  x) (CF  xs) = CF  (x:xs)
cfCons (Left  x) (GCF xs) = GCF ((x,1) : xs)
cfCons (Right x) (CF  xs) = GCF (x : [(b,1) | b <- xs])
cfCons (Right x) (GCF xs) = GCF (x:xs)
gcfFoldr cons nil (CF  xs) = foldr (uncurry cons) nil (map (\b -> (b,1))  xs)
gcfFoldr cons nil (GCF xs) = foldr (uncurry cons) nil xs

{--------------- Machinery for expanding continued fractions ---------------}

skewSnd a0 []             bn = [(bn,a0)]
skewSnd a0 ((b0,a1):rest) bn = (b0, a0) : skewSnd a1 rest bn

scanl2 :: (y -> y -> x -> y)
          -> y -> y -> [x] -> [y]
scanl2 f = go
    where
        go s t []     = [s,t]
        go s t (x:xs) = s : go t (f s t x) xs

stepA aa1 aa2 (b,a) = b * aa2 + a * aa1
stepB bb1 bb2 (b,a) = b * bb2 + a * bb1

tailSkewSnd []              end = error "tailSkewSnd: empty list"
tailSkewSnd ((b0,a1):rest)  end = go a1 rest
    where
        go a1 [] = [(end, a1)]
        go a1 ((b1,a2):rest) = (b1, a1) : go a2 rest

expandA gcf@((b0,_):_) = scanl2 stepA 1 b0 (tailSkewSnd gcf 1)
expandB gcf            = scanl2 stepB 0 1  (tailSkewSnd gcf 1)

-- TODO: optimize these for CF case
expandFrac     (asGCF -> gcf) = zipWith (/) (expandA gcf) (expandB gcf)
expand (asGCF -> gcf) = zipWith divide (expandA gcf) (expandB gcf)
    where divide a b = realToFrac a / realToFrac b

evalCF cf eps = head (convergeBy (~=) (drop 2 (expand cf)))
    where
        x ~= y      = (abs (x-y) < eps)

expandInPlace (asGCF -> gcf) = go (realToFrac b0) 1 1 0 as (tail bs)
    where
        (bs@(b0:_), as@(a1:_)) = unzip gcf
        
        go aa_j aa_jm1 1 bb_jm1 (a_j : as) (b_j : bs)
            | aa_scaled == aa_j = aa_scaled
            | bb /= 0           = go (aa * r_bb) (aa_j * r_bb) 1 r_bb as bs
            | otherwise         = error "bb == 0"
                where
                    a = realToFrac a_j
                    b = realToFrac b_j
                    
                    aa_scaled = aa * r_bb
                    aa = b * aa_j + a * aa_jm1
                    bb = b        + a * bb_jm1
                    r_bb = recip bb

-- does *not* work in fixed point
expandInPlaceCD tiny eps (asGCF -> ((0,a1):rest)) = a1 / expandInPlaceCD tiny eps (GCF rest)
expandInPlaceCD tiny eps (asGCF -> gcf) = go b0 0 b0 as (tail bs)
    where
        (bs@(b0:_), as@(a1:_)) = unzip gcf
        
        go c_jm1 d_jm1 f_jm1 (a_j : as) (b_j : bs)
            | abs (delt - 1) < eps    = f_j
            | otherwise = go c_j d_j f_j as bs
            where
                c_j = b_j + a_j // c_jm1
                d_j = recip' (b_j + a_j * d_jm1)
                delt = c_j * d_j
                f_j = f_jm1 * delt
                
                big = recip tiny
                recip' x@0 = big * signum x
                recip' x = recip x
                
                x // y@0 = x * big * signum y
                x // y = x / y
