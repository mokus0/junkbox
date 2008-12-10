{-
 -      ``wgs84''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    ImplicitParams
  #-}

module Main where

import Test.QuickCheck
import Control.Parallel

-- import Prelude hiding ((^))
-- import qualified Prelude
-- -- a RULES pragma won't work here because (^) gets inlined and specialized too aggressively
-- x ^ 2 = x * x
-- x ^ y = x Prelude.^ y

main = main'
main'  = checkMany ((\x y z -> time (scale lat x y z)) ) 
main'' = testWith (epsEq (2*epsilon)) 

scale f x y z = f (s x) (s y) (s z)
    where s = (*1000000)

nullTest :: Double -> Double -> Double -> Bool
nullTest x y z = x `seq` y `seq` z `seq` True
checkMany xs = check (defaultConfig {configMaxTest = 1000000}) xs
testWith (~=) = checkMany $ \x y z -> if x^2 + y^2 > 2
    then scale lat x y z ~= scale refLat x y z
    else True
time x = x `seq` True

-- WGS84 spheroid
a, b :: Double
a = 6378137
b = 6356752.314245

a_2 = a * a
b_2 = b * b

e = sqrt e_2
e_2 = (a_2-b_2)/(a_2)
e_4 = e_2 * e_2

e'_2 = a_2/b_2 - 1

epsilon = atan2 4 a -- 4 meters at equator
epsEq eps x y = abs (x - y) < eps
(~=) = epsEq epsilon

-- converge f (~=) x = get (iterate f x)
--         where
--                 get (a : rest@(b : _))
--                         | a ~= b        = b
--                         | otherwise     = get rest
-- 

-- lat2 = let
--     ?a = a
--     ?b = b
--     in lat2'

lat2 x y z = phi
    where
--        a       = ?a
--        b       = ?b
--        a_2     = a * a
--        b_2     = b * b
        
        {-# NOINLINE big_e_2 #-}
        big_e_2 = a_2 - b_2
        
        {-# NOINLINE e_2 #-}
        {-# NOINLINE e_4 #-}
        {-# NOINLINE e'_2 #-}
        e_2     = big_e_2 / a_2
        e_4     = e_2 * e_2
        
        e'_2    = big_e_2 / b_2
        
        {-# NOINLINE x_2 #-}
        {-# NOINLINE y_2 #-}
        {-# NOINLINE z_2 #-}
        x_2     = x * x
        y_2     = y * y
        z_2     = z * z
        
        {-# NOINLINE r_2 #-}
        {-# NOINLINE r #-}
        r_2     = x_2 + y_2
        r       = sqrt r_2
        
        {-# NOINLINE foo #-}
        {-# NOINLINE bar #-}
        {-# NOINLINE bar_2 #-}
        {-# NOINLINE eep #-}
        {-# NOINLINE eep_2 #-}
        foo     = (1 - e_2) * z_2
        bar     = r - e_2 * r0
        bar_2   = bar * bar
        eep     = big_s + recip big_s + 1
        eep_2   = eep * eep
        
        {-# NOINLINE big_f #-}
        {-# NOINLINE big_g #-}
        {-# NOINLINE big_g_2 #-}
        {-# NOINLINE big_g_3 #-}
        {-# NOINLINE big_c #-}
        {-# NOINLINE big_c_2 #-}
        {-# NOINLINE big_s #-}
        {-# NOINLINE big_p #-}
        {-# NOINLINE big_q #-}
        {-# NOINLINE r0 #-}
        big_f   = 54 * b_2 * z_2
        big_g   = r_2 + foo - e_2 * big_e_2
        big_g_2 = big_g * big_g
        big_g_3 = big_g * big_g_2
        big_c   = e_4 * big_f * r_2 / big_g_3
        big_c_2 = big_c * big_c
        big_s   = (1 + big_c + sqrt (big_c_2 + big_c + big_c)) ** (1/3)
        big_p   = big_f / (3 * eep_2 * big_g_2)
        big_q   = sqrt (1 + 2 * e_4 * big_p)
        r0      = negate (big_p * e_2 * r) / (1 + big_q) + sqrt (0.5 * a_2 * (1 + recip big_q) - (big_p * foo)/(big_q * (1 + big_q)) - 0.5 * big_p * r_2)
--        big_u   = sqrt (bar_2 + z_2)
        big_v   = sqrt (bar_2 + foo)
        z0      = z / (a * big_v)
        
--        h       = big_u * (1 - b_2 / (a * big_v))
        phi     = atan ((z + big_e_2 * z0) / r)
--        lambda  = atan2 y x


lat x y z = converge (iterate improve phi)
        where   -- (~=) : tightness relation
                (~=) :: Double -> Double -> Bool
                x ~= y = abs (x - y) < epsilon
                        where epsilon = atan2 4 a -- 4 meters at equator
                
                -- converge : iterate until tight by (~=)
                converge :: [Double] -> Double
                converge (a : rest@(b : _))
                        | a ~= b        = b
                        | otherwise     = converge rest
                
                -- WGS84 spheroid (globally constant)
                a, b, e_2 :: Double
                a = 6378137
                b = 6356752.314245
--                {-# NOINLINE big_e_2 #-}
--                {-# NOINLINE e_2 #-}
                big_e_2 = a_2 - b_2
                e_2 = big_e_2 / a_2
                
--                {-# NOINLINE a_2 #-}
--                {-# NOINLINE b_2 #-}
                a_2 = a*a
                b_2 = b*b
                
                -- p : distance from z axis (locally constant)
                {-# NOINLINE p #-}
                p :: Double
                p = sqrt(x*x + y*y)
                
                -- phi : initial estimate
                phi :: Double
                phi = atan2 z (p * (1 - e_2))
                
                -- improve : refinement step
                improve :: Double -> Double
                improve phi = atan2 (z + v * s) p
                        where 
                                {-# NOINLINE s #-}
                                s = sin phi
                                v = big_e_2 / sqrt (a_2 - big_e_2 * s * s)
                                
                                -- v = (a_2 - b_2) / sqrt (a_2 * cos2 phi + b_2 * sin2 phi)
                                -- sin2 x = let y = sin x in y * y
                                -- cos2 x = let y = cos x in y * y

-- reference implementation; straight transcription of something I saw on the 'net
-- ( http://www.movable-type.co.uk/scripts/latlong-convert-coords.html )
refConverge f eps x
        | abs (fst x - snd x) < eps     = fst (f x)
        | otherwise                     = refConverge f eps (f x)

refLat x y z = refConverge f epsilon (phi_0, 2 * pi)
        where
                e_2 = (a^2-b^2)/a^2

                p = sqrt(x^2+y^2)
                phi_0 = atan2 z (p * (1 - e_2))
                
                f (phi, phi') = (atan2 (z+e_2*v*sin phi) p, phi)
                        where v = a / sqrt (1-e_2*(sin phi)^2)
