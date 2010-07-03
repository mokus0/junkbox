{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Root.Finder.Brent
    ( Brent
    , brent
    ) where

import Math.Root.Bracket
import Math.Root.Finder
import Data.Maybe
import Text.Printf

data Brent a b = Brent
    { brA   :: !a
    , brFA  :: !b
    , brB   :: !a
    , brFB  :: !b
    , brC   :: !a
    , brFC  :: !b
    , brD   :: a
    , brE   :: a
    } deriving (Eq, Show)

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

-- TODO: clean up this mess!
instance RealFloat a => RootFinder Brent a a where
    initRootFinder f x1 x2 = fixMagnitudes (Brent x1 f1 x2 f2 x1 f1 dx dx)
        where f1 = f x1; f2 = f x2; dx = x2 - x1
    
    stepRootFinder f r@(Brent a fa b fb c fc _ e)
        |  abs fa > abs fb
        && abs e >= tol1
        && 2 * p < max2p    = advance (p/q)
        |  otherwise        = advance xm
        where
            -- Minimum step size and limits on 'p' to continue with inverse-quadratic interpolation
            tol1 = 2 * eps * (abs b + 0.5)
            max2p = min (3 * xm * q - abs (tol1 * q))
                        (abs (e * q))
            
            -- midpoint for bisection step
            xm = 0.5 * (c - b)
            
            -- subdivision point for inverse quadratic interpolation step
            -- (p/q)
            s = fb / fa
            p = abs p'
            q = if p' > 0 then negate q' else q'
            (p',q') | a == c    = (2 * xm * s, 1 - s)
                    | otherwise = let t = fa / fc
                                      r = fb / fc
                                   in ( s * (2 * xm * t * (t - r) - (b - a) * (r - 1))
                                      , (t - 1) * (r - 1) * (s - 1)
                                      )
            
            -- Actual advancement used for both steps
            advance d = reorder r{brA = b, brFA = fb, brB = b', brFB = f b'}
                where
                    b' = if abs d > tol1 then b + d else b + abs tol1 * signum xm


    estimateRoot  Brent{brB = b}          = b
    estimateError Brent{brB = b, brC = c} = c - b
    converged   _ Brent{brFB = 0}   = True
    converged tol br@Brent{brB = b} = abs (estimateError br) <= tol1
        where
            tol1 = 4 * eps * abs b + tol

brent :: RealFloat a => (a -> a) -> a -> a -> a -> Either (Brent a a) a
brent f x1 x2 xacc = fmap estimateRoot (findRoot f x1 x2 xacc)

-- on input, (a,c) are prev bracket, b is new guess.
-- on output, b and c bracket the root and |f(b)| <= |f(c)|
-- and 'a' is either the prev guess or one of the new endpoints
-- if f(a) is outside the range [f(b), f(c)].
--
-- Basically, this ensures that the algorithm always tightens either the
-- bound on the domain or the bound on the range, and never loosens the bound
-- on the domain.  In rare cases, a midpoint step (but not an inverse-quadratic 
-- step) can loosen the bound on the range, if I understand correctly.
reorder :: (Num a, Num b, Ord b) => Brent a b -> Brent a b
reorder = fixMagnitudes . fixSigns

-- Establish invariant that b and c bracket the root,
-- based on existing invariant that (a,c) already does.
-- 
-- (a,c) brackets implies that either (b,c) or (a,b) brackets.  In the 
-- former case, nothing needs to be done as (by construction) either fb is already
-- between fa and fc or b is already between a and c (depending which kind of 
-- step was taken).  In the latter case, discard C and use A in its place, because
-- c and fc are both (by the existing invariants - (a,c) bracket, |f(c)| >= |f(a)|) 
-- outside the new region of interest.
fixSigns :: (Num a, Num b, Ord b) => Brent a b -> Brent a b
fixSigns br@Brent{ brA  =  a, brB  =  b
                 , brFA = fa, brFB = fb, brFC = fc }
    |  (fb > 0 && fc > 0) || (fb < 0 && fc < 0)
    = br { brC = a, brFC = fa
         , brD = d', brE = d'
         }
    | otherwise 
    = br
    where d' = b - a

-- Establish invariant that |f(c)| >= |f(b)|.
-- If it is not already so, reset 'a' as well to ensure 'fa' falls
-- between fb and fc.
fixMagnitudes :: (Num b, Ord b) => Brent a b -> Brent a b
fixMagnitudes br@Brent{ brC  =  c, brB  =  b
                      , brFC = fc, brFB = fb }
    | abs fc < abs fb
    = br { brA = b, brFA = fb
         , brB = c, brFB = fc
         , brC = b, brFC = fb
         }
    | otherwise 
    = br

-- |debugging function to show a nice trace of the progress of the algorithm
_traceBrent :: (PrintfArg a, RealFloat a,
                PrintfArg b, Ord b, Num b,
                RootFinder Brent a b) =>
               (a -> b) -> Maybe (a, a) -> IO ()
_traceBrent f mbRange = do
    xs <- sequence
        [ put br >> return br
        | br <- traceRoot f x0 x1 (Just eps)
        ]

    putStrLn "(converged)"
    go (last xs)
    where 
        (x0,x1) = fromMaybe (last (bracket f 0 1)) mbRange
        put Brent{brA=a, brB=b, brC=c, brFA=fa, brFB=fb, brFC=fc} = 
            putStrLn . map fixPlus $
            printf (concat (replicate 6 "%-+25g")) a b c fa fb fc
        fixPlus '+' = ' '
        fixPlus c = c
        go x 
            | x == x'   = return ()
            | otherwise = put x >> go x'
            where x' = stepRootFinder f x