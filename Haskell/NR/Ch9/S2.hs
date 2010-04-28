module NR.Ch9.S2 where

import NR.Ch9.S1
import Data.List
import Data.Ord

-- | Using the false-position method, return a root of a function known
-- to lie between x1 and x2.  The root is refined until its accuracy is += xacc.
rtflsp f x1 x2 xacc = convergeToEps "rtflsp" xacc (falsePosition f x1 x2)

-- |Iteratively refine a bracketing interval [x1, x2] of a root of f
-- until total convergence (which may or may not ever be achieved) using 
-- the false-position method.
-- 
-- Each element of the returned list is a pair (x,dx) where
-- x is a new estimate of a root of f x and dx is the amount this
-- estimate differs from the last.
falsePosition f x1 x2
    | f1 <= 0 && f2 >= 0    = go x1 f1 x2 f2
    | f2 <= 0 && f1 >= 0    = go x2 f2 x1 f1
    | otherwise             = error "falsePosition: given interval does not bracket a root"

    where
        f1 = f x1
        f2 = f x2
        
        go xl fl xh fh = case compare fNew 0 of
            LT -> (xNew, xl-xNew) : go xNew fNew  xh   fh
            EQ -> [(xNew, 0)]
            GT -> (xNew, xh-xNew) : go xl   fl    xNew fNew
            where
                dx = xh - xl
                xNew = xl + dx * fl / (fl - fh)
                fNew = f xNew

-- |Using the secant method, return the root of a function thought to lie between
-- x1 and x2.  The root is refined until its accuracy is +-xacc.
rtsec f x1 x2 xacc = convergeToEps "rtsec" xacc (secantMethod f x1 x2)

-- |Iteratively refine 2 estimates x1, x2 of a root of f until total 
-- convergence (which may or may not ever be achieved) using the
-- secant method.
-- 
-- Each element of the returned list is a pair (x,dx) where
-- x is a new estimate of a root of f x and dx is the amount this
-- estimate differs from the last.
secantMethod f x1 x2 
    | abs f1 < abs f2       = go x2 f2 x1 f1
    | otherwise             = go x1 f1 x2 f2
    where
        f1 = f x1
        f2 = f x2
        
        go xl fl rts fRts
            | fNew == 0 = [(xNew, 0)]
            | otherwise = (rts, dx) : go rts fRts xNew fNew
            where
                dx = (xl - rts) * fRts / (fRts - fl)
                xNew = rts + dx
                fNew = f xNew

zriddr f x1 x2 xacc = convergeToEps "zriddr" xacc (riddersMethod f x1 x2)

riddersMethod f x1 x2 
    |  f1 < 0 && f2 < 0
    || f2 > 0 && f1 > 0 = error "riddersMethod: interval does not bracket a root"
    | otherwise         = tail (go x1 f1 x2 f2 undefined)
    where
        f1 = f x1
        f2 = f x2
        
        -- TODO: currently, ans is undefined on first pass, so cannot check xNew==ans.  shift endpoints of loop to make ans well-defined at all times.
        go xl fl xh fh ans 
            | xl == xh  = [(ans, 0)]
            | fNew == 0 = [(xNew, 0)]
            | otherwise = (ans, xNew - ans) : rebrac xl fl xm fm xh fh xNew fNew
            where
                xm = 0.5 * (xl + xh)
                fm = f xm
                s = sqrt (fm*fm - fl*fh)
                xNew = xm + (xm-xl)*((if fl >= fh then id else negate) fm / s)
                fNew = f xNew

        rebrac xl fl xm fm xh fh xNew fNew
            | foo fm fNew   = go xm   fm   xNew fNew xNew
            | foo fl fNew   = go xl   fl   xNew fNew xNew
            | foo fh fNew   = go xNew fNew xh   fh   xNew
            | otherwise                 = error "zriddr: encountered singularity"
        foo a b = a /= 0 && signum b /= signum a

minBy cmp x y = case cmp x y of
    LT -> x
    _  -> y