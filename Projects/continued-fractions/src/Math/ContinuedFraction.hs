{-# LANGUAGE ParallelListComp, ViewPatterns #-}
module Math.ContinuedFraction
    ( CF
    , cf, gcf
    , asCF, asGCF
    
    , truncateCF
    
    , convergents
    , steed
    , lentz
    , modifiedLentz
    ) where

import Control.Arrow ((***))
import Data.List (tails, mapAccumL)

-- * The 'CF' type and basic operations

-- I think I would like to try refactoring this stuff at some point to use
-- an "Inductive" CF type, something like:
-- 
-- > data CF a
-- >     = CFZero               -- eval CFZero          = 0
-- >     | CFAdd    a (CF a)    -- eval (CFAdd    b x) =      b + eval x
-- >     | CFCont a a (CF a)    -- eval (CFCont a b x) = a / (b + eval x)

-- |A continued fraction.  Constructed by 'cf' or 'gcf'.
data CF a 
    = CF a [a]
    -- ^ Not exported. See 'cf', the public constructor.
    | GCF a [(a,a)]
    -- ^ Not exported. See 'gcf', the public constructor.

-- |Construct a continued fraction from its first term and the 
-- partial denominators in its canonical form, which is the form 
-- where all the partial numerators are 1.
-- 
-- @cf a [b,c,d]@ corresponds to @a + (b / (1 + (c / (1 + d))))@,
-- or to @GCF a [(1,b),(1,c),(1,d)]@.
cf :: a -> [a] -> CF a
cf = CF

-- |Construct a continued fraction from its first term, its partial 
-- numerators and its partial denominators.
--
-- @gcf b0 [(a1,b1), (a2,b2), (a3,b3)]@ corresponds to
-- @b0 + (a1 / (b1 + (a1 / (b1 + (a2 / (b2 + (a3 / b3)))))))@
gcf :: a -> [(a,a)] -> CF a
gcf = GCF

instance Show a => Show (CF a) where
    showsPrec p (CF b0 ab) = showParen (p>10)
        ( showString "cf "
        . showsPrec 11 b0
        . showChar ' '
        . showsPrec 11 ab
        )
    showsPrec p (GCF b0 ab) = showParen (p>10)
        ( showString "gcf "
        . showsPrec 11 b0
        . showChar ' '
        . showsPrec 11 ab
        )

instance Functor CF where
    fmap f (CF  b0 cf)  = CF  (f b0) (map f cf)
    fmap f (GCF b0 gcf) = GCF (f b0) (map (f *** f) gcf)

-- |Extract the partial numerators of a 'CF', normalizing it if necessary so 
-- that all the partial denominators are 1.
asCF  :: Fractional a => CF a -> (a, [a])
asCF (CF  b0 cf) = (b0, cf)
asCF (GCF b0 cf) = (b0, zipWith (/) bs cs)
    where
        (as, bs) = unzip cf
        cs = head as : [a / c | c <- cs | a <- drop 1 as]

-- |Extract all the partial numerators and partial denominators of a 'CF'.
asGCF :: Num a => CF a -> (a,[(a,a)])
asGCF (CF  b0  cf) = (b0, [(1, b) | b <- cf])
asGCF (GCF b0 gcf) = (b0, gcf)

-- |Truncate a CF to the specified number of partial numerators and denominators.
truncateCF :: Int -> CF a -> CF a
truncateCF n (CF  b0 ab) = CF  b0 (take n ab)
truncateCF n (GCF b0 ab) = GCF b0 (take n ab)

-- * Evaluating continued fractions

-- |Evaluate the convergents of a continued fraction using the fundamental
-- recurrence formula:
-- 
-- A_0 = b_0, B_0 = 1
--
-- A_1 = b_1b_0 + a_1,  B_1 = b_1
-- 
-- A_{n+1} = b_{n+1}A_n + a_{n+1}A_{n-1}
--
-- B_{n+1} = b_{n+1}B_n + a_{n+1}B_{n-1}
--
-- The convergents are then x_n = A_n/B_n
convergents :: (Real a, Fractional b) => CF a -> [b]
convergents (asGCF -> (realToFrac -> b0, gcf)) = drop 1 (zipWith (/) nums denoms)
    where
        nums   = 1:b0:[realToFrac b * x1 + realToFrac a * x0 | x0:x1:_ <- tails nums   | (a,b) <- gcf]
        denoms = 0:1 :[realToFrac b * x1 + realToFrac a * x0 | x0:x1:_ <- tails denoms | (a,b) <- gcf]

-- |Evaluate the convergents of a continued fraction using Steed's method.
-- Only valid if the denominator in the following recurrence for D_i never 
-- goes to zero.  If this method blows up, try 'modifiedLentz'.
--
-- D_1 = 1/b1
-- 
-- D_i = 1 / (b_i + a_i * D_{i-1})
-- 
-- dx_1 = a_1/b_1
-- 
-- dx_i = (b_i * D_i - 1) * dx_{i-1}
-- 
-- x_0 = b_0
-- 
-- x_i = x_{i-1} + dx_i
-- 
steed :: (Real a, Fractional b) => CF a -> [b]
steed (CF  b0 []) = [realToFrac b0]
steed (GCF b0 []) = [realToFrac b0]
steed (CF  0 (  a  :rest)) = map (1            /) (steed (CF  a rest))
steed (GCF 0 ((a,b):rest)) = map (realToFrac a /) (steed (GCF b rest))
steed (asGCF . fmap realToFrac -> (b0, (a1,b1):gcf)) 
    = scanl (+) b0 dxs
    where
        dxs = a1/b1 : [(b * d - 1) * dx  | (a,b) <- gcf | d <- ds | dx <- dxs]
        ds  =  1/b1 : [recip (b + a * d) | (a,b) <- gcf | d <- ds]

-- |Evaluate the convergents of a continued fraction using Lentz's method.
-- Only valid if the denominators in the following recurrence never go to
-- zero.  If this method blows up, try 'modifiedLentz'.
--
lentz :: (Real a, Fractional b) => CF a -> [b]
lentz (CF  b0 []) = [realToFrac b0]
lentz (GCF b0 []) = [realToFrac b0]
lentz (CF  0 (  a  :rest)) = map (1            /) (lentz (CF  a rest))
lentz (GCF 0 ((a,b):rest)) = map (realToFrac a /) (lentz (GCF b rest))
lentz (asGCF . fmap realToFrac -> (b0, gcf)) 
    = scanl (*) b0 (zipWith (*) cs ds)
    where
        cs = [   b + a/c  | (a,b) <- gcf | c <- b0 : cs]
        ds = [1/(b + a*d) | (a,b) <- gcf | d <- 0  : ds]


-- |Evaluate the convergents of a continued fraction using Lentz's method,
-- (see 'lentz') with the additional rule that if a denominator ever goes
-- to zero, it will be replaced by a (very small) number of your choosing,
-- typically 1e-30 or so.  This modification was proposed by Thompson and 
-- Barnett.  Additionally splits the resulting list of convergents into 
-- sublists, starting a new list every time the \'modification\' is invoked.
modifiedLentz :: (Real a, Fractional b) => b -> CF a -> [[b]]
modifiedLentz z (CF  b0 []) = [[realToFrac b0]]
modifiedLentz z (GCF b0 []) = [[realToFrac b0]]
modifiedLentz z (CF  0 (  a  :rest)) = map (map (1            /)) (modifiedLentz z (CF  a rest))
modifiedLentz z (GCF 0 ((a,b):rest)) = map (map (realToFrac a /)) (modifiedLentz z (GCF b rest))
modifiedLentz z (asGCF . fmap realToFrac -> (b0, gcf)) 
    = snd (mapAccumL multSublist b0 (separate cds))
    where
        multSublist b0 cds = let xs = scanl (*) b0 cds in (last xs, xs) 
        
        cds = zipWith (\(xa,xb) (ya,yb) -> (xa || ya, xb * yb)) cs ds
        cs = [reset (b + a/c)            id   | (a,b) <- gcf | c <- b0 : map snd cs]
        ds = [reset (b + a*d) (\den -> 1/den) | (a,b) <- gcf | d <- 0  : map snd ds]
        
        -- The sublist breaking is computed secondarily - initially, 
        -- 'cs' and 'ds' are constructed with this helper function that
        -- adds a marker to the list whenever a term of interest goes to 0,
        -- while also resetting that term to a small nonzero amount.
        -- Then later, 'separate' breaks the list every time it sees one
        -- of these markers.
        reset x f
            | x == 0    = (True,  f z)
            | otherwise = (False, f x)
        
        -- |Takes a list of (Bool,a) and breaks it into sublists, starting
        -- a new one every time it encounters (True,_).
        separate :: [(Bool,a)] -> [[a]]
        separate [] = []
        separate xs = case break fst xs of
            ([], x:xs)  -> case separate xs of
                []          -> [[snd x]]
                (xs:rest)   -> (snd x:xs):rest
            (xs, ys)            -> map snd xs : separate ys
