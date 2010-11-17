{-# LANGUAGE ViewPatterns #-}
module Math.InductiveCF where

-- Experimental representation of continued fractions, and an initial attempt
-- at mutually-recursive add and multiply operations for them.  Can't seem
-- to find a way to make the recursion well-founded both computationally and
-- mathematically.  It's easy to make a computationally-well-founded recursion,
-- but all the ways of doing so that I've found so far just do it by infinitely
-- deferring any actual mathematical progress.

data CF a
    = CFZero
    | CFAdd a (CF a)
    | CFDiv a (CF a)
    deriving (Eq, Show)

eval CFZero          = 0
eval (CFAdd b x) =      b + eval x
eval (CFDiv a x) = a / eval x

scale a CFZero = CFZero
scale a (CFAdd x y) = CFAdd (a*x) (scale a y)
scale a (CFDiv x y) = CFDiv (a*x) y

inv (CFDiv a x) = scale (recip a) x
inv a = CFDiv 1 a

trunc 0 x = CFZero
trunc n CFZero = CFZero
trunc n (CFAdd a b) = CFAdd a (trunc (n-1) b)
trunc n (CFDiv a b) = case trunc (n-1) b of
    CFZero -> CFAdd a CFZero
    rest -> CFDiv a rest

simplify CFZero = CFZero
simplify (CFAdd x (CFAdd y z)) = CFAdd (x+y) (simplify z)
simplify (CFDiv x (CFDiv y z)) = CFDiv (x/y) (simplify z)
simplify (CFAdd x y) = CFAdd x (simplify y)
simplify (CFDiv x y) = CFDiv x (simplify y)

instance Fractional a => Num (CF a) where
    fromInteger 0 = CFZero
    fromInteger x = CFAdd (fromInteger x) 0
    
    negate = scale (-1)
    
    CFZero + x = x
    x + CFZero = x
    
    CFDiv 0 _ + x = x
    x + CFDiv 0 _ = x
    
    CFAdd a x + CFAdd b y = CFAdd (a+b) (x+y)
    CFDiv a x + CFDiv b y = (scale a y + scale b x) * inv (x * y)
    
    CFAdd a x + y = CFAdd a (x + y)
    y + CFAdd a x = CFAdd a (x + y)
    

    CFZero * x = CFZero
    x * CFZero = CFZero
    
    CFDiv 0 _ * x = CFZero
    x * CFDiv 0 _ = CFZero
    
    CFDiv a x * CFDiv b y = CFDiv (a*b) (x*y)
    CFAdd a x * CFAdd b y = CFAdd (a*b) (scale a x + scale b y + x*y)
    
    (simplify -> CFAdd a (CFDiv b x)) * CFDiv c y = CFDiv (a*c) y + CFDiv (b*c) (x*y)
    x*y = y*x
--    CFDiv b y * (simplify -> CFAdd a x) = CFAdd (a*b) (scale b x) * inv y

    
    abs     = error "CF: abs not defined"
    signum  = error "CF: signum not defined"