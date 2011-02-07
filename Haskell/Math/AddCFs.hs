-- playing with ideas from HAKMEM §101(b)
module Math.AddCFs where

import Data.List (maximumBy)

data CF a
    = Inf
    | Step a a (CF a)   -- Step p q x = p + q/x
    deriving (Eq, Show)

fromList [] = Inf
fromList (x:xs) = Step x 1 (fromList xs)

constCF x = Step x 1 Inf
piApprox :: Num a => CF a
piApprox  = Step 3 1 (Step 7 1 (Step 15 1 (Step 1 1 (Step 292 1 (Step 1 1 (Step 1 1 Inf))))))

phi :: Floating a => a
phi = 0.5 + sqrt 1.25

phiCF :: Num a => CF a
phiCF = Step 1 1 phiCF

eCF :: Num a => CF a
eCF = fromList (2 : concat [[1,fromInteger n,1] | n <- [2,4..]])

-- Bilinear forms on a^2
data BilinearForm a = BF
    { xy, x, y, k :: a 
    } deriving (Eq, Show)

evalBF (BF a b c d) x y = a*x*y + b*x + c*y + d

evalBF' bf (Lift x) (Lift y) = Lift (evalBF bf x y)
evalBF' _ Infinity _ = Infinity -- a*x*y + b*x + c*y + d
evalBF' _ _ Infinity = Infinity

-- | Rational bilinear form, not radial basis function
data RBF a = RBF {p, q :: BilinearForm a}
    deriving (Eq, Show)

evalRBF (RBF p q) x y = evalBF p x y / evalBF q x y

evalRBF' (RBF p q) x y = case evalBF' q x y of
    Lift 0      -> Infinity
    Infinity    -> Lift 0
    Lift qxy    -> case evalBF' p x y of
        Lift pxy -> Lift (pxy / qxy)
        Infinity -> Infinity

add, sub, mul, dvd :: Num a => RBF a
add = RBF (BF 0 1   1  0) (BF 0 0 0 1)
sub = RBF (BF 0 1 (-1) 0) (BF 0 0 0 1)
mul = RBF (BF 1 0   0  0) (BF 0 0 0 1)
dvd = RBF (BF 0 1   0  0) (BF 0 0 1 0)

data CompositeCF a = CompositeCF
    { form :: RBF a
    , xCF :: CF a
    , yCF :: CF a
    } deriving (Show)

cfAdd x y = CompositeCF add x y
cfSub x y = CompositeCF sub x y
cfMul x y = CompositeCF mul x y
cfDiv x y = CompositeCF dvd x y

stepCompositeCF cf@(CompositeCF _ Inf   _) = stepX cf
stepCompositeCF cf@(CompositeCF _   _ Inf) = stepY cf
stepCompositeCF cf@(CompositeCF _   x   y) = case compareRangeWidth xRange yRange of
    GT -> stepX cf
    EQ -> stepX cf
    LT -> stepY cf
    where
        xRange = compositeCFLargestXRange cf
        yRange = compositeCFLargestYRange cf

stepX (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) = case x of
    Inf -> Left (RLF a b e f, y)
    Step p q x' -> Right
        (CompositeCF
            (RBF (BF (p*a+c) (p*b+d) (q*a) (q*b))
                 (BF (p*e+g) (p*f+h) (q*e) (q*f)))
            x' y)

stepY (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) = case y of
    Inf -> Left (RLF a c e g, x)
    Step r s y' -> Right
        (CompositeCF
            (RBF (BF (r*a+b) (s*a) (r*c+d) (s*c))
                 (BF (r*e+f) (s*e) (r*g+h) (s*g)))
            x y')

emit t u (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) = 
    (CompositeCF 
        (RBF (BF (  u*e) (  u*f) (  u*g) (  u*h)) 
             (BF (a-t*e) (b-t*f) (c-t*g) (d-t*h))) 
        x y)

emitReg t (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) = 
    (CompositeCF 
        (RBF (BF (    e) (    f) (    g) (    h)) 
             (BF (a-t*e) (b-t*f) (c-t*g) (d-t*h))) 
        x y)

emitDigit t (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) = 
    (CompositeCF 
        (RBF (BF (10*(a-t*e)) (10*(b-t*f)) (10*(c-t*g)) (10*(d-t*h))) 
             (BF (        e ) (        f ) (        g)  (        h )))
        x y)

reduceRBF orig@(RBF (BF a b c d) (BF e f g h))
    | q > 1     = RBF (BF (a `div` q) (b `div` q) (c `div` q) (d `div` q))
                      (BF (e `div` q) (f `div` q) (g `div` q) (h `div` q))
    | otherwise = orig
        where
            q = foldl1 gcd [a,b,c,d,e,f,g,h]

-- rational linear form:
-- RLF a b c d = ax + b / (cx + d)
data RLF a = RLF a a a a
    deriving (Eq, Show)

idRLF :: Num a => RLF a
idRLF = RLF 1 0 0 1

cfId cf = (RLF idRLF, cf)

evalRLF (RLF a b c d) x = (a*x + b) / (c*x + d)

-- TODO: handle corner cases
evalRLF' rlf (Lift x) = Lift (evalRLF rlf x)

stepRLF (RLF a b c d)           Inf = Left (a/c)
stepRLF (RLF a b c d) (Step p q x') = Right (RLF (a*p+b) (a*q) (c*p+d) (c*q), x')

emitRLF t u (RLF a b c d) = RLF (c*u) (d*u) (a-c*t) (b-d*t)

data Completed a = Lift a | Infinity
    deriving (Eq, Ord, Show)

data CFRange a
    = Inside  (Completed a) (Completed a)
    | Outside (Completed a) (Completed a)
    deriving (Show)

endpoints (Inside  x y) = [x,y]
endpoints (Outside x y) = [y,x]

equivRange (Inside  x y) = Outside y x
equivRange (Outside x y) = Inside  y x

normalizeRange r@(Inside x y)
    | x <= y    = r
    | otherwise = equivRange r
normalizeRange r@(Outside x y)
    | x <= y    = r
    | otherwise = equivRange r

instance Eq a => Eq (CFRange a) where
    Inside  a b == Inside  c d =  a == c && b == d
    Inside  a b == Outside d c =  a == c && b == d
    Outside b a == Inside  c d =  a == c && b == d
    Outside b a == Outside d c =  a == c && b == d

stdRange :: Num a => CFRange a
stdRange = Inside (Lift 1) Infinity

cfRange Inf = Inside Infinity Infinity
cfRange (Step p q x) = Inside (Lift p) (Lift (p+q))   -- assume continuation is >= 1

toInside (Inside  x y) = Inside x y
toInside (Outside x y) = Inside y x

-- unions :: [CFRange a] -> CFRange a
unions = id

unions' ranges
    | x <= y    = Inside  x y
    | otherwise = Outside (Lift 0) (Lift 0)
    where
        x = maximum [l | Inside l _ <- map toInside ranges]
        y = minimum [h | Inside _ h <- map toInside ranges]

lmap f (Inside  x y) = normalizeRange (Inside  (f x) (f y))
lmap f (Outside x y) = normalizeRange (Outside (f x) (f y))

compositeCFRange cf = 
    compositeCFXRanges cf ++ compositeCFYRanges cf

compositeCFLargestXRange cf = maximumBy compareRangeWidth (compositeCFXRanges cf )
compositeCFXRanges (CompositeCF rbf x y) =
    [ lmap (\y -> evalRBF' rbf x y) yRange
    | x <- endpoints xRange
    ] where
        xRange = cfRange x
        yRange = cfRange y

compositeCFLargestYRange cf = maximumBy compareRangeWidth (compositeCFYRanges cf )
compositeCFYRanges (CompositeCF rbf x y) =
    [ lmap (\x -> evalRBF' rbf x y) xRange
    | y <- endpoints yRange
    ] where
        xRange = cfRange x
        yRange = cfRange y

compareRangeWidth xRaw yRaw = case (x,y) of
    (Outside xlo xhi, Outside ylo yhi) -> compare (width ylo yhi) (width xlo xhi)
    (Inside  xlo xhi, Inside  ylo yhi) -> compare (width xlo xhi) (width ylo yhi)
    (Outside{}, Inside{}) -> GT
    (Inside{}, Outside{}) -> LT
    where
        width (Lift a) (Lift b) = Lift (b - a)
        width Infinity _        = Infinity
        width _        Infinity = Infinity
        
        x = normalizeRange xRaw
        y = normalizeRange yRaw

-- requires inputs to be normalized
Inside Infinity b `contains` Inside c d
    = d <= b
Inside a b `contains` Inside c d
    = a <= c && d <= b
Outside a b `contains` Outside Infinity d
    = d >= b
Outside a b `contains` Outside c d
    = a >= c && d >= b

rlfRange rlf = lmap (evalRLF' rlf) . cfRange

-- TODO: 
--  * check range stuff, especially handling of infinities; I don't think it's right.  I also 
--    don't think it's even necessary to represent infinity - we shouldn't even be checking the 
--    range of an 'Inf' CF, because such a fraction can and should simply be eliminated;
--    it has yielded all the information it contains.
--  * implement reduction/emission driver, preferably also with some control over how long
--    it will search for a term before signalling that the term "might be infinity"
--  * generalize so that ranges can be computed with fractional types when
--    CFs are over integral types
--  * See whether this construction can be easily translated to use the numerator-denominator
--    term grouping (0-centered rather than ∞-centered) used in the continued-fractions library
