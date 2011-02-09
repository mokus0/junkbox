-- playing with ideas from HAKMEM §101(b)
module Math.AddCFs where

import Data.List (maximumBy)
import Data.Monoid

data CF a
    = Inf
    | Step !a !a (CF a)   -- Step p q x = p + qxyx
    deriving (Eq, Show)

evalCF Inf = error "evalCF: Inf"
evalCF (Step p q Inf) = fromIntegral p
evalCF (Step p q   x) = fromIntegral p + fromIntegral q / evalCF x

takeCF _ Inf = Inf
takeCF 0   x = Inf
takeCF n (Step p q x) = Step p q (takeCF (n-1) x)

fromList [] = Inf
fromList (x:xs) = Step x 1 (fromList xs)

toList cf = loop (rlfToCF (mempty, cf))
    where
        loop Inf = []
        loop (Step x 1 y) = x : loop y

gcf [] = Inf
gcf ((p,q):xs) = Step p q (gcf xs)

constCF x = Step x 1 Inf
piApprox :: Num a => CF a
piApprox  = Step 3 1 (Step 7 1 (Step 15 1 (Step 1 1 (Step 292 1 (Step 1 1 (Step 1 1 Inf))))))

piCF :: Num a => CF a
piCF = Step 0 4 gcf_4_pi
    where
        gcf_4_pi = gcf (zip (map fromInteger [1,3..]) (map (fromInteger.(^ 2)) [1..]))

phi :: Floating a => a
phi = 0.5 + sqrt 1.25

phiCF :: Num a => CF a
phiCF = Step 1 1 phiCF

eCF :: Num a => CF a
eCF = fromList (2 : concat [[1,fromInteger n,1] | n <- [2,4..]])

-- Bilinear forms on a^2
data BilinearForm a = BF
    { xy, x, y, k :: !a 
    } deriving (Eq, Show)

instance Functor BilinearForm where
    fmap f (BF a b c d) = BF (f a) (f b) (f c) (f d)

evalBF (BF a b c d) x y = a*x*y + b*x + c*y + d

evalBF' bf (Lift x) (Lift y) = Lift (evalBF bf x y)
evalBF' _ Infinity _ = Infinity -- a*x*y + b*x + c*y + d
evalBF' _ _ Infinity = Infinity

-- | Rational bilinear form, not radial basis function
data RBF a = RBF {p, q :: !(BilinearForm a)}
    deriving (Eq, Show)

instance Functor RBF where
    fmap f (RBF p q) = RBF (fmap f p) (fmap f q)

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
    { form :: !(RBF a)
    , xCF  :: !(CF a)
    , yCF  :: !(CF a)
    } deriving (Show)

swapXY (CompositeCF (RBF (BF a b c d) (BF e f g h)) x y) =
       (CompositeCF
           (RBF (BF a c b d)
                (BF e g f h))
           y x)

cfAdd x y = CompositeCF add x y
cfSub x y = CompositeCF sub x y
cfMul x y = CompositeCF mul x y
cfDiv x y = CompositeCF dvd x y

stepCompositeCF cf@(CompositeCF _ Inf   _) = stepX cf
stepCompositeCF cf@(CompositeCF _   _ Inf) = stepY cf
stepCompositeCF cf@(CompositeCF _   x   y) = case compareRangeWidth xRange yRange of
    GT -> stepX cf
    EQ -> stepX (swapXY cf)
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

compositeCFToCF :: Integral a => CompositeCF a -> CF a
compositeCFToCF = compositeCFToCFWith (emitSimpleRF . idRange)
    where
        idRange = id :: CFRange Rational -> CFRange Rational

compositeCFToGCF :: Integral a => CompositeCF a -> CF a
compositeCFToGCF = compositeCFToCFWith (emitGCF . idRange)
    where
        idRange = id :: CFRange Rational -> CFRange Rational

compositeCFToCFWith :: (RealFrac b, Integral a) => (CFRange b -> Maybe (a,a)) -> CompositeCF a -> CF a
compositeCFToCFWith shouldEmit cf = case shouldEmit (compositeCFRange cf) of
    Just (t,u)  -> Step t u (compositeCFToCFWith shouldEmit (emit t u cf))
    Nothing     -> either (rlfToCFWith shouldEmit)
                          (compositeCFToCFWith shouldEmit)
                          (stepCompositeCF cf)


-- rational linear form:
-- RLF a b c d = ax + b / (cx + d)
data RLF a = RLF a a a a
    deriving (Eq, Show)

-- (axy + bx + cy + d) / (exy + fx + gy + h)
-- ((ax + c)y + (bx + d)) / ((ex + g)y + (fx + h))
contractX (RBF (BF a b c d) (BF e f g h)) (Lift x) = RLF (a*x + c) (b*x + d) (e*x + g) (f*x + h)
-- lim_{x->∞} ((ay + b)x + cy + d) / ((ey + f)x + (gy + h))
-- (ay + b) / (ey + f)
contractX (RBF (BF a b c d) (BF e f g h)) Infinity = RLF a b e f

-- (axy + bx + cy + d) / (exy + fx + gy + h)
-- ((ay + b)x + cy + d) / ((ey + f)x + (gy + h))
contractY (RBF (BF a b c d) (BF e f g h)) (Lift y) = RLF (a*y + b) (c*y + d) (e*y + f) (g*y + h)
-- lim_{y->∞} ((ax + c)y + (bx + d)) / ((ex + g)y + (fx + h))
-- (ax + c) / (ex + g)
contractY (RBF (BF a b c d) (BF e f g h)) Infinity = RLF a c e g

instance Functor RLF where
    fmap f (RLF a b c d) = RLF (f a) (f b) (f c) (f d)

instance Num a => Monoid (RLF a) where
    -- evalRLF mempty = id
    mempty = RLF 1 0 0 1
    -- evalRLF (mappend f g) = evalRLF f . evalRLF g
    mappend (RLF a b c d) (RLF e f g h) = 
        RLF (e*a + b*g) (f*a + b*h)
            (e*c + d*g) (f*c + d*h)

reduceRLF rlf@(RLF a b c d)
    | q > 1     = RLF (a `div` q) (b `div` q) (c `div` q) (d `div` q)
    | otherwise = rlf
    where
        q = foldl1 gcd [a, b, c, d]

evalRLF (RLF a b c d) x = (a*x + b) / (c*x + d)

evalRLF' (RLF a b c d) (Lift x)
    | c * x + d == 0    = Infinity
evalRLF' (RLF 0 b 0 d) _ = Lift (b/d)
evalRLF' (RLF 0 b c d) Infinity = Lift 0
evalRLF' (RLF a b c d) Infinity = Infinity
evalRLF' rlf (Lift x) = Lift (evalRLF rlf x)

stepRLF (RLF a b c d)           Inf = Left (a, c)
stepRLF (RLF a b c d) (Step p q x') = Right (RLF (a*p+b) (a*q) (c*p+d) (c*q), x')

emitRLF t u (RLF a b c d) = RLF (c*u) (d*u) (a-c*t) (b-d*t)

rlfToCF :: (Integral a) => (RLF a, CF a) -> CF a
rlfToCF = rlfToCFWith (emitSimpleRF . idRange)
    where
        idRange = id :: CFRange Rational -> CFRange Rational

rlfToGCF :: (Integral a) => (RLF a, CF a) -> CF a
rlfToGCF = rlfToCFWith (emitGCF . idRange)
    where
        idRange = id :: CFRange Rational -> CFRange Rational

-- rlfToCFVia :: (RealFrac b, Integral a) => b -> (RLF a, CF a) -> CF a
-- rlfToCFVia tyWit (rlf,cf) = case rlfRange rlf cf of
--     Inside (Lift x) (Lift y)
--         |   y <  fromInteger (ceiling (x `asTypeOf` tyWit))
--         || (x == fromInteger (floor   x) && y < x+1)
--         -> Step (floor x) 1 (rlfToCF (emit (floor x)))
--     _ -> either finish rlfToCF (stepRLF rlf cf)
--     where
--         emit t = (emitRLF t 1 rlf, cf)
--         finish (a,c) = fracToCF a c -- TODO: is this right?

emitSimpleRF (Inside (Lift x) (Lift y))
    |   y <  fromInteger (ceiling x)
    || (x == fromInteger (floor   x) && y < x+1)
    = Just (floor x, 1)
emitSimpleRF _ = Nothing

emitGCF (Inside (Lift x) (Lift y))
    | x >= 1 && 0 < d && d <= fx -- y > x
    || (y > 0 && fx + 1 == cy)
    = Just (fx, d)
    where 
        fx = floor x
        cy = ceiling y
        d = cy - fx
emitGCF _ = Nothing

rlfToCFWith :: (RealFrac b, Integral a) => (CFRange b -> Maybe (a,a)) -> (RLF a, CF a) -> CF a
rlfToCFWith shouldEmit (rlf,cf) = case shouldEmit (rlfRange rlf cf) of
    Just (t,u)  -> Step t u (rlfToCFWith shouldEmit (emit t u))
    Nothing     -> either finish (rlfToCFWith shouldEmit) (stepRLF rlf cf)
    where
        emit t u = (emitRLF t u rlf, cf)
        finish (a,c) = fracToCF a c -- TODO: is this right?

fracToCF :: Integral a => a -> a -> CF a
fracToCF p 0 = Inf
fracToCF p q = Step d 1 (fracToCF q m) -- p/q as CF
    where
        -- d+m/q = p/q
        (d,m) = p `divMod` q

data Completed a = Lift a | Infinity
    deriving (Eq, Ord, Show)

instance Functor Completed where
    fmap f (Lift x) = Lift (f x)
    fmap f Infinity = Infinity

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

cfRange :: Num a => CF a -> CFRange a
cfRange Inf = Inside Infinity Infinity
cfRange (Step p q x) = Inside (Lift p) (Lift (p+q))   -- assume continuation is >= 1

toInside (Inside  x y) = Inside x y
toInside (Outside x y) = Inside y x

unions :: (Num a, Ord a) => [CFRange a] -> CFRange a
unions ranges
    | x <= y    = Inside  x y
    | otherwise = Outside (Lift 0) (Lift 0)
    where
        x = minimum [l | Inside l _ <- map toInside ranges]
        y = maximum [h | Inside _ h <- map toInside ranges]

lmap f (Inside  x y) = normalizeRange (Inside  (f x) (f y))
lmap f (Outside x y) = normalizeRange (Outside (f x) (f y))

compositeCFRange cf = unions (compositeCFRanges cf)
compositeCFRanges cf = compositeCFXRanges cf ++ compositeCFYRanges cf

compositeCFLargestXRange cf = maximumBy compareRangeWidth (compositeCFXRanges cf )

compositeCFXRanges (CompositeCF rbf x y) =
--    [ lmap (\y -> evalRBF' (fmap realToFrac rbf) (fmap realToFrac x) (fmap realToFrac y)) (cfRange y)
    [ rlfRange (contractX rbf x) y 
    | x <- endpoints (cfRange x)
    ]

compositeCFLargestYRange cf = maximumBy compareRangeWidth (compositeCFYRanges cf )
compositeCFYRanges (CompositeCF rbf x y) =
--    [ lmap (\x -> evalRBF' (fmap realToFrac rbf) (fmap realToFrac x) (fmap realToFrac y)) (cfRange x)
    [ rlfRange (contractY rbf y) x
    | y <- endpoints (cfRange y)
    ]

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

rlfRange :: (Real a, Ord b, Fractional b) => RLF a -> CF a -> CFRange b
rlfRange rlf@(RLF a b c d) x = normalizeRange $ case (a*d) `compare` (b*c) of
    LT -> {- antitone -} Inside fXHi fXLo
    EQ -> {- constant -} Inside fXLo fXHi
    GT -> {- monotone -} Inside fXLo fXHi
    where
        Inside xLo xHi = toInside (lmap (fmap realToFrac) (cfRange x))
        rlf' = fmap realToFrac rlf
        fXLo = evalRLF' rlf' xLo
        fXHi = evalRLF' rlf' xHi

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
