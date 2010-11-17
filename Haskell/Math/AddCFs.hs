-- playing with ideas from HAKMEM §101(b)
module Math.AddCFs where

data CF a
    = Inf
    | Step a a (CF a)   -- Step p q x = p + q/x
    deriving (Eq, Show)

-- Bilinear forms on a^2
data BilinearForm a = BF
    { xy, x, y, k :: a 
    } deriving (Eq, Show)

evalBF (BF a b c d) x y = a*x*y + b*x + c*y + d
evalBF' (BF a b c d) Infinity y = a*x*y + b*x + c*y + d

-- | Rational bilinear form, not radial basis function
data RBF a = RBF {p, q :: BilinearForm a}
    deriving (Eq, Show)

evalRBF (RBF p q) x y = evalBF p x y / evalBF q x y

-- rational linear form:
-- RLF a b c d = ax + b / (cx + d)
data RLF a = RLF a a a a

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

data Completed a = Lift a | Infinity
    deriving (Eq, Ord, Show)

data CFRange a
    = Inside  (Completed a) (Completed a)
    | Outside (Completed a) (Completed a)
    deriving (Show)

endpoints (Inside x y) = undefined

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
cfRange (Step p q x) = Inside p (p+q)   -- assume continuation is >= 1

-- unions :: [CFRange a] -> CFRange a
unions = id

lmap f (Inside  x y) = normalizeRange (Inside  (f x) (f y))
lmap f (Outside x y) = normalizeRange (Outside (f x) (f y))

compositeRange (CompositeCF rbf x y) = unions
    [ lmap (\y -> evalRBF' rbf x y) yRange
    | x <- endpoints xRange
    ] ++
    [ lmap (\x -> evalRBF' rbf x y) xRange
    | y <- endpoints yRange
    ]
    where
        xRange = cfRange x
        yRange = cfRange y