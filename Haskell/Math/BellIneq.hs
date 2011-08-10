{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Math.BellIneq where

import Control.Monad
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Normal (normalPair, stdNormal)
import Data.VectorSpace
import Math.CayleyDickson
import Math.Statistics.Monoid

newtype Rotation = R (H Double)
    deriving (Eq, Show)

instance AdditiveGroup Rotation where
    zeroV = R 1
    R r1 ^+^ R r2 = R (r1 * r2)
    negateV (R r) = R (recip r)

instance Distribution StdUniform Rotation where
    rvar ~StdUniform = do
        [w,x,y,z] <- unitVec 4
        return (R (CD (CD w x) (CD y z)))

rPow (R r) n = R (r ^ n)

flipR (R r) = R (negateV r)

applyR (R q) (x,y,z) = vectorPart (q * v * recip q)
    where
        v = CD (CD 0 x) (CD y z)

mkRotation theta x y z = R (CD (CD c (ux*s)) (CD (uy*s) (uz*s)))
    where
        c = cos (theta / 2)
        s = sin (theta / 2)
        (ux,uy,uz) = normalized (x,y,z)
        
randomRotationWithAngle theta = do
    [ux,uy,uz] <- unitVec 3
    return (mkRotation theta ux uy uz)

-- A classical hidden-variables model for spin
-- For our purposes here a Spin is an axis of rotation, represented by a
-- unit 3-vector.
newtype Spin = S { spinAxis :: (Double, Double, Double) }
mkSpin = S . normalized

flipS (S x) = S (negateV x)

spinX = mkSpin (1, 0, 0)
spinY = mkSpin (0, 1, 0)
spinZ = mkSpin (0, 0, 1)

instance Eq Spin where
    x == y  = spinAxis x == spinAxis y

instance Show Spin where
    showsPrec p x = showParen (p>10)
        ( showString "mkSpin "
        . showsPrec 11 (spinAxis x)
        )

instance Distribution StdUniform Spin where
    rvar _ = do
        [x,y,z] <- (unitVec 3)
        return (S (x,y,z))

angle u v = acos ((u <.> v) / (magnitude u * magnitude v))

-- take a rotation and a spin, return a new spin with its axis rotated by the rotation
rotateSpin r s = mkSpin (applyR r (spinAxis s))

-- 1 + corr a c >= abs (corr b a - corr b c)
-- 1 >= abs (corr b a - corr b c) - corr a c
bell corr a b c = 
    bellval corr a b c <= 1

bellval corr a b c = abs (corr b a - corr b c) - corr a c

-- bell test; classically, result <= 1
bellTest corr = do
    r <- stdUniform
    a <- stdUniform
    let b = rotateSpin r a
        c = rotateSpin r b
    
    return (bellval corr a b c)

chsh corr a b a' b'
    = chshval corr a b a' b' <= 2

chshval corr a b a' b'
    = corr a  b
    + corr a  b'
    + corr a' b
    - corr a' b'

-- CHSH test; classically, result <= 2
chshTest corr = do
    [x,y] <- replicateM 2 stdUniform
    let theta = deg2rad 135
    r <- stdUniform --randomRotationWithAngle theta
    let [x',y'] = map (rotateSpin r) [x,y]
    return (chshval corr x y x' y')


-- Not entirely sure this is right, but it seems to have
-- all the properties I expect.
expectedCorr x y = ssq (u <.> v) / (magnitudeSq u * magnitudeSq v)
    where
        ssq x = x * abs x
        
        u = spinAxis x
        v = spinAxis y


-- misc util functions
deg2rad d = d * (pi/180)

vectorPart (CD (CD _ x) (CD y z)) = (x,y,z)

unitVec n = do
    cs <- replicateM n stdNormal
    if any (/=0) cs 
        then do
            let m = sqrt (sum (map (^2) cs))
            return (map (/m) cs)
        else unitVec n

spinSample trueSpin samplingAxis = do
    let p = expectedCorr trueSpin samplingAxis
    u <- bernoulli (abs p)
    return (u * signum p)

singlet = do
    s <- stdUniform
    return (s, flipS s)

singletSample x y = do
    (s,t) <- singlet
    xObs <- spinSample s x
    yObs <- spinSample t y
    return  (xObs, negate yObs)

-- Simulate a CHSH test.  I don't think I'm doing this right.
chshDefaultTheta = deg2rad 135

chshSetup theta = do
    [x,y] <- replicateM 2 stdUniform
    r <- randomRotationWithAngle theta
    let [x',y'] = map (rotateSpin r) [x,y]
    return (x, y, x', y')

chshStep x y x' y' = do
    xTest <- randomElement [Left x, Right x']
    yTest <- randomElement [Left y, Right y']
    liftE2 singletSample xTest yTest

liftE  f = emap fmap f
liftE2 f = emap (fmap . fmap) (liftE . f)
emap lift f = either (lift Left . f) (lift Right . f)


data CHSHCoincidenceSummary = CS
    { aa, ab, ba ,bb :: !(Covar Double) }
    deriving (Eq, Show)
instance Monoid CHSHCoincidenceSummary where
    mempty = CS mempty mempty mempty mempty
    mappend (CS aa1 ab1 ba1 bb1) (CS aa2 ab2 ba2 bb2)
        = CS (aa1 `mappend` aa2) (ab1 `mappend` ab2) (ba1 `mappend` ba2) (bb1 `mappend` bb2)

data CHSHDatum = A | B
chshCoincidenceSummaryCorr cs d1 d2 = maybe (error "insufficient data") id (correl (f cs))
    where
        f = case (d1, d2) of
            (A,A) -> aa; (A,B) -> ab; (B,A) -> ba; (B,B) -> bb

chshCoincidenceSummary (Left  (Left  (x, y))) = mempty {aa = mkCovar x y}
chshCoincidenceSummary (Left  (Right (x, y))) = mempty {ab = mkCovar x y}
chshCoincidenceSummary (Right (Left  (x, y))) = mempty {ba = mkCovar x y}
chshCoincidenceSummary (Right (Right (x, y))) = mempty {bb = mkCovar x y}

chshEmpiricalTest theta n = do
    (x,y,x',y') <- chshSetup theta
    coincidences <- replicateM n (chshStep x y x' y')
    
    return (foldMap chshCoincidenceSummary coincidences)

