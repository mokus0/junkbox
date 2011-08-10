{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Math.Angle
    ( tau
    , Angle
    , turns,   toTurns,   directedTurns
    , radians, toRadians, directedRadians
    , degrees, toDegrees, directedDegrees
    , grads,   toGrads,   directedGrads
    , projectiveReal, toProjectiveReal
    
    , bisect, subdivide
    
    ) where

import Data.VectorSpace 
import Math.ProjectiveLine

-- |tau = 2*pi.  'tau' is the angle, in radians, of one full turn.
tau :: Floating a => a
tau = 2*pi

-- |Angles stored as fractions of a half-circle in the range
--   -0.5 < x <= 0.5
newtype Angle = Angle Double

turns :: Double -> Angle
turns = Angle . normalizeTurns

toTurns :: Angle -> Double
toTurns (Angle x) = x

radians :: Double -> Angle
radians = turns . (/tau)

toRadians :: Angle -> Double
toRadians = (tau *) . toTurns

degrees :: Double -> Angle
degrees = turns . (/ 360)

toDegrees :: Angle -> Double
toDegrees = (360 *) . toTurns

grads :: Double -> Angle
grads = turns . (/400)

toGrads :: Angle -> Double
toGrads = (400 *) . toTurns

-- |Normalize a number of turns to the range (-0.5) < x <= 0.5
normalizeTurns :: RealFrac a => a -> a
normalizeTurns x
    | frac > 0.5        = frac - 1
    | frac <= (-0.5)    = frac + 1
    | otherwise         = frac
    where
        _typed = int :: Int
        (int, frac) = properFraction x

-- |Convert 2 'Angle's to 'Double' values, in turns, such that
-- the first is less than the second and both have absolute value 
-- less than '1'.
directedTurns :: Angle -> Angle -> (Double, Double)
directedTurns (Angle t0) (Angle t1)
    | t0 <= t1          = (t0, t1)
    | abs t0 >= abs t1  = (t0 - 1, t1)
    | otherwise         = (t0, t1 + 1)

-- |Convert 2 'Angle's to 'Double' values, in radians, such that
-- the first is less than the second and both have absolute value 
-- less than 'tau'.
directedRadians :: Angle -> Angle -> (Double, Double)
directedRadians lo hi = (t0 * tau, t1 * tau)
    where ~(t0,t1) = directedTurns lo hi

-- |Convert 2 'Angle's to 'Double' values, in degrees, such that
-- the first is less than the second and both have absolute value 
-- less than 360.
directedDegrees :: Angle -> Angle -> (Double, Double)
directedDegrees lo hi = (t0 * 360, t1 * 360)
    where ~(t0,t1) = directedTurns lo hi

-- |Convert 2 'Angle's to 'Double' values, in grads, such that
-- the first is less than the second and both have absolute value 
-- less than 400.
directedGrads :: Angle -> Angle -> (Double, Double)
directedGrads lo hi = (t0 * 400, t1 * 400)
    where ~(t0,t1) = directedTurns lo hi

-- |Bisect 2 angles.  The order is significant, and determines the direction of
-- the result.  For example, @bisect (degrees 0) (degrees 90)@ gives @degrees 45@,
-- but @bisect (degrees 90) (degrees 0)@ gives @degrees (-135)@
bisect :: Angle -> Angle -> Angle
bisect lo hi = turns (0.5 * (theta0 + theta1))
    where
        (theta0, theta1) = directedTurns lo hi

-- |Subdivide an angular interval into 'n' parts.  Returns @n-1@ new angles
-- which, together with the given angles, are the boundaries of the @n@
-- subintervals.
subdivide :: Int -> Angle -> Angle -> [Angle]
subdivide n lo hi =
    [ turns (lerp theta0 theta1 f)
    | i <- [1..n-1]
    , let f = fromIntegral i / fromIntegral n
    ] where
        (theta0, theta1) = directedTurns lo hi


instance Show Angle where
    showsPrec p x = showParen (p>10)
        ( showString "degrees "
        . showsPrec 11 (toDegrees x)
--        ( showString "turns "
--        . showsPrec 11 (toTurns x)
        )

instance Eq Angle where
    Angle x == Angle y
        = (normalizeTurns x == normalizeTurns y)

instance Ord Angle where
    compare (Angle x) (Angle y) = compare (normalizeTurns x) (normalizeTurns y)

instance Num Angle where
    fromInteger = radians . fromInteger
    Angle x + Angle y = Angle (normalizeTurns (x+y))
    Angle x - Angle y = Angle (normalizeTurns (x-y))
    Angle x * Angle y = Angle (normalizeTurns (x*y*tau))
    
    negate  (Angle x) = Angle   (negate x)
    abs     (Angle x) = Angle   (abs    x)
    signum  (Angle x) = radians (signum x)

instance Real Angle where
    toRational = toRational . toRadians

instance AdditiveGroup Angle where
    zeroV   = Angle 0
    negateV = negate
    (^+^)   = (+)

instance VectorSpace Angle where
    type Scalar Angle = Double
    s *^ Angle v = turns (s *^ v)

-- An isomorphism between the circle and the projective line
projectiveReal :: ProjectiveLine Double -> Angle
projectiveReal Infinity = turns 0.5
projectiveReal (Real x) = radians (2 * atan x)

toProjectiveReal :: Angle -> ProjectiveLine Double
toProjectiveReal x
    | x == turns 0.5    = Infinity
    | otherwise         = Real (tan (toRadians x / 2))
