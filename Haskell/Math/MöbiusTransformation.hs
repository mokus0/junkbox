{-# LANGUAGE RecordWildCards, GADTs #-}
module Math.MöbiusTransformation
    ( MöbiusTransformation
    , identityMöbiusTransformation
    , linearTransformation
    , möbiusTransformation
    , determinant
    , normalizedCoeffs
    
    , invertMöbiusTransformation
    , composeMöbiusTransformation
    , applyMöbiusTransformation
    ) where

import Math.ProjectiveLine
import qualified Control.Category as Cat
import Data.Monoid

data MöbiusTransformation a
    -- INVARIANT: if the function is the identity, then it is represented by Identity
    -- (in other words, @not (b==0 && c==0 && a==d)@)
    = Identity
    | MöbiusTransformation
        { a, b, c, d :: !a
        , det        :: !a
        }

determinant Identity                 = 1
determinant MöbiusTransformation{..} = det

normalizedCoeffs Identity = [1,0,0,1]
normalizedCoeffs MöbiusTransformation{..} = map (/ firstNonzero) coeffs
    where
        firstNonzero = head (nonzero ++ [1])
        nonzero = filter (0 /=) coeffs
        coeffs = [a,b,c,d]

instance Fractional a => Eq (MöbiusTransformation a) where
    f == g  = normalizedCoeffs f == normalizedCoeffs g

instance Num a => Show (MöbiusTransformation a) where
    showsPrec p Identity = showString "identityMöbiusTransformation"
    showsPrec p MöbiusTransformation{c=0, d=1, ..}
        = showParen (p>10)
            ( showString "linearTransformation "
            . showsPrec 11 a
            . showChar ' '
            . showsPrec 11 b
            )
    showsPrec p MöbiusTransformation{..} 
        = showParen (p>10)
            ( showString "möbiusTransformation "
            . showsPrec 11 a
            . showChar ' '
            . showsPrec 11 b
            . showChar ' '
            . showsPrec 11 c
            . showChar ' '
            . showsPrec 11 d
            )

instance Num a => Monoid (MöbiusTransformation a) where
    mempty = identityMöbiusTransformation
    mappend = composeMöbiusTransformation

identityMöbiusTransformation :: MöbiusTransformation a
identityMöbiusTransformation = Identity

linearTransformation a b =
    möbiusTransformation a b 0 1

möbiusTransformation a b c d
    | b==0 && c==0 && a == d    = Identity
    | otherwise                 = MöbiusTransformation a b c d (a*d - b*c)

composeMöbiusTransformation Identity f = f
composeMöbiusTransformation f Identity = f
composeMöbiusTransformation f g
    | b==0 && c==0 && a == d    = Identity
    | otherwise                 = MöbiusTransformation{..}
    where
        MöbiusTransformation a0 b0 c0 d0 det0 = f
        MöbiusTransformation a1 b1 c1 d1 det1 = g
        
        a = a0 * a1 + b0 * c1
        b = a0 * b1 + b0 * d1
        c = c0 * a1 + d0 * c1
        d = c0 * b1 + d0 * d1
        det = det0 * det1


invertMöbiusTransformation Identity = Identity
invertMöbiusTransformation MöbiusTransformation{..}
    | det /= 0  = MöbiusTransformation d (-b) (-c) a (recip det)
    | otherwise = error "invertMöbiusTransformation: not invertible"

applyMöbiusTransformation Identity x = x
applyMöbiusTransformation MöbiusTransformation{a=0, c=0, ..} x
    = x `seq` (Real b / Real d)
applyMöbiusTransformation MöbiusTransformation{..} Infinity
    = Real a / Real c
applyMöbiusTransformation MöbiusTransformation{..} (Real x)
    | det == 0  = Real a / Real c
    | otherwise = Real p / Real q
    where
        p = a*x + b
        q = c*x + d
