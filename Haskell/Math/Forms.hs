{-# LANGUAGE GADTs, 
             MultiParamTypeClasses, 
             FunctionalDependencies, 
             FlexibleInstances, 
             TypeFamilies, 
             FlexibleContexts, 
             UndecidableInstances,
             OverlappingInstances,
             IncoherentInstances,
             TypeOperators
  #-}
module Math.Forms
    ( Form(..)
    , LF, idLF, constLF, lf
    , MF, idMF, constMF, mfCons
    , linearMF, bilinearMF, trilinearMF
    ) where

import TypeExperiments.Curry2
import Data.VectorSpace
import Unsafe.Coerce

class Form f a b | f a -> b where
    eval :: f a -> a -> b

-- linear form: LF a b ~ ax+b
data LF a = LF !a !a
    deriving (Eq, Ord, Show)

instance Num a => Form LF a a where
    eval (LF a b) x = a*x + b

instance AdditiveGroup a => AdditiveGroup (LF a) where
    zeroV = LF zeroV zeroV
    negateV (LF a b) = LF (negateV a) (negateV b)
    LF a b ^+^ LF c d   = LF (a ^+^ c) (b ^+^ d)

instance VectorSpace a => VectorSpace (LF a) where
    type Scalar (LF a) = Scalar a
    s *^ LF a b = LF (s *^ a) (s *^ b)

idLF :: Num a => LF a
idLF = LF 1 0

constLF b = lf 0 b

lf a b = LF a b

-- multilinear form
data MF a t where
    MFNil  :: !a -> MF a ()                     -- no variables
    MFCons :: ConsTuple a t1 t2
           => !(LF (MF a t1))  -> MF a t2       -- add a variable

---- Not sure these instances, or anything like them, can be made to work in
---- the presence of so much type hackery:
--instance (AdditiveGroup a) => AdditiveGroup (MF a t) where
--    zeroV = undefined
--    negateV (MFNil a) = MFNil (negateV a)
--    
--    MFNil  a ^+^ MFNil  b   = MFNil  (a ^+^ b)
--    MFCons a ^+^ MFCons b   = MFCons (a ^+^ b)
--
--instance (VectorSpace a) => VectorSpace (MF a t) where
--    type Scalar (MF a ()) = Scalar a
--    
--    s *^ MFNil  a = MFNil  (s *^ a)
--    s *^ MFCons a = MFCons (s *^ a)

instance Eq a => Eq (MF a t) where
    MFNil  x == MFNil  y = x == y

    -- MFCons x == MFCons y = x == y
        -- ^ doesn't typecheck, but I'm pretty sure it should because of the fundeps on ConsTuple
    MFCons x == MFCons y = x == unsafeCoerce y
        -- tried quite a few things to "trick" the type system, but never managed it.  So now I'm gonna tell it I'm right (I hope I am ;)).

instance Show a => Show (MF a t) where
    showsPrec p (MFNil a) = showParen (p > 10)
        ( showString "MFNil "
        . showsPrec 11 a
        )
    showsPrec p (MFCons a) = showParen (p > 10)
        ( showString "MFCons "
        . showsPrec 11 a
        )

instance Num a => Form (MF a) t a where
    eval (MFNil a) () = a
    eval (MFCons (LF a b)) t1 = eval (lf (eval a t2) (eval b t2)) x
        where (x, t2) = unConsTuple t1

mfCons a b = MFCons (LF a b)

constMF a                   = MFNil a
linearMF a b                = mfCons (constMF a)          (constMF b)
bilinearMF a b c d          = mfCons (linearMF a b)       (linearMF c d)
trilinearMF a b c d e f g h = mfCons (bilinearMF a b c d) (bilinearMF e f g h)

idMF :: Num a => MF a (One a)
idMF = linearMF 1 0

-- rational form
data RF f a = RF !(f a) !(f a)
    deriving (Eq, Ord)

instance (Form f a b, Fractional b) => Form (RF f) a b where
    eval (RF p q) x = eval p x / eval q x

p // q = RF p q
infixl 7 //

instance Show (f a) => Show (RF f a) where
    showsPrec prec (RF p q) = showParen (prec > 7)
        ( showsPrec 7 p
        . showString " // "
        . showsPrec 8 q
        )

data Variance
    = Antitone
    | Constant
    | Monotone
    deriving (Eq, Ord, Enum, Read, Show)

lfVariance (LF a b) = case a `compare` 0 of
    LT -> Antitone
    EQ -> Constant
    GT -> Monotone

-- d/dx (ax+b)(cx+d)^-1
-- = (ax+b)(d/dx (cx+d)^-1) + (d/dx (ax+b))(cx+d)^-1
-- = (ax+b)(-(cx+d)^-1(d/dx (cx+d))) + (a)(cx+d)^-1
-- = (ax+b)(-c(cx+d)^-2) + (a)(cx+d)^-1
-- = (a)(cx+d)^-1 - (ax+b)(c(cx+d)^-2)
-- = (a)(cx+d)^-1 - (acx+bc)((cx+d)^-2)
-- = a / (cx+d) - (acx + bc)/(cx+d)^2
-- = ((acx + ad) - (acx + bc))/(cx+d)^2
-- = (ad - bc)/(cx+d)^2

rlfVariance (RF (LF a b) (LF c d))
    = case (a*d) `compare` (b*c) of
        LT -> Antitone
        EQ -> Constant
        GT -> Monotone
