{-
 -      ``NumberViews''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        ViewPatterns
  #-}

module NumberViews where

data FloatValue a = NaN | NegativeZero | Infinity | NegativeInfinity | RealFloat a

floatValue x
        | isNaN x               = NaN
        | isNegativeZero x      = NegativeZero
        | isInfinite x          = if x > 0 then Infinity else NegativeInfinity
        | otherwise             = RealFloat x

data FloatNormalization = Denormalized | Normalized

normalization x
        | isDenormalized x      = Denormalized
        | otherwise             = Normalized