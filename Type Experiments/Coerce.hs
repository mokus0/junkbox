{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
{-
 -	"Coerce.hs"
 -	(c) 2008 James Cook
 -}

module Coerce where

-- class Coerce a b c | a b -> c where
-- 	coerce :: a -> b -> (c,c)
-- 
-- instance (Integral a) => Coerce a Integer Integer
-- 	where
-- 		coerce x y = (toInteger x, y)
-- 
-- instance (Integral a) => Coerce Integer a Integer
-- 	where
-- 		coerce x y = (x, toInteger y)
-- 
-- instance Coerce a a a where	-- arr... i don't like the way ghc resolves (fails to resolve, that is) these...
-- 	coerce = (,)
-- 

class Coercible a b where
    coerce :: Either a b -> Either a b -> Either (a, a) (b, b)

instance Coercible Float Double where
    coerce (Left x)  (Left y)  = Left  (x,y)
    coerce (Right x) (Right y) = Right (x,y)
    
    coerce (Left f)  (Right d) = Right (realToFrac f, d)
    coerce (Right d) (Left f)  = Right (d, realToFrac f)

instance Coercible Double Float where
    coerce (Left x)  (Left y)  = Left  (x,y)
    coerce (Right x) (Right y) = Right (x,y)
    
    coerce (Left d)  (Right f) = Left  (d, realToFrac f)
    coerce (Right f) (Left d)  = Left  (realToFrac f, d)

liftCoerce (+) (*) x y = emap (uncurry (+)) (uncurry (*)) (coerce x y)

emap f g = either (Left . f)  (Right . g)

instance (Num a, Num b, Coercible a b) => Num (Either a b) where
    (+) = liftCoerce (+) (+)
    (-) = liftCoerce (-) (-)
    (*) = liftCoerce (*) (*)
    negate = emap negate negate
    abs = emap abs abs
    signum = emap signum signum
    fromInteger = Right . fromInteger

instance Coercible (Either a b) (Either b a) where
    coerce (Left x)  (Left y)  = Left  (x,y)
    coerce (Right x) (Right y) = Right (x,y)
    
    coerce (Left ab) (Right ba) = Right (either Right Left ab, ba)
    coerce (Right ba) (Left ab) = Right (ba, either Right Left ab)

instance Coercible (Either a (Either b c)) (Either (Either a b) c)

