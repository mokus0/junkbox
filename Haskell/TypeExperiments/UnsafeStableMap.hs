module TypeExperiments.UnsafeStableMap where

import Prelude hiding (lookup)

import System.Mem.StableName
import System.Mem.StableName.Map

-- |A simple demonstration of the fact that the stable-maps library is
-- not type-safe.  This function is behaviorally nearly identical to
-- @return . unsafeCoerce@.
-- 
-- The core of the problem is that sn1 and sn2 are equal (after type erasure)
-- because they were constructed from the same closure ('undefined').
-- 
-- 'undefined' has no special role here - any polymorphic closure will work
-- (except maybe if type-classes are involved, because then the closures
-- may not be the same at different types).  In particular, the value need
-- not be \"bottom\".  'id' works just fine, for example.
unsafeCoerceIO :: a -> IO b
unsafeCoerceIO x = do
    sn1 <- makeStableName undefined
    sn2 <- makeStableName undefined
    
    let m = singleton sn1 (Just x)
        Just (Just y) = lookup sn2 m
    
    return y


main :: IO ()
main = do
    f <- unsafeCoerceIO ()
    f "what did you do to my argument stack?!"
