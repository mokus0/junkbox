module TypeExperiments.UnsafeStableMap where

import Prelude hiding (lookup)

import Data.Functor.Identity
import System.Mem.StableName
import System.Mem.StableName.Map

-- |A simple demonstration of the fact that the stable-maps library is
-- not type-safe.  This function is behaviorally nearly identical to
-- @return unsafeCoerce@.
-- 
-- The core of the problem is that sn1 and sn2 are equal (after type erasure)
-- because they were constructed from the same closure ('undefined').
-- 
-- 'undefined' has no special role here - any polymorphic closure will work
-- (except maybe if type-classes are involved, because then the closures
-- may not be the same at different types).  In particular, the value need
-- not be \"bottom\".  'id' works just fine, for example.
unsafeCoerceIO :: IO (a -> b)
unsafeCoerceIO = do
    sn1 <- makeStableName undefined
    sn2 <- makeStableName undefined
    
    return $ \x ->
        let m = singleton sn1 (Identity x)
            Just (Identity y) = lookup sn2 m
         in y

main :: IO ()
main = do
    unsafeCoerce <- unsafeCoerceIO
    unsafeCoerce () "what did you do to my argument stack?!"
