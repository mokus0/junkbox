{-# LANGUAGE Safe #-}
-- should compile when base, prim-uniq, dependent-sum and dependent-map are trusted.
-- The latter 2 wouldn't even require trust were it not for their Typeable instances
-- which cannot be derived.
module Safe.Test2 where

import Data.Dependent.Map
import Data.Unique.Tag
import Math.ContinuedFraction

phi = (1 + sqrt 5) / 2
phiCF = cf 1 (repeat 1)

main = do
    x <- newTag
    let m = singleton x "hello world"
    print (m ! x)
    mapM_ print (take 40 (map (phi -) (convergents phiCF)))

