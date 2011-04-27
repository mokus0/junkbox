-- |I'm rather surprised that M.filter* require an Ord instance.
-- does having one somehow enable a more efficient implementation 
-- than this?  Sounds like some testing is in order.
--
-- The Data.Map implementations are significantly faster than these via-list
-- ones.  Whether that's due to differences in GC activity (from list creation)
-- or a genuinely better algorithm I do not know.  It certainly seems, though,
-- that the Ord instance is superfluous.
module Benchmarks.MapFilters where

import Control.Exception (evaluate)
import Control.DeepSeq (NFData(..))
import qualified Data.Map as M
import Criterion.Main

testMap :: M.Map Int Int
testMap = M.fromList (zip [1..1000] [1000,998..])

testMapWithoutOrd :: M.Map (OrdLess Int) Int
testMapWithoutOrd = M.mapKeysMonotonic OrdLess testMap

filter' :: (v -> Bool) -> M.Map k v -> M.Map k v
filter' f = M.fromDistinctAscList . filter (f . snd) . M.toAscList

filterWithKey' :: (k -> v -> Bool) -> M.Map k v -> M.Map k v
filterWithKey' f = M.fromDistinctAscList . filter (uncurry f) . M.toAscList

filterKeys1 f = M.filterWithKey (const . f)
filterKeys2 f = M.fromDistinctAscList . filter (f . fst) . M.toAscList
filterKeys3 f = M.filterWithKey (\(OrdLess k) _ -> f k)

main = do
    evaluate (rnf testMap)
    evaluate (rnf testMapWithoutOrd)
    defaultMain
        [ bgroup "builtins"
            [ bench "filter"        (nf (M.filter even) testMap)
            , bench "filterWithKey" (nf (M.filterWithKey (\k v -> even k || odd v)) testMap)
            ]
        , bgroup "builtins with Ord explicitly forgotten"
            [ bench "filter"        (nf (M.filter even) testMapWithoutOrd)
            , bench "filterWithKey" (nf (M.filterWithKey (\(OrdLess k) v -> even k || odd v)) testMapWithoutOrd)
            ]
        , bgroup "custom" 
            [ bench "filter"        (nf (filter' even) testMap)
            , bench "filterWithKey" (nf (filterWithKey' (\k v -> even k || odd v)) testMap)
            , bench "filterKeys1"   (nf (filterKeys1 even) testMap)
            , bench "filterKeys2"   (nf (filterKeys2 even) testMap)
            , bench "filterKeys3"   (nf (filterKeys3 even) testMapWithoutOrd)
            ]
        ]

-- Here's a silly experiment:  are the built-in ones even using the Ord instance?
-- 
-- The answer?  No.  They do not use it.
-- So why do they require it?  "Historical reasons"?
newtype OrdLess a = OrdLess { fromOrdLess :: a } deriving Show
no func = error (unwords ["no", func, "for you!"])
instance Eq (OrdLess a) where
    (==) = no "=="
    (/=) = no "/="
instance Ord (OrdLess a) where
    compare = no "compare"
    (>)  = no ">"
    (>=) = no ">="
    (<)  = no "<"
    (<=) = no "<="
instance NFData a => NFData (OrdLess a) where
    rnf (OrdLess a) = rnf a

withoutOrd f = M.mapKeysMonotonic fromOrdLess . f . M.mapKeysMonotonic OrdLess

filterWithoutOrd        f = withoutOrd (M.filter f)
filterWithKeyWithoutOrd f = withoutOrd (M.filterWithKey (f . fromOrdLess))

-- wrap mapKeys as well, just to make sure things will fail as expected
mapKeysWithoutOrd       f = withoutOrd (M.mapKeys (OrdLess . f . fromOrdLess))
