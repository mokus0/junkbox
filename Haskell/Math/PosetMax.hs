module Math.PosetMax where
import Data.List (nubBy, genericLength, sortBy)
import Data.Ord (comparing)

type PartialOrdering a = a -> a -> Maybe Ordering

lub cmp xs = case maximaBy cmp xs of
    [x] -> Just x
    _   -> Nothing

-- set of maximal elements of a poset.
maximaBy cmp xs = loop xs (empty cmp)
    where
        loop [] (MutuallyIncomparable _ maxes) = maxes
        loop (x:xs) maxes = loop xs (insertMax x maxes)

minimaBy cmp xs = maximaBy (flip cmp) xs

-- take the maximaBy and compute a 'bestness' score - the fraction of all the
-- alternatives that each maximal element dominates, with "best" first.
bestness cmp xs = sortBy (flip (comparing snd))
    [(x, genericLength (ideal cmp x xs) / n) | x <- maximaBy cmp xs]
    where
        n = genericLength xs

-- maximal subsets with well-defined LUB
maximalIdeals cmp xs = [filter (gte cmp x) xs | x <- maximaBy cmp xs]

-- maximal subset with x as LUB
ideal cmp x xs = x : filter (gt cmp x) xs

data MutuallyIncomparable a = MutuallyIncomparable !(PartialOrdering a) [a]

empty :: PartialOrdering a -> MutuallyIncomparable a
empty cmp = MutuallyIncomparable cmp []

-- Try to add a new element to a MutuallyIncomparable set, preferring larger 
-- ones.  5 possible cases, assuming the PartialOrdering is valid and the 
-- existing elements are mutually incomparable:
-- 
-- 0. The set is empty.  Add the element.
-- 
-- 1. The element is mutually incomparable to every element of the set. Add it.
-- 2. It is LT one or more elements of the set.  Don't add it.
-- 3. It is EQ one element of the set.  Combine it.
-- 4. It is GT one or more elements of the set.  Add it and remove those elements.

insertMaxWith combine x0 (MutuallyIncomparable cmp ys0)  = MutuallyIncomparable cmp (ins x0 ys0)
    where
        ins x [] = [x]
        ins x (y:ys) = case cmp x y of
            Nothing -> y : ins x ys
            Just LT -> y : ys
            Just EQ -> combine x y : ys
            Just GT -> x : del x ys
            
        del x ys = filter (\y -> nlte cmp y x) ys

insertMax :: a -> MutuallyIncomparable a -> MutuallyIncomparable a
insertMax = insertMaxWith const

-- exactly the same as insertMaxWith, but with opposite ordering.
insertMinWith combine x0 (MutuallyIncomparable cmp ys0) =
    case insertMaxWith combine x0 (MutuallyIncomparable (flip cmp) ys0) of
        ~(MutuallyIncomparable _ ys) -> MutuallyIncomparable cmp ys

insertMin :: a -> MutuallyIncomparable a -> MutuallyIncomparable a
insertMin = insertMinWith const

-- The original motivation for all this: Given a set of attributes, each of
-- which independently can be ordered, I want to select every object which
-- _might_ be the best in the set, without actually prioritizing the 
-- attributes.
-- 
-- So, we have a partial order I call "unambiguously better", where one 
-- object is unambiguously better than another if it is better under all 
-- possible prioritizations of the attributes.  Or equivalently, if it is 
-- better on any attribute _and_ equal or better on every other.
-- 
-- This is that partial order for pairs.
cmpPair (a,b) (c,d)
    | o1 == o2  = Just o1
    | o1 == EQ  = Just o2
    | o2 == EQ  = Just o1
    | otherwise = Nothing
    where
        o1 = compare a c
        o2 = compare b d

bestPairs :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
bestPairs = maximaBy cmpPair

nc cmp x y = cmp x y == Nothing
lt cmp x y = cmp x y == Just LT
eq cmp x y = cmp x y == Just EQ
gt cmp x y = cmp x y == Just GT

nnc cmp x y = cmp x y /= Nothing
nlt cmp x y = cmp x y /= Just LT
neq cmp x y = cmp x y /= Just EQ
ngt cmp x y = cmp x y /= Just GT

lte cmp x y = lt cmp x y || eq cmp x y
gte cmp x y = gt cmp x y || eq cmp x y

nlte cmp x y = nlt cmp x y && neq cmp x y
ngte cmp x y = ngt cmp x y && neq cmp x y
