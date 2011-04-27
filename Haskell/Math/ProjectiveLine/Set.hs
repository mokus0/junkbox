module Math.ProjectiveLine.Set
    ( Set, valid, empty, full
    , singleton, cosingleton
    , range, closedRange, openRange, openClosed, closedOpen
    , isEmpty, isFull
    
    , member
    , contains
    , disjoint
    , neighborhood
    , boundaryPoints
    , foldRanges
    , ranges
    , singletons
    , rangesAndSingletons
    , components
    , numComponents
    , numSingletons
    , measure
    , count
    
    , complement
    , union, unions
    , intersect, intersections
    , difference, xor
    
    , closure
    ) where

import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Math.ProjectiveLine

-- Representation of subsets of the real projective line with finite numbers
-- of connected components, based on the following internal model:

-- A subset is either:
-- 
--  1) the empty set
--  2) the full projective line
--  3) a collection of boundaries, each one defined as a point that is
--     either included or excluded from the set.  Additionally, the boundary
--     specifies whether the segment immediately to that point's "right" 
--     (+ direction) is included.
--
-- The topology used is the usual topology of the real projective line; ranges
-- can "wrap around" from high to low, so the region below the lowest bound
-- is included if the region above the highest bound is.
-- The type parameter of 'Set' need not actually be @ProjectiveLine a@ unless
-- you want to be able to have boundaries at Infinity.

data Boundary = Boundary
    { pointIncluded     :: !Bool
    , segmentIncluded   :: !Bool
    } deriving (Eq, Ord, Show)

data Set a
    = Empty
    -- INVARIANT: the Map is never empty
    -- INVARIANT: every boundary is significant
    -- Together these invariants ensure that the representation of every
    -- Set is unique, so the derived Eq instance will work.
    | Set !(M.Map a Boundary)
    | Full
    deriving Eq


-- convenience constant controlling whether the Show instance will display
-- wrapped subranges as "complement (range lo hi)" or "range hi lo"
complementWrappedRanges = True

instance (Ord a, Show a) => Show (Set a) where
    showsPrec _ Empty = showString "empty"
    showsPrec _ Full  = showString "full"
    showsPrec p other
        | null bs           = panic "showsPrec" "invalid Set"
        | null (tail bs)    = showParen (p > 10) (head bs)
        | otherwise         = showParen (p > 10)
            ( showString "unions ["
            . joinShows (showChar ',') bs
            . showChar ']'
            )
        where
            joinShows sep = foldr (.) id . intersperse sep
            bs = maybe (panic "showsPrec" "toRangeList returned Nothing for a non-trivial set")
                (map showComponent)
                (rangesAndSingletons other)
            
            showComponent (Left x)
                = showString "singleton "
                . showsPrec 11 x
            showComponent (Right ((x, False), (y, False))) | x == y
                = showString "cosingleton "
                . showsPrec 11 x
            showComponent (Right r)
                = showRange r
            
            showRange ((x, xI), (y, yI))
                | complementWrappedRanges && x > y
                    = showString "complement "
                    . showParen True (showRange ((y, not yI), (x, not xI)))
                | otherwise = joinShows (showChar ' ')
                    [ showString (rangeCon xI yI)
                    , showsPrec 11 x
                    , showsPrec 11 y
                    ]
            
            rangeCon True  True  = "closedRange"
            rangeCon True  False = "closedOpen"
            rangeCon False True  = "openClosed"
            rangeCon False False = "openRange"


valid Empty = True
valid Full  = True
valid s@(Set m)
    | M.null m                              = False
    | any (not . isBoundaryIn s) (M.keys m) = False
    | otherwise                             = True

panic func reason = error (func ++ ": " ++ reason)

empty, full :: Set a
empty = Empty
full  = Full
mbSet isFull m
    | M.null m  = if isFull then Full else Empty
    | otherwise = Set m

singleton   x = Set (M.singleton x (Boundary True  False))
cosingleton x = Set (M.singleton x (Boundary False True))

range (lo, incLo) (hi, incHi)
    | lo == hi  = if incLo /= incHi
        then panic "range" "half-open range with both endpoints the same make no dadgum sense"
        else if incLo then singleton lo else empty
    | otherwise = Set $ M.fromList
        [ (lo, Boundary incLo True)
        , (hi, Boundary incHi False)
        ]

closedRange lo hi = range (lo, True)  (hi, True)
openRange   lo hi = range (lo, False) (hi, False)
openClosed  lo hi = range (lo, False) (hi, True)
closedOpen  lo hi = range (lo, True)  (hi, False)

isEmpty Empty = True
isEmpty     _ = False

isFull Full = True
isFull    _ = False

-- find the boundary affecting a point; either the one at that point
-- or the closest one below it (wrapping around if necessary).
lookupBoundary func x m = case M.splitLookup x m of
    (_, Just b, _) -> (x, b)
    (below, Nothing, above) -> case M.maxViewWithKey below of
        Just (b, _) -> b
        Nothing -> case M.maxViewWithKey above of
            Just (b, _) -> b
            Nothing -> panic func "invalid internal Set state"

member :: Ord a => a -> Set a -> Bool
member _ Empty   = False
member _ Full    = True
member x (Set m)
    | x == bX   = pointIncluded   b
    | otherwise = segmentIncluded b
    where
        (bX, b) = lookupBoundary "member" x m

-- Order of matches in these first 4 is very significant.
contains Full  _ = True
contains _  Full = False
contains _ Empty = True
contains Empty _ = False
-- TODO: this can be implemented more efficiently
contains s1 s2 = intersect s1 s2 == s2

disjoint s1 s2 = isEmpty (intersect s1 s2)
    

-- Given a point and a set this function
-- returns 3 bits describing that boundary:
--   1) whether the region immediately below the point is in the set
--   2) whether the point is in the set
--   3) whether the region immediately above the point is in the set
neighborhood :: Ord a => a -> Set a -> (Bool, Bool, Bool)
neighborhood x Full     = (True,  True,  True)
neighborhood x Empty    = (False, False, False)
neighborhood x (Set m)  
    | x == bX   = (segmentIncluded b', pointIncluded b, segmentIncluded b)
    | otherwise = cnst3 (segmentIncluded b)
    where
        (bX,  b)  = lookupBoundary "neighborhood" x m
        
        m' = M.delete bX m
        (bX', b')
            | M.null m' = (bX, b)
            | otherwise = lookupBoundary "neighborhood" x m'
        
        cnst3 z = (z,z,z)

mergeNeigborhood op (a,b,c) (d,e,f)
    = (op a d, op b e, op c f)

isBoundaryIn s x = neighborhoodIsBoundary (neighborhood x s)
neighborhoodIsBoundary (a,b,c) = a /= b || b /= c

boundaryPoints Empty = []
boundaryPoints Full  = []
boundaryPoints s@(Set m) = filter (isBoundaryIn s) (M.keys m)

-- Nothing indicates 'isFull'.
-- If there is a range which wraps around from high to low, that range appears first.
ranges :: Set t -> Maybe [((t, Bool), (t, Bool))]
ranges = foldRanges (Just []) Nothing mkSet s r []
    where
        mkSet True  rs = Just (last rs : init rs)
        mkSet False rs = Just rs
        
        s x             = id
        r x xI y yI     = (:) ((x,   xI), (y,   yI))

singletons :: Set t -> [t]
singletons = foldRanges [] [] (const id) s r []
    where
        s x             = (x :)
        r x xI y yI     = id

-- Nothing indicates 'isFull'.
-- If there is a range which wraps around from high to low, that range appears first.
-- If a range has duplicate endpoints, it represents a "cosingleton" - an open set
-- starting at that point and extending up through the entire set and back to that point.
rangesAndSingletons :: Set t -> Maybe [Either t ((t, Bool), (t, Bool))]
rangesAndSingletons = foldRanges (Just []) Nothing mkSet s r []
    where
        mkSet True  rs = Just (last rs : init rs)
        mkSet False rs = Just rs
        s x             = (:) (Left x)
        r x xI y yI     = (:) (Right ((x, xI), (y, yI)))

components :: Ord t => Set t -> [Set t]
components = foldRanges [empty] [full] (const id) s r []
    where
        s x             = (singleton x           :)
        r x xI y yI     = (range (x, xI) (y, yI) :)
        

numComponents :: Set t -> Int
numComponents = numComponentsWhere (const True)

numSingletons :: Set t -> Int
numSingletons = numComponentsWhere isSingleton
    where
        isSingleton Left{}  = True
        isSingleton _       = False

numComponentsWhere p = foldRanges 0 0 (const id) s r 0
    where
        s x
            | p (Left x)                    = (1+)
            | otherwise                     = id
        r x xI y yI
            | p (Right ((x,xI), (y,yI)))    = (1 +)
            | otherwise                     = id

-- volume of reals in set
measure :: (Num t, Ord t) => Set (ProjectiveLine t) -> (ProjectiveLine t)
measure = measureBy s r (+) 0 Infinity
    where
        s _         = 0
        r x _ y _
            | x < y     = y - x
            | otherwise = Infinity

-- number of integers in set
count :: (Integral t) => Set (ProjectiveLine t) -> (ProjectiveLine t)
count = measureBy s r (+) 0 Infinity
    where
        s _         = 1
        r x xI y yI
            | x < y     = y - x 
                + (if xI then 0 else -1)
                + (if yI then 1 else 0)
            | otherwise = Infinity

measureBy s r (+) zero inf = foldRanges zero inf (const id) consS consR zero
    where
        consS x         = (+) (s x)
        consR x iX y iY = (+) (r x iX y iY)

foldRanges empty full set sCons rCons nil Empty = empty
foldRanges empty full set sCons rCons nil Full  = full
foldRanges empty full set sCons rCons nil (Set m) 
    | M.null m  = panic "foldRanges" "invalid internal Set state"
    | otherwise = uncurry set (foldRangeList sCons rCons nil (M.toAscList m))

foldRangeList consSingleton consRange nil [(x,b)]
    | segmentIncluded b = (True,  consRange x (pointIncluded b) x (pointIncluded b) nil)
    | pointIncluded   b = (False, consSingleton x nil)
    | otherwise         = panic "foldRanges" "invalid internal Set state"
foldRangeList consSingleton consRange nil bs@(b0:_) = (wraps, folded)
    where
        (wraps, folded) = loop wraps bs
        
        loop emittedX []            = (emittedX, nil)
        loop emittedX ((x,bX):rest) = fmap f (loop isRange rest)
            where
                f  | isRange               = consRange x xI y yI
                   | xI && not emittedX    = consSingleton x
                   | otherwise             = id
                isRange = segmentIncluded bX
                
                -- Endpoint of current range; wrap around if 'rest' is empty
                (y,bY)
                    | null rest = b0
                    | otherwise = head rest
                
                xI = pointIncluded bX
                yI = pointIncluded bY

complement Empty    = Full
complement Full     = Empty
complement (Set m)  = Set (fmap f m)
    where
        f (Boundary x y) = Boundary (not x) (not y)

normalize func Empty   = Empty
normalize func Full    = Full
normalize func s@(Set m)
    | M.null m  = panic func "invalid internal Set state"
    | otherwise = mbSet (0 `member` s) (filterKeys (isBoundaryIn s) m)
    where
        filterKeys f = M.filterWithKey (const . f)

union x y = union' (normalize "union (1st arg)" x) (normalize "union (2nd arg)" y)

union' Empty x = x
union' x Empty = x
union' Full _ = Full
union' _ Full = Full
union' s1@(Set m1) s2@(Set m2) = mbSet True $ M.fromAscList
    [ (x, Boundary p s)
    | x <- S.toAscList (S.union (M.keysSet m1) (M.keysSet m2))
    , let n@(_,p,s) = mergeNeigborhood (||) (neighborhood x s1) (neighborhood x s2)
    , neighborhoodIsBoundary n
    ]

unions ss = foldl' union Empty ss

intersect x y = intersect' (normalize "intersect (1st arg)" x) (normalize "intersect (2nd arg)" y)

intersect' Empty _ = Empty
intersect' _ Empty = Empty
intersect' Full x = x
intersect' x Full = x
intersect' s1@(Set m1) s2@(Set m2) = mbSet False $ M.fromAscList
    [ (x, Boundary p s)
    | x <- S.toAscList (S.union (M.keysSet m1) (M.keysSet m2))
    , let n@(_,p,s) = mergeNeigborhood (&&) (neighborhood x s1) (neighborhood x s2)
    , neighborhoodIsBoundary n
    ]

intersections ss = complement (unions (map complement ss))

difference x y = intersect x (complement y)

xor x y = union x y `difference` intersect x y

closure Empty   = Empty
closure Full    = Full
closure (Set m) 
    = normalize "closure" 
    . mbSet True
    $ fmap (\b -> b {pointIncluded = True}) m
