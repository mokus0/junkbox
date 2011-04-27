module Math.ProjectiveLine.Set
    ( Set, valid, empty, full
    , singleton, cosingleton
    , range, closedRange, openRange
    , isEmpty, isFull
    
    , member, neighborhood
    , boundaryPoints
    , foldRanges, toRangeList
    , components
    , numComponents
    , measure
    
    , complement
    , union, unions
    , intersect, intersections
    , difference, xor
    
    , closure
    ) where

import Data.List (foldl', intersperse)
import qualified Data.Map as M
import qualified Data.Set as S

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
-- The topology used is the usual topology of the real projective line.
-- The type parameter of 'Set' need not actually be @ProjectiveLine a@ unless
-- you want to be able to have boundaries at Infinity.

data Boundary = Boundary
    { pointIncluded     :: !Bool
    , segmentIncluded   :: !Bool
    } deriving (Eq, Ord, Show)

data Set a
    = Empty
    | Full
    -- INVARIANT: the Map is never empty
    -- INVARIANT: every boundary is significant
    | Set !(M.Map a Boundary)

instance (Eq a, Show a) => Show (Set a) where
    showsPrec _ Empty = showString "empty"
    showsPrec _ Full  = showString "full"
    showsPrec p other
        | null bs           = panic "showsPrec" "invalid Set"
        | null (tail bs)    = head bs
        | otherwise         = showParen (p > 10)
            ( showString "unions ["
            . foldr (.) id (intersperse (showChar ',') bs)
            . showChar ']'
            )
        where
            bs = 
                [ if x0 == x1
                    then if inc0
                        then showString "singleton "   . showsPrec 11 x0
                        else showString "cosingleton " . showsPrec 11 x0
                    else case (inc0,inc1) of
                        (True,True) -> showString "closedRange "
                            . showsPrec 11 x0
                            . showChar ' '
                            . showsPrec 11 x1
                        (False,False) -> showString "openRange "
                            . showsPrec 11 x0
                            . showChar ' '
                            . showsPrec 11 x1
                        _  -> showString "range " 
                            . showsPrec 11 ep0
                            . showChar ' '
                            . showsPrec 11 ep1
                        
                | (ep0@(x0, inc0), ep1@(x1, inc1)) <- 
                    maybe (panic "showsPrec" "toRangeList returned Nothing for a non-trivial set") id 
                        (toRangeList other)
                ]


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

toRangeList :: Set t -> Maybe [((t, Bool), (t, Bool))]
toRangeList = foldRanges Nothing Nothing s r [] Just
    where
        s x                 = (:) ((x, True), (x, True))
        r x0 inc0 x1 inc1   = (:) ((x0,inc0), (x1,inc1))

components :: Ord t => Set t -> [Set t]
components = foldRanges [empty] [full] s r [] id
    where
        s x               = (singleton x                 :)
        r x0 inc0 x1 inc1 = (range (x0, inc0) (x1, inc1) :)
        

numComponents :: Set t -> Int
numComponents = foldRanges 0 0 s r 0 id
    where
        s _       = (1 +)
        r _ _ _ _ = (1 +)

measure :: Num t => Set t -> t
measure = measureBy id 0 (+) (-)

measureBy s zero (+) (-) = foldRanges zero zero f g zero id
    where
        f x       = (+) (s x)
        g x _ y _ = (+) (y-x)

foldRanges e f s r z rs Empty = e
foldRanges e f s r z rs Full  = f
foldRanges e f s r z rs (Set m) 
    | M.null m  = panic "foldRanges" "invalid internal Set state"
    | otherwise = rs (foldRangeList s r z (M.toAscList m))

foldRangeList s r z bs@(b0:_) = loop False bs
    where
        loop _ [] = z
        loop emittedX ((x,b):rest)
            | segmentIncluded b = 
                let (x',b') = case rest of
                        []      -> b0
                        xb:_    -> xb
                 in r x (pointIncluded b) x' (pointIncluded b') (loop True rest)
            | pointIncluded b && not emittedX
                = s x (loop False rest)
            | otherwise         = loop False rest

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
    