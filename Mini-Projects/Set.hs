module Set where

import Data.List

class (Enum a, Bounded a, Eq a) => Attribute a

allValues :: Attribute a => [a]
allValues = [minBound .. maxBound]

enumFromZero :: (Enum a) => [a]
enumFromZero = enumFrom (toEnum 0)

data Number = Single | Double | Triple
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Attribute Number

data Color = Red | Green | Blue
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Attribute Color

data Shading = Open | Shaded | Solid
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Attribute Shading

data Shape = Oval | Diamond | Squiggle
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Attribute Shape

data Card = Card Number Color Shading Shape
	deriving (Eq, Ord, Read, Show)
instance Enum Card where
    toEnum n
        | r == 0    = Card (toEnum num) (toEnum col) (toEnum shd) (toEnum shp)
        | otherwise = error ("toEnum: value out of range (" ++ show n ++ ")")
        where
            (r,  num) = quotRem r3 3
            (r3, col) = quotRem r2 3
            (r2, shd) = quotRem r1 3
            (r1, shp) = quotRem n 3
    fromEnum (Card num col shd shp) = sum $ zipWith (*) (iterate (*3) 1)
        [fromEnum shp, fromEnum shd, fromEnum col, fromEnum num]
instance Bounded Card where
    minBound = toEnum 0
    maxBound = toEnum 80

isSetAttr a b c = allEq (map (uncurry (==)) $ pairs [a,b,c])
mkSetAttr a b = if (a == b) then [a] else allValues \\ [a,b]

isSet a b c = elem a (mkSet b c)
mkSet (Card a1 b1 c1 d1) (Card a2 b2 c2 d2) =
	[Card a b c d | a <- mkSetAttr a1 a2, b <- mkSetAttr b1 b2, c <- mkSetAttr c1 c2, d <- mkSetAttr d1 d2]

choose :: (Num n) => n -> [a] -> [[a]]
choose 0 _ = []
choose _ [] = []
choose 1 list = map (: []) list
choose n (x:xs) = (map (x :) (choose (n-1) xs)) ++ (choose n xs)

pairs :: [a] -> [(a,a)]
pairs list = map (\(x:y:_) -> (x,y)) (choose 2 list)

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

fullDeck :: [Card]
fullDeck = [minBound .. maxBound]

sets list = filter (\[a,b,c] -> isSet a b c) $ choose 3 list

noSet17 = 
    [ Card Single Red Open Oval
    , Card Single Red Shaded Oval
    , Card Single Red Solid Squiggle
    , Card Single Blue Open Diamond
    , Card Single Blue Shaded Squiggle
    , Card Double Red Open Squiggle
    , Card Double Red Solid Diamond
    , Card Double Green Open Squiggle
    , Card Double Green Solid Diamond
    , Card Double Blue Open Oval
    , Card Double Blue Open Diamond
    , Card Double Blue Solid Oval
    , Card Triple Green Open Diamond
    , Card Triple Green Shaded Squiggle
    , Card Triple Blue Open Oval
    , Card Triple Blue Shaded Oval
    , Card Triple Blue Solid Squiggle
    ]
