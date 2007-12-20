module Set where

import Data.List

class EnumAll a where
	allValues :: [a]

class (EnumAll a, Eq a) => Attribute a

enumFromZero :: (Enum a) => [a]
enumFromZero = enumFrom (toEnum 0)

data Number = Single | Double | Triple
	deriving (Eq, Read, Show, Enum)
instance EnumAll Number where
	allValues = enumFromZero
instance Attribute Number

data Color = Red | Green | Blue
	deriving (Eq, Read, Show, Enum)
instance EnumAll Color where
	allValues = enumFromZero
instance Attribute Color

data Shading = Open | Shaded | Solid
	deriving (Eq, Read, Show, Enum)
instance EnumAll Shading where
	allValues = enumFromZero
instance Attribute Shading

data Shape = Oval | Diamond | Squiggle
	deriving (Eq, Read, Show, Enum)
instance EnumAll Shape where
	allValues = enumFromZero
instance Attribute Shape

data Card = Card Number Color Shading Shape
	deriving (Eq, Read, Show)
instance EnumAll Card where
	allValues = fullDeck

class SetTesting a where
	isSet :: a -> a -> a -> Bool
	mkSet :: a -> a -> [a]
	
instance (Attribute a) =>  SetTesting a
	where
		isSet a b c = allEq (map (uncurry (==)) $ pairs [a,b,c])
		mkSet a b = if (a == b) then [a] else allValues \\ [a,b]

instance SetTesting Card
	where
		isSet a b c = elem a (mkSet b c)
		mkSet (Card a1 b1 c1 d1) (Card a2 b2 c2 d2) =
			[Card a b c d | a <- mkSet a1 a2, b <- mkSet b1 b2, c <- mkSet c1 c2, d <- mkSet d1 d2]

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
fullDeck = [Card n c s1 s2 | n <- allValues, c <- allValues, s1 <- allValues, s2 <- allValues]

sets :: (SetTesting a) => [a] -> [[a]]
sets list = filter (\[a,b,c] -> isSet a b c) $ choose 3 list

noSet18 = [
	Card Single Red Open Oval,
	Card Single Red Solid Squiggle,
	Card Triple Blue Solid Squiggle,
	Card Triple Blue Open Oval,
	Card Single Blue Open Diamond, 
	Card Triple Green Open Diamond, 
	Card Single Blue Shaded Squiggle,
	Card Single Blue Shaded Squiggle, 
	Card Triple Green Shaded Squiggle, 
	Card Double Green Solid Diamond, 
	Card Double Green Open Squiggle, 
	Card Double Blue Solid Oval, 
	Card Double Blue Open Diamond, 
	Card Double Red Solid Diamond,
	Card Double Red Open Squiggle,
	Card Double Blue Open Oval, 
	Card Single Red Shaded Oval, 
	Card Triple Blue Shaded Oval
	]