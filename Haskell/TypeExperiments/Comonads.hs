{-# LANGUAGE GADTs, RankNTypes #-}
module TypeExperiments.Comonads where

import Control.Applicative
import Control.Monad
import Control.Comonad
import Control.Monad.Reader (Reader(..), ask, local)
import Data.Monoid
import qualified Data.Set as S
import Data.List
import Text.Regex

instance Monoid r => Copointed (Reader r) where
    extract (Reader f) = f mempty

instance Monoid r => Comonad (Reader r) where
    -- is there a "correct" order for the mappend here?
    -- the (r ->) instance in Control.Comonad uses this order - is it forced to, or
    -- is it just what they chose?
    duplicate (Reader f) = Reader (\x -> Reader (\y -> f (mappend x y)))


data Named a = Named {name :: String, value :: a} deriving (Eq, Show)

instance Functor Named where
    fmap f thing = thing {value = f (value thing)}

instance Copointed Named where
    extract = value

instance Comonad Named where
    extend f n = Named {name = name n, value = f n}

data PointedSet a where
    -- explicit constructors for fmap and join to defer the "Ord" to
    -- a time when it can be realized.
    FMap :: (a -> b) -> PointedSet a -> PointedSet b
    Join :: PointedSet (PointedSet a) -> PointedSet a
    
    -- base case; ensures values can only be constructed out of
    -- types with Ord instances
    MkSet :: (Ord a, Show a) => a -> S.Set a -> PointedSet a

instance (Bounded a, Enum a, Ord b) => Ord (a -> b) where
    compare x y = compare (trace x) (trace y)
        where
            trace f = [f z | z <- [minBound .. maxBound]]

instance (Bounded a, Enum a, Eq b) => Eq (a -> b) where
    x == y = trace x == trace y
        where
            trace f = [f z | z <- [minBound .. maxBound]]

examine x = examinePrec 0 x ""

examinePrec :: Int -> PointedSet a -> ShowS
examinePrec p (FMap f s) = showParen (p > 10)
    ( showString "FMap <fun> "
    . examinePrec 11 s
    )
examinePrec p (Join ss) = showParen (p > 10)
    ( showString "Join "
    . examinePrec 11 ss
    )
examinePrec p (MkSet pt s) = showParen (p > 10)
    ( showString "MkSet "
    . showsPrec 11 pt
    . showChar ' '
    . showsPrec 11 (S.toAscList s)
    )


instance (Show a, Ord a) => Show (PointedSet a) where
    showsPrec p s = showParen (p > 10) 
        ( showString "MkSet "
        . showsPrec 11 (extract s)
        . showChar ' '
        . showsPrec 11 (extractSet s)
        )

pointedSet p s = MkSet p (S.fromList s)

toSet s = S.fromList (toSetAsList s)

extractSet :: Ord a => PointedSet a -> S.Set a
extractSet s = S.fromList (extractSetAsList s)

extractSetAsList :: PointedSet a -> [a]
extractSetAsList (MkSet p s) = S.toAscList s
extractSetAsList (FMap f s) = map f (extractSetAsList s)
extractSetAsList (Join ss) = extractSetAsList (extract ss) ++ concatMap toSetAsList (extractSetAsList ss)

toSetAsList :: PointedSet a -> [a]
toSetAsList ps = extract ps : extractSetAsList ps

instance Ord a => Eq (PointedSet a) where
    x == y = (extract x == extract y) && (toSet x == toSet y)

instance Ord a => Ord (PointedSet a) where
    compare x y = compare (extract x, toSet x) (extract y, toSet y)

instance Functor PointedSet where
    fmap  = FMap

instance Pointed PointedSet where
    point x = FMap (const x) (MkSet () S.empty)

instance Monad PointedSet where
    return = point
    x >>= f = Join (fmap f x)

instance Applicative PointedSet where
    pure = point
    (<*>) = ap

instance Copointed PointedSet where
    extract (FMap f s) = f (extract s)
    extract (MkSet p _)= p
    extract (Join ss) = extract (extract ss)

focus s = S.fromList
    [ MkSet x (S.fromAscList xs)
    | (x,xs) <- select (S.toAscList s)
    ]

select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

splits xs = zip (inits xs) (tails xs)

instance Comonad PointedSet where
    duplicate (FMap f s) = FMap (FMap f) (duplicate s)
    duplicate ps@(MkSet p s) = MkSet ps (S.map (\x -> MkSet x (toSet ps S.\\ S.singleton x)) s)

data PointedList a = PointedList a [a]
    deriving (Eq, Ord, Show)

fromList (x:xs) = PointedList x xs
toList (PointedList x xs) = x:xs

instance Functor PointedList where
    fmap f (PointedList x xs) = PointedList (f x) (map f xs)

instance Pointed PointedList where
    point x = PointedList x []

instance Copointed PointedList where
    extract (PointedList x _) = x

instance Comonad PointedList where
    duplicate pl@(PointedList x xs) = PointedList pl [PointedList y ys | y:ys <- tails xs]

instance Monad PointedList where
    return = point
    PointedList x xs >>= f = case (x:xs) >>= (toList.f) of (y:ys) -> PointedList y ys

liftPointedList f (PointedList x xs) = case f (x:xs) of (y:ys) -> PointedList y ys

-- a very sketchy idea i had -- seems like lexing could be "almost" comonadic.
lexWith :: Comonad w => (w a -> (shape, token)) -> (token -> b -> b) -> (forall t. shape -> w t -> w t) -> w a -> b
lexWith matchToken cons drop str = walk (extend matchToken str)
    where
        walk x = case extract x of
            (n, token) -> token `cons` walk (drop n x)
        
matchToken regex str = case matchRegexAll regex (toList str) of
    Just ("", matched, _, matches)
        -> (length matched, Just matches)
    _ -> (1, Nothing)

dropPl 0 pl = pl
dropPl n (PointedList _ (x:xs)) = dropPl (n-1) (PointedList x xs)
dropPl _ pl = pl

lexWithRegex re = lexWith (matchToken re) (:) dropPl . fromList