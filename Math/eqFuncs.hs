{-
 -	"eqFuncs"
 -	(c) 2007 James Cook
 -}

{-# OPTIONS -fglasgow-exts #-}

module Math.EqFuncs where

import Prelude hiding (catch)
import Control.Exception (catch)

import Data.List ((\\))
import qualified Data.Map as M

import GHC.IOBase

class EnumAll a where enumAll :: [a]

instance EnumAll () where enumAll = [()]
instance EnumAll Bool where enumAll = [True, False]
instance EnumAll Ordering where enumAll = [LT, EQ, GT]
instance EnumAll Char where enumAll = ['\000' .. '\255'] -- yes, this isn't "all" - sue me...

instance (EnumAll a) => EnumAll (Maybe a)
	where
		enumAll = Nothing : [Just x | x <- enumAll]

instance (EnumAll a,  EnumAll b) => EnumAll (Either a b)
	where
		enumAll = [Left x | x <- enumAll] ++ [Right x | x <- enumAll]

instance (EnumAll a, EnumAll b) => EnumAll (a,b)
	where
		enumAll = [(x,y) | x <- enumAll, y <- enumAll]

instance (EnumAll a, EnumAll b, EnumAll c) => EnumAll (a,b,c)
	where
		enumAll = [(x,y,z) | x <- enumAll, y <- enumAll, z <- enumAll]

instance (EnumAll a, Ord a, EnumAll b, Eq b) => EnumAll (M.Map a b)
	where
		enumAll = [ M.fromList (zip a's c's)
			  | c's <- combs (length a's) b's
			  ]
			where
				a's = enumAll
				b's = enumAll

combs 0 _ = [[]]
combs n l = [h:t | h <- l, t <- combs (n-1) l]

instance (EnumAll a, Ord a, EnumAll b, Eq b) => EnumAll (a -> b)
	where
		enumAll = [toFunction m | m <- enumAll]

toFunction :: (Ord a) => M.Map a b -> a -> b
toFunction = (M.!)

instance (EnumAll a, Eq b) => Eq (a -> b) 
	where
		f == g = and [noErr (f x) == noErr (g x) | x <- enumAll]

noErr x = unsafePerformIO ((return (Left x)) `catch` (\err -> return (Right err)))
