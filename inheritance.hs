{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
{-
 -	"/Users/mokus/Projects/mokus-sandbox/inheritance.hs"
 -	(c) 2007 James Cook
 -	
 -	Simple test of automatic inheritance.  Essentially, a
 -	simple example of automagically deriving all class functions,
 -	made possible by the design of the base class.
 -}

module Inheritance where

class Base a where
	out :: a -> String -> IO ()
	out x = putStrLn


class (Base a) => Foo a where
	foo :: a -> IO ()
	foo x = out x "foo"

class (Base a) => Bar a where
	bar :: a -> IO ()
	bar x = out x "bar"


instance (Bar a) => Foo a

data BaseImpl = Base String
	deriving (Eq, Show)

instance Base BaseImpl where
	out (Base name) str = do
		putStr name
		putStr " says: "
		putStrLn str

data BarImpl = Bar
	deriving (Eq, Show)

instance Base BarImpl
instance Bar BarImpl