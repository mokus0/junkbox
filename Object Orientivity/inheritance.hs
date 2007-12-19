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

class (Base a) => Eep a where
	eep :: a -> IO ()
	eep x = out x "eep"

class (Base a) => Bar a where
	bar :: a -> IO ()
	bar x = out x "bar"

data EepImpl = Eep
	deriving (Eq, Show)

instance Base EepImpl
instance Eep EepImpl

newtype BarImpl = Bar EepImpl
	deriving (Eq, Show, Base, Eep)

instance Foo BarImpl
instance Bar BarImpl

class (Base a) => Qux a where
	qux :: a -> IO ()
	qux x = out x "qux"

newtype QuxImpl = Qux BarImpl
	deriving (Eq, Show, Base, Foo, Bar)

instance Qux QuxImpl