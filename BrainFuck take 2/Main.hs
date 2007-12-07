{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import BF
import OptBF

import IO
import System.Environment

import Data.Int
import Data.Char
import GHC.Exts

i2c :: Int8 -> Char
c2i :: Char -> Int8

i2c = chr . fromIntegral
c2i = fromIntegral . ord

getByte _ = do
	ch <- getChar `catch` (\_ -> return '\000')
	return (c2i ch)

putByte b = putChar (i2c b)

main = do 
	(sourceFileName:_) <- getArgs
	sourceFile <- openFile sourceFileName ReadMode
	source <- hGetContents sourceFile
	
	hSetBuffering stdin NoBuffering 
	hSetBuffering stdout NoBuffering
	
	runBF source 30000 getByte putByte
	hFlush stdout



