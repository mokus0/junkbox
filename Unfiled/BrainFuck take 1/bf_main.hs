module Main where

import BF
import IO
import System.Environment

main :: IO ()

main = do 
	(sourceFileName:_) <- getArgs
	sourceFile <- openFile sourceFileName ReadMode
	source <- hGetContents sourceFile
	hSetBuffering stdin LineBuffering 
	input <- hGetContents stdin
	hPutStr stdout $ runStandardBF source input
	hFlush stdout
	


