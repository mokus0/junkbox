module Main where

import IO
import System.Environment

-- skipSet:
--	chars to skip in text stream
skipSet :: [Char]
skipSet = ['\000' .. '\032'] ++ ['\127' .. '\255']

--
-- starfilter:
--	merge 2 strings by replacing every '*' in one with a character
-- from the other.  Extra '*'s are kept.  Extra data from string 2
-- is discarded.
starfilter :: String -> String -> String
starfilter [] _ = []
starfilter ('*':pattern) [] = '*' : pattern
starfilter ('*':pattern) (c:text) = 
	if elem c skipSet
		then (starfilter ('*':pattern) text)
		else c : (starfilter pattern text)
starfilter (c:pattern) text = c : (starfilter pattern text)


main :: IO ()
main = do
	(starFileName : textFileName : _) <- getArgs
	starFile <- openFile starFileName ReadMode
	textFile <- openFile textFileName ReadMode
	star <- hGetContents starFile
	text <- hGetContents textFile
	putStr $ starfilter star text