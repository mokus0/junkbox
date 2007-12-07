import Data.Char
import Data.Bits
import System.Environment

	-- function to hash one character, given one character each from the
	-- password and challenge hash
hashChar pChar hChar = chr (((p - 0x20) `xor` h) + 0x20)
	where
		p = ord pChar
		h = ord hChar

	-- hash the whole string, by iterating the 'hashChar' function
	-- and collecting the results
hashThing = zipWith hashChar

	-- "main" - make the function into a command-line program
	-- that takes 2 args
main = do
	(pass: hash: _) <- getArgs
	putStrLn (hashThing pass hash)
