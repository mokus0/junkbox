module Main
	where

import IO

main = do
	hSetBuffering stdin LineBuffering
	nums <- getNums
	
	let s = sum nums
	let p = product nums
	let fs = map (\n -> (n, factorial n)) nums
	
	putStrLn ("Sum: " ++ (show s))
	putStrLn ("Product: " ++ (show p))
	
	let f = unlines (map (\(n,f) -> (show n) ++ " Factorial is " ++ (show f)) fs)
	
	putStrLn f

getNums = do
	putStr "Number: "
	num <- getLine
	if read num == 0 
		then return []
		else do 
			morenums <- getNums
			return (read num : morenums)

factorial 0 = 1
factorial x = x * (factorial (x-1))