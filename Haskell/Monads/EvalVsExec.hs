module Monads.EvalVsExec where

main = do
    putStr "What are your names (separated by spaces)? "
    names <- fmap words getLine
    greetRandomName names

greetRandomName [] = putStrLn "Hello there!"
greetRandomName names = randomElement names >>= greet

greet :: String -> IO ()
greet = undefined
    
    