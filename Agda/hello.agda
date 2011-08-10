module hello where

open import IO
open import Data.Unit
open import Data.String
open import Function
open import Coinduction

import IO.Primitive

postulate getLine : IO.Primitive.IO String
{-# COMPILED getLine getLine #-}

askName =  ♯ (putStrLn "Type your name, OR ELSE (or else don't, that is - I don't care): ")
        >> ♯ lift getLine

sayHi : String -> IO ⊤
sayHi name = putStrLn ("hello " ++ name ++ ", it finally works.")

gripe = putStrLn "Agda needs MOAR SYNTAX..."

main = run (♯(♯ askName >>= ((♯_) ∘ sayHi)) >> ♯ gripe)

    
