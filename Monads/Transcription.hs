{-# OPTIONS -fth #-}
{-
 -      ``Transcription.hs''
 -      (c) 2008 James Cook
 -}

module Transcription where

add = [| \x y -> x + y |]

mult 0 = [| \y -> 0 |]
mult (n+1) = [| \y -> y + $(mult n) y |]