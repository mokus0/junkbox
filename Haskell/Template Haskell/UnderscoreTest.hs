{-# LANGUAGE CPP, QuasiQuotes #-}
module UnderscoreTest where

#include "_.h"

x = _ :: Int -> Int
y = _ :: String
z = _ :: Double

a :: String
a = undefined

