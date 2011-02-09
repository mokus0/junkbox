#!/usr/bin/env runhaskell
module Main where

import Math.AddCFs

main = mapM_ print (toList piCF)