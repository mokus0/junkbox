module M1M2 where

{-# NOINLINE m1 #-}
m1      = ((filter odd [1..]) !!)

{-# NOINLINE m2 #-}
m2 n    = ((filter odd [1..]) !! n)

{-# NOINLINE m3 #-}
m3 :: Int -> Integer
m3 n    = ((filter odd [1..]) !! n)
