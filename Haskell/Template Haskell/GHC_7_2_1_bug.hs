{-# LANGUAGE TemplateHaskell #-}
module GHC_7_2_1_bug where

$([d| instance Show (a -> b) where
        showsPrec _ _ = showString "<function>"
 |])

