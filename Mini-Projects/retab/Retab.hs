{-
 -      ``Retab''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    RecordPuns, ParallelListComp
  #-}

module Retab where

data TabSpec
    = Soft      { spacesPerTab  :: Int }
    | Hard      { spacesPerTab  :: Int }
--     | Elastic   {}

type Line = String 

detabLine :: TabSpec -> Line -> Line
detabLine (Soft {}) = id
detabLine (Hard {spacesPerTab}) = hardDetab spacesPerTab spacesPerTab
    where
        hardDetab n k ('\t':rest)   = replicate k ' ' ++ hardDetab n n rest
        hardDetab n (k+1) (c:rest)  = c : hardDetab n k rest
        hardDetab n 0 str           = hardDetab n n str
        hardDetab _ _ [] = []

