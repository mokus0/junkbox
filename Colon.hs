{-
 -      ``Colon.hs''
 -      (c) 2008 James Cook
 -}

module Colon where

import Prelude (Eq(..), Show(..), Bool(..))

data C a b = a : b
        deriving (Eq, Show)

infixr 1 :
infixr 1 ?

True ? a : b = a
False ? a : b = b