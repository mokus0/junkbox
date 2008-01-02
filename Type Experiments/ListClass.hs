{-# OPTIONS -fglasgow-exts #-}
{-
 -      "ListClass.hs"
 -      (c) 2008 James Cook
 -      
 -      This would be nifty if it were allowed...
 -}

module ListClass where

import qualified Prelude

class List h t l | h t -> l
        where 
                (:)     :: h -> t -> l
                []      :: l

instance List x (Prelude.[] x) (Prelude.[] x)
        where
                (:) = (Prelude.:)
                [] = Prelude.[]