{-# OPTIONS -fglasgow-exts #-}
{-
 -      "ListClass.hs"
 -      (c) 2008 James Cook
 -      
 -      This would be nifty if original : and [] could be redefined.
 -}

module ListClass where

class List h t l | h t -> l, l -> h t
        where 
                cons     :: h -> t -> l
                nil      :: l

instance List x [x] [x]
        where
                cons = (:)
                nil = []