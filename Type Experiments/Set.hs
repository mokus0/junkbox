{-
 -      ``Set''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
        FlexibleContexts
  #-}

module Set where

type Set a = a -> Bool
type Relation a b = a -> b -> Bool

contains :: Relation (Set a) a
set `contains` elem = set elem

class Existential c where
        exists :: (c -> Bool) -> Bool

join :: Existential (Set a) => Set (Set a) -> Set a
join s = \e -> exists (\s -> s `contains` e)