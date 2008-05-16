{-
 -      ``Set''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Set where

type Set a = a -> Bool

join :: Set (Set a) -> Set a
join s = \e -> ???