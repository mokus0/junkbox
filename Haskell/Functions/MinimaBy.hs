{-
 -      ``MinimaBy''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module MinimaBy where

minimaBy cmp list = minimaBy' [] list
        where   minimaBy' ms []           = ms
                minimaBy' [] (x:xs)       = minimaBy' [x] xs
                minimaBy' ms@(m:_) (x:xs) = case m `cmp` x of
                        LT -> minimaBy' ms      xs
                        EQ -> minimaBy' (x:ms)  xs
                        GT -> minimaBy' [x]     xs
