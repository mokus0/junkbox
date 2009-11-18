{-
 -      ``MultiFold''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        KindSignatures,
        TypeFamilies
  #-}

module TypeExperiments.MultiFold where

--biFoldList :: (a -> b -> c -> c) -> (a -> c -> c) -> (b -> c -> c) -> c -> [a] -> [b] -> c

-- zip-fold: fold over a pair in parallel.  1 case for each product of 2 cases in the base algebra
zipFoldList :: (a -> b -> c -> c)
            -> (a -> c -> c)
            -> (b -> c -> c)
            -> c
            -> [a] -> [b] -> c
zipFoldList cc cz zc zz = mergeFoldList (\_ _ -> (True, True)) cc undefined undefined undefined cz zc zz
-- zipFoldList cc cz zc zz = fold
--         where
--                 fold [] [] = zz
--                 fold (x:xs) [] = cz x (fold xs [])
--                 fold [] (y:ys) = zc y (fold [] ys)
--                 fold (x:xs) (y:ys) = cc x y (fold xs ys)

pzip :: [a] -> [b] -> [(a,b)]
pzip = zipFoldList (\a b c -> (a,b) : c) (const id) (const id) []

-- merge-fold: fold over a pair without necessarily consuming everything at every step
-- 1 case for each projection of each product of 2 cases in the base algebra
mergeFoldList :: (a -> b -> (Bool,Bool))  -- ^ cmp: Selection function
              -> (a -> b -> c -> c)    -- ^ fxy: Combine when a and b are interesting
              -> (a -> c -> c)         -- ^ fx:  Combine when a is interesting
              -> (b -> c -> c)         -- ^ fy:  Combine when b is interesting
              -> c                     -- ^ f:   Early termination when nothing is interesting
              
              -> (a -> c -> c)         -- ^ cz:  Combine when b is empty
              -> (b -> c -> c)         -- ^ zc:  Combine when a is empty
              
              -> c                     -- ^ z:   Base case
              -> [a] -> [b] -> c       -- ^ Argument lists and result list
mergeFoldList cmp fxy fx fy f cz zc z = fold
        where
                fold [] [] = z
                fold (x:xs) [] = cz x (fold xs [])
                fold [] (y:ys) = zc y (fold [] ys)
                fold xs@(x:xs') ys@(y:ys') = case cmp x y of
                        ( True,  True) -> fxy x y (fold xs' ys')
                        ( True, False) -> fx x (fold xs' ys)
                        (False,  True) -> fy y (fold xs ys')
                        (False, False) -> f

mergeByR cmp fxy fx fy = mergeFoldList select fxy fx fy undefined
        where 
                select x y = case cmp x y of
                        GT -> (False,  True)
                        EQ -> ( True,  True)
                        LT -> ( True, False)
