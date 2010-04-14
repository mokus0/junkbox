{-
 -      ``Trim''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Functions.Trim where
import Debug.Trace

-- simple test cases
t1a trim = length (t1 trim)     -- test speed
t1b trim = head (t1 trim)       -- test laziness
t1c trim = t1 trim == t1 trimBy -- test correctness
t1 trim = trim p [1..1000000]
    where
        p x = x < 10000 || x > 10500
           && not (x > 10200 && x < 10300)
           && x /= 999999

t2 trim = trim p [0..10] == [1..9]    -- test pattern of evaluation of 'p'
    where
        p x = trace ("p " ++ show x) $! (q x)
        q x = even x

        

-- simple, effective; requires double traversal, i think.
-- Extremely quick, but does not run lazily due to double-reversal.
trimBy p = t . t
        where t = reverse . dropWhile p

-- best lazy version I've come up with so far.
-- uses inverse lists (functions that prepend to lists) in
-- the accumulator to avoid multiple traversals when inserting
-- the 'stashed' elements.  Runs lazily and about as quickly as 'trimBy'
-- when compiled, as long as 'p' is quick.  Potentially a lot slower
-- though - it applies p to every element of the list.
trim, dropEnd :: (a -> Bool) -> [a] -> [a]
trim p = dropEnd p . dropWhile p

-- dropEnd p = reverse . dropWhile p . reverse
dropEnd p = go id
    where
        go t (x:xs)
            -- if p x, stash x (will only be used if 'not (any p xs)')
            | p x       =        go (t.(x:))  xs
            -- otherwise insert x and all stashed values in output and reset the stash
            | otherwise = t (x : go  id       xs)
        -- at end of string discard the stash
        go t [] = []

-- There seems to be a negligible time difference between this and 'trim'.
-- Laziness is the same. Not sure about memory usage or anything else.
trim3 p = dropEnd3 p . dropWhile p
dropEnd3 :: (a -> Bool) -> [a] -> [a]
dropEnd3 p = go []
    where
        go t (x:xs)
            -- if p x, stash x (will only be used if 'not (any p xs)')
            | p x       =                   go (x:t) xs
            -- otherwise insert x and all stashed values in output and reset the stash
            | otherwise = reverse t ++ (x : go  []   xs)
        -- at end of string discard the stash
        go t [] = []

-- other versions, for posterity (and a laugh or 4):

-- not simple, not fast; especially bad with long sequences
-- satisfying |p|.  Not correct either... (t trimBy /= t trimBy2)
trimBy2 p [] = []
trimBy2 p (x:xs)
        | p x           = trimBy2 p xs
        | otherwise     = trimEnd xs
        where
                trimEnd [] = []
                trimEnd (x:xs)
                        | p x           = case trimEnd xs of
                                []      -> []
                                ys      -> x : ys
                        | otherwise     = x : trimEnd xs

-- same thing, but with call to reverse in place of manual reversal
-- (which should eliminate one traversal on each reverse)
trimBy3 p = trimEnd [] . dropWhile p
        where
                trimEnd ys [] = []
                trimEnd [] (x:xs)
                        | p x           = trimEnd [x] xs
                        | otherwise     = x : trimEnd [] xs
                trimEnd ys (x:xs)
                        | p x           = trimEnd (x:ys) xs
                        | otherwise     = reverse ys ++ x : trimEnd [] xs

-- not-so-good version that works in chunks.  Uses lots of stack, does too much work... just plain sucks
trim2 p = dropEnd2 p . dropWhile p
dropEnd2 :: (a -> Bool) -> [a] -> [a]
dropEnd2 p = go
    where
        go xs = case break p xs of 
            (keep, rest) -> keep ++ go2 rest
        
        go2 xs = case break (not.p) xs of
            (stash, rest) -> 
                let gone = go rest
                 in if null gone
                     then []
                     else stash ++ gone
