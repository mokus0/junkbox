{-
 -      ``select.hs''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Select where

import Data.List
import Control.Monad.State
import Control.Monad.List

permuteBy select []     = [[]]
permuteBy select xs     = [(x:zs) | (x, ys) <- select xs
                                  , zs <- permuteBy select ys]

select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | !(y, ys) <- select xs]

select2 [] = []
select2 (x:xs) = (x, xs) : do
        (y, ys) <- select2 xs
        return (y, x:ys)

select3 [] = []
select3 (x:xs) = (x, xs) : (select3 xs >>= \(y, ys) -> return (y, x:ys))

select4 [] = []
select4 (x:xs) = (x, xs) : concatMap (\(y, ys) -> [(y, x:ys)]) (select4 xs)

select5 [] = []
select5 (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select5 xs)

select6 xs = zip xs (zipWith (++) (inits xs) (tail (tails xs)))

-- this one's not stable; it gradually reverses its list.
-- however, unlike all the others it doesn't build towers of thunks.
-- it can be made stable by reversing |is| in the output of |f| or
-- by accumulating (is ++ [x]) instead of (x:is)
select7 xs = snd (mapAccumL f ([], xs) xs)
        where 
                f (is, (x:ts)) _ = ((x:is, ts), (x, is ++ ts))

-- same thing, ``hiding'' the accumulator in a state monad.
-- doesn't work though; each path through the list monad has its
-- own private state.
select8' xs = evalStateT f ([], xs)
        where f = do
                lift xs
                (is, (x:ts)) <- get
                put (x:is, ts)
                return (x, is ++ ts)

-- what I actually meant is this.
select8 xs = evalStateT (mapM (const f) xs) ([], xs)
        where f = do
                (is, (x:ts)) <- get
                put (x:is, ts)
                return (x, is ++ ts)


-- same as above, but commuting list and state.
-- note that I'm not sure (ListT State) satisfies all the relevant laws...
select9 xs = evalState (runListT f) ([], xs)
        where f = do
                x <- ListT (return xs) -- ugly hack to lift [x] to ListT m x
                (is, (_:ts)) <- lift get
                lift (put (x:is, ts))
                return (x, is ++ ts)
