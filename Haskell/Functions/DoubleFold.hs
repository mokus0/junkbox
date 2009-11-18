module Functions.DoubleFold where

import Control.Arrow

-- these functions come in pairs.  unless I am mistaken, they are split
-- mono/epi. (ie, @foo' . foo = id@ and @foo . foo'@ is idempotent)

-- this is the only really interesting version in here... it takes 2 folds
-- and combines them into one that does both in a single traversal.
-- laziness makes things really confusing, though, when you try to answer
-- the question of whether there is really any less work being done.
doubleFold :: ((a -> b -> b, b), (a -> c -> c, c)) -> (a -> (b,c) -> (b,c), (b,c))
doubleFold ((f1, z1), (f2, z2)) = (\x -> f1 x *** f2 x, (z1, z2))

-- this one's not as sensible because the function can misbehave by depending on
-- both parameters.  When it does, it will always only see the initial value
-- for the one not being propagated.
doubleFold' :: (a -> (b,c) -> (b,c), (b,c)) -> ((a -> b -> b, b), (a -> c -> c, c))
doubleFold' (f, (z1,z2)) = ((f1,z1), (f2,z2))
    where
        f1 x xs = fst (f x (xs, z2))
        f2 x xs = snd (f x (z1, xs))

-- these are even less sensible, because the function can misbehave in
-- even worse ways - although 'eitherFold' will never produce a fold that
-- does.
eitherFold :: Either (a -> b -> b, b) (a -> c -> c, c) -> (a -> Either b c -> Either b c, Either b c)
eitherFold (Left  (f,z)) = (\x -> f x +++ id, Left  z)
eitherFold (Right (f,z)) = (\x -> id +++ f x, Right z)

-- eitherFold' has to cope with the possibility of a function that
-- will produce a different type than it consumes.  There's no good solution
-- to that, I don't think.  The best idea I came up with was to feed it back
-- the same value without advancing in the fold, but that raises
-- the possibility that the fold will never terminate even when the original
-- one would have.
eitherFold' :: (a -> Either b c -> Either b c, Either b c) -> Either (a -> b -> b, b) (a -> c -> c, c)
eitherFold' (f, Left z) = Left (l, z)
    where
        l x = go . Left
            where
                go xs = case f x xs of
                    Left y  -> y
                    eww -> go eww
eitherFold' (f, Right z) = Right (r, z)
    where
        r x = go . Right
            where
                go xs = case f x xs of
                    Right y  -> y
                    eww -> go eww
