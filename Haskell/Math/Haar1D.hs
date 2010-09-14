-- translation of SML code from wikipedia, mostly just an exercise in reading ML
-- Turns out this is a pretty goofy implementation.  It reverses every other level,
-- which decodes just fine as long as you don't try to truncate the encoded form
-- by an odd number of levels.
module Math.Haar1D where

haar [] = []
haar l = aux l [] []
    where
        aux [s] [] d = s : d
        aux []  s  d = aux s [] d
        aux (h1:h2:t) s d = aux t ((h1 + h2) : s) ((h1 - h2) : d)
        aux _ _ _ = error "haar: list length was not a power of 2"

invHaar [] = []
invHaar (s:xs) = extend [s] xs []
    where
        extend ss [] [] = ss
        extend [] xs zs = extend zs xs []
        extend (s:ss) (d:ds) zs = extend ss ds (0.5 * (s + d) : 0.5 * (s - d) : zs)
        extend _ _ _ = error "invHaar: list length was not a power of 2"
