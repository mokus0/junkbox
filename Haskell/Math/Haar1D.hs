-- translation of SML code from wikipedia, mostly just an exercise in reading ML
module Math.Haar1D where

haar [] = error "haar: empty list"
haar l = aux l [] []
    where
        aux [s] [] d = s : d
        aux []  s  d = aux s [] d
        aux (h1:h2:t) s d = aux t ((h1 + h2) : s) ((h1 - h2) : d)
        aux _ _ _ = error "haar: list length was not a power of 2"

