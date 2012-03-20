module intro

add_all : Num a => Vect a n -> Vect a n -> Vect a n
add_all []        []        = []
add_all []        (y :: ys) impossible
add_all (x :: xs) (y :: ys) = (x + y) :: add_all xs ys

append : Vect a n -> Vect a m -> Vect a (n + m)
append []        ys = ys
append (x :: xs) ys = x :: append xs ys

reverse : Vect a n -> Vect a n
reverse []        = []
reverse (x :: xs) ?= append (reverse xs) [x]