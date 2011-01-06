{-# LANGUAGE EmptyDataDecls #-}
-- based on code from http://cs.bham.ac.uk/~mhe/papers/exhaustive.pdf
-- with some gaps filled in
module Extern.Cantor where

data NF t = Z | S t deriving (Eq, Ord, Show)
data Void

newtype Mu f = Mu (f (Mu f))

newtype N   = N (Mu NF)
newtype Bit = Bit (NF Void)

instance Show     N
instance Eq       N
instance Ord      N
instance Num      N
instance Enum     N
instance Integral N
instance Real     N

instance Show Bit
instance Eq   Bit
instance Ord  Bit
instance Num  Bit

type Cantor = N -> Bit
-- foreveryC :: (Cantor -> Bool) -> Bool
-- foreveryC p = undefined
equalC :: (Cantor -> N) -> (Cantor -> N) -> Bool
equalC f g = foreveryC(\a -> f a == g a)

-- -- Not sure what's up with these, I'm pretty sure I copied them correctly
-- -- from the paper, but as-is they are ill-typed
-- 
-- f,g,h :: Cantor -> N
-- f a = a(10*a(3^80)+100*a(4^80)+1000*a(5^80))
-- g a = a(10*a(3^80)+100*a(4^80)+1000*a(6^80))
-- h a= if a(4^80) == 0 then a j else a (100+j)
--     where
--         i = if a(5^80) == 0 then 0 else 1000
--         j = if a(3^80) == 1 then 10+i else i

type Searcher d = (d -> Bool) -> d
type Quantifier d = (d -> Bool) -> Bool

forsome, forevery :: Searcher d -> Quantifier d
forsome k p = p(k p)
forevery k p = not(forsome k(\x -> not(p x)))

image :: (d -> e) -> Searcher d -> Searcher e
image f k = \q -> f(k(\x -> q(f x)))

times :: Searcher d -> Searcher d' -> Searcher(d,d')
(k `times` k') p = (x, x')
    where
        x = k (\x -> forsome k'(\x' -> p(x,x')))
        x'= k'(\x'-> p(x,x'))

prod :: (N -> Searcher d) -> Searcher(N -> d)
prod e p n=e n(\x->r n x(prod(\i->e(i+n+1))(r n x)))
    where
        r n x a = p (\i -> if i < n then prod e p i
                                    else if i == n then x
                                                   else a (i - n - 1))

bit :: Searcher Bit
bit = \q -> if q 1 then 1 else 0

cantor :: Searcher Cantor
cantor = prod (\i -> bit)

foreveryC :: (Cantor -> Bool) -> Bool
foreveryC = forevery cantor


fan :: (Cantor -> Bool) -> Bool
fan p = mu (\n -> foreveryC (\a -> foreveryC(\b -> 
                    eq n a b ==> (p a == p b))))

-- "Minimization operator"
mu :: (N -> Bool) -> Bool
mu = undefined

-- "=_n"
eq :: N -> Cantor -> Cantor -> Bool
eq = undefined

-- boolean implication
(==>) :: Bool -> Bool -> Bool
True  ==> a = a
False ==> _ = True



prod' :: (N -> Searcher d) -> Searcher(N -> d)
prod' e p = b
    where
        b = id'(\n->e n(\x->r x n(prod'(\i->e(i+n+1))(r x n))))
        r x n a = p(\i->if i < n then b i 
                else if i == n  then x
                                else a(i-n-1))

data T d = B d (T d) (T d)

code :: (N -> d) -> T d
code f = B (f 0) (code(\n -> f(2*n+1)))
                 (code(\n -> f(2*n+2)))

decode :: T d -> (N -> d)
decode (B x l r) 0 = x
decode (B x l r) n = if odd n
                     then decode l ((n-1) `div` 2)
                     else decode r ((n-2) `div` 2)

id' :: (N -> d) -> (N -> d)
id' = decode.code



(x # a)(i) = if i == 0 then x else a(i-1)
tl a = \i -> a(i+1)
prod'' e p =
    let x = e 0(\x->p(x#(prod''(tl e)(\a->p(x#a)))))
     in x#(prod''(tl e)(\a->p(x#a)))

berger :: (Cantor -> Bool) -> Cantor
berger p =  if p(0 # berger(\a -> p(0 # a)))
            then 0 # berger(\a -> p(0 # a)) 
            else 1 # berger(\a -> p(1 # a))