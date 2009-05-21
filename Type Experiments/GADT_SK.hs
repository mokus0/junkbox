{-
 -      ``GADT_SK''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE GADTs #-}

module GADT_SK where

-- from the haskell wiki
data Term x where
    K :: Term (a -> b -> a)
    S :: Term ((a -> b -> c)  -> (a -> b) -> a -> c)
    Const :: (Int -> ShowS) -> a -> Term a
    (:@) :: Term (a -> b) -> (Term a) -> Term b
infixl 6 :@

step :: Term a -> Term a
step x = case step' x of
    Nothing -> x
    Just x  -> x

step' :: Term a -> Maybe (Term a)
step' (K :@ x :@ y) = Just x
step' (S :@ x :@ y :@ z) = Just (x :@ z :@ (y :@ z))
step' (Const fs f :@ x) = Just (Const (\p -> showParen (p > 10) (fs 10 . showChar ' ' . showsPrec 11 x)) (f (eval x)))
step' (f :@ x) = case step' f of
    Just f -> Just (f :@ x)
    Nothing -> case step' x of
        Just x -> Just (f :@ x)
        Nothing -> Nothing
step' x = Nothing

eval :: Term a -> a
eval K = const
eval S = \f g x -> f x (g x)
eval (Const _ x) = x
eval (f :@ x) = eval f (eval x)

instance Show (Term t) where
    showsPrec p K = showChar 'K'
    showsPrec p S = showChar 'S'
    showsPrec p (Const cs _) = showParen (p > 10) (showString "Const " . cs 11)
--    showsPrec p (Const c) = showParen (p > 10) (showString "Const " . showsPrec 11 c)
    showsPrec p (f :@ x) = showParen (p > 6) (showsPrec 0 f . showString " " . showsPrec 11 x)

join :: Term (Term a) -> Term a
join (Const _ t) = t
join (K :@ x :@ y) = K :@ join x :@ y
join (S :@ x :@ y :@ z) = S :@ (Const showEval (\f x y -> eval (f x y)) :@ x) :@ y :@ z
    where
        showEval p = showParen (p > 9) (showString "\\f x y -> eval (f x y)")

fTerm :: (Term a -> Term b) -> Term (a -> b)
fTerm = undefined

joinTerm :: Term (Term a -> a)
joinTerm = undefined

join2Term :: (a -> b -> Term c) -> a -> b -> c
join2Term f x y = eval (f x y)