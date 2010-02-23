{-# LANGUAGE GADTs #-}
module Monads.Expr4 where

import Control.Monad.Prompt

data Expr t where
    Add         :: Num t => t -> t -> Expr t
    Multiply    :: Num t => t -> t -> Expr t
    Negate      :: Num t => t -> Expr t
    Length      :: [t] -> Expr Int

eval :: Prompt Expr t -> t
eval = runPrompt f
    where
        f :: Expr t -> t
        f (Add x y) = x + y
        f (Multiply x y) = x * y
        f (Negate x) = negate x
        f (Length xs) = length xs