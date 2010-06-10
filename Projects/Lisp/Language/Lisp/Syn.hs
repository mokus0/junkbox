{-
 -      ``Language/Lisp/Syn''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Language.Lisp.Syn where

data SExp a
        = Atom a
        | List [SExp a]
        deriving (Eq, Show)

data Atom
        = R Rational    -- Rational literal (including integers)
        | F Double      -- Floating-point literal
        | V String    -- Symbol
        | S String    -- String literal
        deriving (Eq, Show)

type LispSyn = SExp Atom
