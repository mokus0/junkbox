{-
 -      ``Language/Lisp/Parse''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
module Language.Lisp.Parse where

import Language.Lisp.Syn
import Text.ParserCombinators.Parsec
import Control.Monad

atom :: Parser Atom
atom = foldl1 (<|>) [try p | p <- [r,f,v,s]]
        where
                r = fmap (R . fromIntegral . read) (many1 digit)
                f = fmap (F) (fail "")
                v = fmap (V) (fail "")
                s = fmap (S) (liftM2 (:) letter (many symChar))
                
                symChar = letter <|> digit

sExp :: Parser a -> Parser (SExp a)
sExp atom = undefined
        <|> undefined