{-
 -      ``Dice''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE GADTs #-}

module Dice where

import Data.List
import Data.Ratio
import Text.Printf

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Control.Monad

import Random

data Expr a where
    Const  :: String -> a       -> Expr a
    Plus   :: Expr a -> Expr a  -> Expr a
    Minus  :: Expr a -> Expr a  -> Expr a
    Times  :: Expr a -> Expr a  -> Expr a
    Divide :: Expr a -> Expr a  -> Expr a
--    Repeat :: Expr a :: Expr a :: -> Expr a
    deriving Show

instance Functor Expr where
    fmap f = foldExpr (\s x -> Const s (f x)) Plus Minus Times Divide

foldExpr c (+) (-) (*) (/) {-(#)-} = fold
    where 
        fold (Const  s a) = c s a
        fold (Plus   x y) = fold x + fold y
        fold (Minus  x y) = fold x - fold y
        fold (Times  x y) = fold x * fold y
        fold (Divide x y) = fold x / fold y
--        fold (Repeat x y) = fold x # fold y

evalExprWithDiv :: Num a => (a -> a -> a) -> Expr a -> a
evalExprWithDiv (/) = foldExpr (const id) (+) (-) (*) (/) -- (*)

evalFractionalExpr :: Fractional a => Expr a -> a
evalFractionalExpr = evalExprWithDiv (/)

evalIntegralExpr :: Integral a => Expr a -> a
evalIntegralExpr = evalExprWithDiv div

----------------------------------------------------------------
-- Commuting Expr with an arbitrary Monad m

commute con x y = do
    x <- runExpr x
    y <- runExpr y
    return (con x y)

runExpr :: Monad m => Expr (m a) -> m (Expr a)
runExpr (Const  s x) = x >>= return . Const s
runExpr (Plus   x y) = commute Plus   x y
runExpr (Minus  x y) = commute Minus  x y
runExpr (Times  x y) = commute Times  x y
runExpr (Divide x y) = commute Divide x y
-- runExpr (Repeat x y) = commute Repeat x y

----------------------------------------------------------------
-- Pretty-printing 'Expr's

fmtIntegralExpr :: (Show a, Integral a) => Expr a -> String
fmtIntegralExpr e = 
    shows (evalIntegralExpr e)
    . showString " = "
    . showParen True (fmtExprPrec showScalarConst e 0)
    $ ""

fmtIntegralListExpr :: (Show a, Integral a) => Expr [a] -> String
fmtIntegralListExpr e = 
    shows (evalIntegralExpr (fmap sum e))
    . showString " = "
    . showParen True (fmtExprPrec showListConst e 0)
    $ ""

fmtSimple :: Integral a => Expr [a] -> String
fmtSimple e = 
    shows (evalIntegralExpr (fmap sum e))
    . showString " = "
    . showParen False (fmtExprPrec showSimpleListConst e 0)
    $ ""

fmtSimpleRational :: Expr [Integer] -> String
fmtSimpleRational e =
    showRational 0 (evalFractionalExpr (fmap (fromInteger.sum) e))
    . showString " = "
    . showParen False (fmtExprPrec showSimpleListConst e 0)
    $ ""


showScalarConst d  v  p = showString d . showString "[" . shows v . showString "]"
showListConst   d  v  p = showString d . shows v

showSimpleConst showsPrec d [v] p = showsPrec p v
showSimpleConst showsPrec d  v  p = showParen (p > 0) (foldl1 (.) (intersperse (showChar '+') (map (showsPrec 6) v)))

showSimpleListConst :: Show a => String -> [a] -> Int -> ShowS
showSimpleListConst = showSimpleConst showsPrec

showSimpleRationalConst = showSimpleConst showRational

showDouble :: Double -> ShowS
showDouble d = showString (trim (printf "%.04g" d))
    where trim = reverse . dropWhile (=='0') . reverse

showRational p d
    | denominator d == 1    = shows (numerator d)
    | otherwise             = showParen (p > 7)
        ( showDouble (fromRational d)
        . showString " = "
        . shows (numerator d) 
        . showChar '/'
        . shows (denominator d)
        )

fmtExprPrec :: (String -> a -> Int -> ShowS) -> Expr a -> Int -> ShowS
fmtExprPrec showConst e = foldExpr
    (\d v p -> showConst d v p)
    (\x y p -> showParen (p >  6) (x 6 . showString " + " . y 6))
    (\x y p -> showParen (p >  6) (x 6 . showString " - " . y 7))
    (\x y p -> showParen (p >  7) (x 7 . showString " * " . y 7))
    (\x y p -> showParen (p >  7) (x 7 . showString " / " . y 8))
    e

----------------------------------------------------------------
-- Rolling dice

rollEm :: String -> IO (Either ParseError String)
rollEm str = case parseExpr "rollEm" str of
    Left err    -> return (Left err)
    Right ex    -> do
        ex <- runExpr ex :: IO (Expr [Integer])
        return (Right (fmtSimpleRational ex))
--        return (Right (fmtIntegralListExpr ex))

roll :: (Integral a, Random a) => a -> a -> IO [a]
roll count sides
    | count > 100   = do
        x <- ndRandomIO
        let e = count*(sides+1)`div`2
            e' = fromIntegral (count*(sides+1)`mod`2)/2
            v = fromIntegral (sides*sides-1)/12
            x' = e' + x * sqrt (fromIntegral count * v)
        return [e + round x']
    | count > 7     = do
        ls <- rollEm
        return [sum ls]
    | otherwise     = rollEm

    where rollEm = replicateM (fromIntegral count)
                   (randomRIO (1,sides))

-- | get a normally distributed random number
ndRandomIO :: IO Double
ndRandomIO = do r   <- randomRIO (0, 1)
                phi <- randomRIO (0, 2*pi)
                let r' = sqrt (-2 * log r)
                return (r' * sin phi)

----------------------------------------------------------------
-- The parser

parseExpr :: (Integral a, Random a) => String -> String -> Either ParseError (Expr (IO [a]))
parseExpr src str = runParser expr False src str

-- a token-lexer thing
diceLang :: TokenParser st
diceLang = makeTokenParser 
    (haskellStyle { reservedOpNames = ["*","/","+","-"{-,"#"-}] })

expr :: (Integral a, Random a) => CharParser st (Expr (IO [a]))
expr = do
    whiteSpace diceLang
    e <- term
    eof
    return e

term :: (Integral a, Random a) => CharParser st (Expr (IO [a]))
term = buildExpressionParser table primExp
    where   table =
                [ [binary "*" Times AssocLeft, binary "/" Divide AssocLeft ] 
                , [binary "+" Plus  AssocLeft, binary "-" Minus  AssocLeft ]
--                , [binary "#" Repeat AssocRight]
                ]
            binary name fun assoc = Infix (do{ reservedOp diceLang name; return fun }) assoc

primExp :: (Integral a, Random a) => CharParser st (Expr (IO [a]))
primExp = try dieExp <|> numExp <|> parens diceLang term

dieExp :: (Integral a, Random a) => CharParser st (Expr (IO [a]))
dieExp = do
    (cStr, count) <- option ("", 1) number
    (sStr, sides) <- char 'd' >> number
    return (Const (cStr ++ 'd' : sStr) (roll (fromInteger count) (fromInteger sides)))

numExp :: Num a => CharParser st (Expr (IO [a]))
numExp = do 
    (str, num) <- number
    return (Const str (return [fromInteger num]))

number :: CharParser st (String, Integer)
number = do
    n <- many1 digit <?> "number"
    whiteSpace diceLang
    return (n, read n)
