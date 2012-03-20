{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Mangle where

import Control.Applicative
import Data.IORef
import Data.Char
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Language.Haskell.TH
import System.IO
import Data.Random
import Data.Generics.Geniplate

data Mangler m = Mangler { getMangledName :: Name -> m Name }

liftMangler :: (forall t. m t -> n t) -> Mangler m -> Mangler n
liftMangler lift = Mangler . fmap lift . getMangledName

ioMangler :: (Name -> IO Name) -> IO (Mangler IO)
ioMangler mangleNewName = do
    knownNames <- newMVar M.empty
    
    let getName :: Name -> M.Map Name Name -> IO (M.Map Name Name, Name)
        getName n ns = case M.lookup n ns of
            Just x  -> return (ns, x)
            Nothing -> do x <- mangleNewName n; return (M.insert n x ns, x)
    
    return Mangler { getMangledName = modifyMVar knownNames . getName }

stdMangler :: IO (Mangler IO)
stdMangler = do
    allWords <- V.fromList . lines <$> readFile "/usr/share/dict/words"
    pickedUpper <- newMVar S.empty
    pickedLower <- newMVar S.empty
    
    let (upper, lower) = V.partition (isUpper . head) allWords
        isTypeName = foo . head . nameBase where foo ':' = True; foo c = isUpper c
        pickWord orig
            | isTypeName orig   = pickWordFrom upper pickedUpper
            | otherwise         = pickWordFrom lower pickedLower
        
        pickWordFrom theWords pickedWords = modifyMVar pickedWords $ \picked -> do
            let nWords = V.length theWords
            
            let pickIndex = do
                    i <- sample (uniform 0 (nWords - 1))
                    if i `S.member` picked
                        then pickIndex
                        else return i
            
            i <- pickIndex
            return (S.insert i picked, mkName (theWords V.! i))
    
    ioMangler pickWord 

mangler `except` p = Mangler $ \name -> if p name then return name else getMangledName mangler name

preludeNames = S.fromList
    [ '(:)
    , '[]
    , '($!)
    , '(!!)
    , '($)
    , '(&&)
    , '(++)
    , '(.)
    , '(=<<)
    , ''Bool
    , 'False
    , 'True
    , ''Bounded
    , 'minBound
    , 'maxBound
    , ''Char
    , ''Double
    , ''Either
    , ''Enum
    , 'succ
    , 'pred
    , 'toEnum
    , 'fromEnum
    , 'enumFrom
    , 'enumFromThen
    , 'enumFromTo
    , 'enumFromThenTo
    , ''Eq
    , '(==)
    , '(/=)
    , ''FilePath
    , ''Float
    , ''Fractional
    , 'pi
    , 'exp
    , 'sqrt
    , 'log
    , '(**)
    , 'logBase
    , 'sin
    , 'tan
    , 'cos
    , 'asin
    , 'atan
    , 'acos
    , 'sinh
    , 'tanh
    , 'cosh
    , 'asinh
    , 'atanh
    , 'acosh
    , ''Fractional
    , '(/)
    , 'recip
    , 'fromRational
    , ''Functor
    , 'fmap
    , '(<$)
    , ''IO
    , ''IOError
    , ''Int
    , ''Integer
    , ''Integral
    , 'quot
    , 'rem
    , 'div
    , 'mod
    , 'quotRem
    , 'divMod
    , 'toInteger
    , ''Maybe
    , 'Nothing
    , 'Just
    , ''Monad
    , '(>>=)
    , '(>>)
    , 'return
    , 'fail
    , ''Num
    , '(+)
    , '(*)
    , '(-)
    , 'negate
    , 'abs
    , 'signum
    , 'fromInteger
    , ''Ord
    , 'compare
    , '(<)
    , '(>=)
    , '(>)
    , '(<=)
    , 'max
    , 'min
    , ''Ordering
    , 'LT
    , 'EQ
    , 'GT
    , ''Rational
    , ''Read
    , 'readsPrec
    , 'readList
    , ''ReadS
    , ''Real
    , 'toRational
    , ''RealFloat
    , 'floatRadix
    , 'floatDigits
    , 'floatRange
    , 'decodeFloat
    , 'encodeFloat
    , 'exponent
    , 'significand
    , 'scaleFloat
    , 'isNaN
    , 'isInfinite
    , 'isDenormalized
    , 'isNegativeZero
    , 'isIEEE
    , 'atan2
    , ''RealFrac
    , 'properFraction
    , 'truncate
    , 'round
    , 'ceiling
    , 'floor
    , ''Show
    , 'showsPrec
    , 'show
    , 'showList
    , ''ShowS
    , ''String
    , '(^)
    , '(^^)
    , 'all
    , 'and
    , 'any
    , 'appendFile
    , 'asTypeOf
    , 'break
    , 'catch
    , 'concat
    , 'concatMap
    , 'const
    , 'curry
    , 'cycle
    , 'drop
    , 'dropWhile
    , 'either
    , 'elem
    , 'error
    , 'even
    , 'filter
    , 'flip
    , 'foldl
    , 'foldl1
    , 'foldr
    , 'foldr1
    , 'fromIntegral
    , 'fst
    , 'gcd
    , 'getChar
    , 'getContents
    , 'getLine
    , 'head
    , 'id
    , 'init
    , 'interact
    , 'ioError
    , 'iterate
    , 'last
    , 'lcm
    , 'length
    , 'lex
    , 'lines
    , 'lookup
    , 'map
    , 'mapM
    , 'mapM_
    , 'maximum
    , 'maybe
    , 'minimum
    , 'not
    , 'notElem
    , 'null
    , 'odd
    , 'or
    , 'otherwise
    , 'print
    , 'product
    , 'putChar
    , 'putStr
    , 'putStrLn
    , 'read
    , 'readFile
    , 'readIO
    , 'readLn
    , 'readParen
    , 'reads
    , 'realToFrac
    , 'repeat
    , 'replicate
    , 'reverse
    , 'scanl
    , 'scanl1
    , 'scanr
    , 'scanr1
    , 'seq
    , 'sequence
    , 'sequence_
    , 'showChar
    , 'showParen
    , 'showString
    , 'shows
    , 'snd
    , 'span
    , 'splitAt
    , 'subtract
    , 'sum
    , 'tail
    , 'take
    , 'takeWhile
    , 'uncurry
    , 'undefined
    , 'unlines
    , 'until
    , 'unwords
    , 'unzip
    , 'unzip3
    , 'userError
    , 'words
    , 'writeFile
    , 'zip
    , 'zip3
    , 'zipWith
    , 'zipWith3
    , '(||)
    ]

isPreludeName = flip S.member preludeNames

exceptPrelude :: Monad m => Mangler m -> Mangler m
exceptPrelude = flip except isPreludeName

-- does not properly handle "generic" names, but whatever... it's just a silly toy.
mangleDec :: (Name -> Q Name) -> Dec -> Q Dec
mangleDec = $(genTransformBiM 'mangleDec)
