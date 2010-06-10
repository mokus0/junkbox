module The'Lude
     -- basic data types
    ( P.Bool(..), (P.&&), (P.||), P.not, P.otherwise
    
    , P.Maybe(..), P.maybe
    
    , P.Either(..), P.either
    
    , P.Ordering(..)
    , P.Char, P.String
    
    , P.fst, P.snd, P.curry, P.uncurry
    
    -- basic type classes
    , P.Eq(..), P.Ord(..)
    , P.Enum(..), P.Bounded(..)
    
    -- Numeric types
    , P.Int, P.Integer
    , P.Float, P.Double
    , P.Rational
    
    -- Numeric type classes
    , P.Num(..), P.Integral(..), P.Real(..)
    , P.Fractional(..), P.RealFrac(..)
    , P.Floating(..), P.RealFloat(..)
    
    -- Numeric functions
    , P.subtract
    , P.even, P.odd, P.gcd, P.lcm
    , (P.^), (P.^^)
    , P.fromIntegral, P.realToFrac
    
    -- Higher-order classes
    , Category(id, (.))
    , P.Functor(..), Applicative(..)
    , Pointed(..), P.Monad(..)
    , Copointed(..), Comonad(..)
    
    -- related functions
    , P.mapM, P.mapM_
    , P.sequence, P.sequence_
    , (P.=<<)
    
    -- Misc functions
    , P.const
    , P.flip
    , (P.$), (P.$!)
    , P.until
    , P.asTypeOf
    , P.error, P.undefined
    , P.seq
    
    -- List operations
    , P.map
    , (P.++)
    , P.filter
    , P.head, P.tail
    , P.init, P.last
    , P.null
    , P.length
    , (P.!!)
    , P.reverse
    
    -- Reducing lists
    , P.foldl, P.foldl1
    , P.foldr, P.foldr1
    
    -- Special folds
    , P.and, P.or
    , P.any, P.all
    , P.sum, P.product
    , P.concat, P.concatMap
    , P.maximum, P.minimum
    
    -- Building lists
    , P.scanl, P.scanl1
    , P.scanr, P.scanr1
    , P.iterate
    , P.repeat
    , P.replicate
    , P.cycle
    
    -- Sublists
    , P.take, P.takeWhile
    , P.drop, P.dropWhile
    , P.splitAt
    , P.span, P.break
    
    -- Zipping and unzipping lists
    , P.zip, P.zip3
    , P.zipWith, P.zipWith3
    , P.unzip, P.unzip3
    
    -- Functions on Strings
    , P.lines, P.unlines
    , P.words, P.unwords
    
    -- Converting to String
    , P.ShowS, P.Show(..)
    , P.shows, P.showChar, P.showString
    , P.showParen
    
    -- Converting from String
    , P.ReadS, P.Read(..)
    , P.reads, P.read
    , P.readParen
    , P.lex
    
    -- Basic Input and Output
    , P.IO
    
    -- Simple IO operations
    , P.putChar, P.putStr, P.putStrLn, P.print
    , P.getChar, P.getLine, P.getContents
    , P.interact
    
    -- Files
    , P.FilePath
    , P.readFile, P.writeFile, P.appendFile
    , P.readIO, P.readLn
    
    -- Exception handling
    , Exception(), SomeException
    , throwIO
    , try, evaluate
    , catch, handle, finally
    , bracket, bracket_
    , onException
    
--    , isEvaluated
    ) where

import qualified Prelude as P

import Control.Category
import Control.Applicative
import Control.Functor.Pointed
import Control.Comonad

import Control.Exception
--import Data.IsEvaluated