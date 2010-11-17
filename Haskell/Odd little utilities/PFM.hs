{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns #-}
module PFM where

import Control.Applicative ((<|>))
import Prelude hiding (readFile)
import Data.ByteString (readFile)
import Control.Monad (replicateM)
import Numeric
import Data.Attoparsec
import qualified Data.Vector.Unboxed as U (Vector, fromListN)
import qualified Data.Vector as V (Vector, fromListN)
import Data.Word
import Data.List (genericReplicate)
import Data.Char (ord, chr, isDigit)
import Foreign

data PFM = PFM
    { width     :: !Word32
    , height    :: !Word32
    , scale     :: !Float
    , image     :: !(V.Vector (U.Vector Float))
    }

readPFMFromFile :: FilePath -> IO PFM
readPFMFromFile path = do
    pfmData <- readFile path
    case parse pfm pfmData of
        Fail x y z -> fail (unlines [show y, show z])
        Partial _ -> fail "incomplete PFM data"
        Done _ x -> return x
    


greplicateM :: (Integral a, Monad m) => a -> m b -> m [b]
greplicateM n = sequence . genericReplicate n

checkedFromIntegral :: (Integral a, Integral b) => a -> b
checkedFromIntegral x
    | x == fromIntegral x'  = x'
    | otherwise             = error "checkedFromIntegral: check failed"
        where x' = fromIntegral x

pfm :: Parser PFM
pfm = do
    ((!width,!height),!scale,!le) <- pfmHeader
    !imgData <- greplicateM height (floatLine width le)
    
    let !image = packImageData height imgData
    return $! PFM{..}

floatLine :: Word32 -> Bool -> Parser (U.Vector Float)
floatLine w le = do
    floats <- greplicateM w (rawFloat le)
    return $! U.fromListN (checkedFromIntegral w) floats

packImageData :: Word32 -> [U.Vector Float] -> V.Vector (U.Vector Float)
packImageData h xss = V.fromListN h' xss
    where
        h' = checkedFromIntegral h

pfmHeader :: Parser ((Word32, Word32), Float, Bool)
pfmHeader = do
    string "Pf\n"
    size   <- dimensionsLine
    (s,le) <- scaleLine
    return (size,s,le)

dimensionsLine :: Parser (Word32, Word32)
dimensionsLine = do
    w <- int
    word8 0x20 <|> word8 0x0a
    h <- int
    word8 0x0a
    return (w,h)

scaleLine :: Parser (Float, Bool)
scaleLine = do
    scale <- float
    word8 0x0a
    return (abs scale, scale < 0)

int :: Integral a => Parser a
int = do
    str <- many1 (satisfyWith (chr.fromIntegral) isDigit)
    return (fromInteger (read str))

float :: RealFrac a => Parser a
float = do
    sgn <- option id $ do
        neg <- word8 45
        return negate
    str1 <- many1 (satisfyWith (chr.fromIntegral) isDigit)
    str2 <- option "" $ do
        dot <- word8 46
        digs <- many1 (satisfyWith (chr.fromIntegral) isDigit)
        return ('.':digs)
    
    let [(f, "")] = readFloat (str1 ++ str2)
    return (sgn f)

rawFloat :: Bool -> Parser Float
rawFloat le = do
    [a,b,c,d] <- replicateM 4 anyWord8
    return (buildFloat le a b c d)

buildFloat :: Bool -> Word8 -> Word8 -> Word8 -> Word8 -> Float
buildFloat False a b c d = buildFloat True d c b a
buildFloat True  a b c d = unsafePerformIO $ allocaBytes 4 $ \buf -> do
    -- depends on the fact that this is running on a little-endian system
    pokeByteOff buf 0 a
    pokeByteOff buf 1 b
    pokeByteOff buf 2 c
    pokeByteOff buf 3 d
    peek (castPtr buf)
