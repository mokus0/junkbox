{-# LANGUAGE RecordWildCards #-}
module Math.Luby where

import Control.Monad.State
import Data.Bits
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap          as I
import qualified Data.IntSet          as IS
import qualified Data.Set             as S
import Data.Random
import Data.RVar
import Data.Word
import System.Random.Mersenne.Pure64

data Params = Params
    { nBlocks       :: Int
    , degreeDist    :: RVar Int
    }

type Seed = Word64

data Block = Block
    { seed      :: Seed
    , chunk     :: BS.ByteString
    }

sampleWithSeed :: Seed -> RVar a -> a
sampleWithSeed s x = evalState (sampleRVar x) (pureMT s)

xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS x y = BS.pack (BS.zipWith xor (pad x) (pad y))
    where
        n = max (BS.length x) (BS.length y)
        pad z
            | l < n     = BS.append z (BS.replicate (n-l) 0)
            | otherwise = z
            where l = BS.length z

geom :: Params -> Seed -> [Int]
geom Params{..} = flip sampleWithSeed $ do
    d   <- degreeDist
    shuffleNofM d nBlocks [0..]

block :: Params -> [BS.ByteString] -> Seed -> Block
block params message seed = Block{..}
    where chunk = foldr xorBS BS.empty (map (message !!) (geom params seed))

data ReceiverState = ReceiverState
    { chunks    :: I.IntMap BS.ByteString
    , blocks    :: S.Set Block
    }

initialState = ReceiverState I.empty S.empty

recvBlock :: Params -> Block -> ReceiverState -> ReceiverState
recvBlock params Block{..} ReceiverState{..} = undefined
    where
        -- need "blocks" to store geometry, not seed
        incomingBlocks = geom params seed
        incomingKnown = filter (`I.member` chunks)
        