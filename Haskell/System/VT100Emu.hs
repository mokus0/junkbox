{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
module System.VT100Emu where

import System.VT100Emu.Buffer

import Prelude                          as P
import Data.Attoparsec                  as AP
import Data.ByteString                  as BS
import Data.Text                        as T
import Data.Text.Encoding               as T
import Codec.Terminal.ECMA048
import Codec.Terminal.ECMA048.C0
import Codec.Terminal.ECMA048.C1
import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Reader
import TypeExperiments.PrimRef

import qualified Data.Vector            as B
import qualified Data.Vector.Unboxed    as U

import Data.Vector.Generic.Mutable as MV

type InputChunk = Either Control ByteString
inputChunk :: Parser InputChunk
inputChunk
    =   Left  <$> control c1_7bit
    <|> Right <$> AP.takeWhile (/= 0x1b)

data VT100State s = VT100State
    { screenBuf     :: Buffer B.MVector s (Buffer U.MVector s Char)
    , streamBuf     :: STRef s (ByteString -> AP.Result InputChunk)
    , textBuf       :: STRef s ByteString
    , encodeText    :: Text -> ByteString
    , decodeText    :: ByteString -> (Text, ByteString)
    }

newtype VT100 m a = VT100 (ReaderT (VT100State (PrimState m)) m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans VT100 where
    lift = VT100 . lift

runVT100 :: PrimMonad m => VT100 m a -> VT100State (PrimState m ) -> m a
runVT100 (VT100 x) = runReaderT x

vt100state :: Monad m => VT100 m (VT100State (PrimState m))
vt100state = VT100 ask

decodeUtf8Stream :: ByteString -> (Text, ByteString)
decodeUtf8Stream bs = case decodeUtf8' bs of
    Left exc -> error "decodeUtf8Stream: write me"
    Right t  -> (t, BS.empty)

newVT100State :: PrimMonad m => m (VT100State (PrimState m))
newVT100State = do
    screenBuf <- newBuffer
    line0 <- newBuffer
    insert line0 screenBuf
    setPosition screenBuf 0
    
    streamBuf <- newSTRef (parse inputChunk)
    textBuf   <- newSTRef BS.empty
    let encodeText = encodeUtf8
        decodeText = decodeUtf8Stream
    return VT100State{..}

resetStreamBuf :: PrimMonad m => VT100 m ()
resetStreamBuf = do
    VT100State{..} <- vt100state
    lift (writeSTRef streamBuf (parse inputChunk))

feedStreamBuf :: PrimMonad m => ByteString -> VT100 m [InputChunk]
feedStreamBuf bs = do
    VT100State{..} <- vt100state
    streamParser <- lift (readSTRef streamBuf)
    case streamParser bs of
        Fail rest expecting msg -> fail (P.unwords ["stream parsing error: ", show expecting, show msg])
        Done rest result -> do
            resetStreamBuf
            liftM (result :) (feedStreamBuf rest)
        Partial f -> do
            -- try to force parser to end, but don't push it into an
            -- error state or accept an incomplete control sequences
            let accept rest result = do
                    resetStreamBuf
                    case result of
                        Right x |  BS.null x    -> return []
                        _                       -> return [result] 
                defer = do
                    lift (writeSTRef streamBuf f)
                    return []
            
            case f BS.empty of
                Done _ (Left (C1 CSI))  -> defer
                Done rest result        -> accept rest result 
                _                       -> defer

receive :: PrimMonad m => ByteString -> VT100 m ()
receive bs = do
    inputs <- feedStreamBuf bs
    P.mapM_ processInputChunk inputs

processInputChunk :: PrimMonad m => InputChunk -> VT100 m ()
processInputChunk (Left ctrl) = processControl ctrl
processInputChunk (Right str) = processByteString str

processControl (C0 c) = processC0 c
processControl _ = return ()

processC0 LF = undefined
processC0 _ = return ()

processByteString :: PrimMonad m => ByteString -> VT100 m ()
processByteString bs = do
    VT100State{..} <- vt100state
    buffered <- lift (readSTRef textBuf)
    let (text, rest) = decodeText (BS.append buffered bs)
    lift (writeSTRef textBuf rest)
    
    processText text

processText :: PrimMonad m => Text -> VT100 m ()
processText txt = do
    VT100State{..} <- vt100state
    lift $ do
        buf <- (MV.new (T.length txt))
        P.sequence_
            [ MV.write buf i c
            | i <- [0..]
            | c <- T.unpack txt
            ]
        edit (replaceMany buf) screenBuf

