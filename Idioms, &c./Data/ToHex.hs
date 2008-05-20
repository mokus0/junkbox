{-
 -      ``Data/ToHex''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}

module Data.ToHex where

import Data.Word
import Data.Int
import Data.List
import Text.Printf

class ToHex a where
        toHex :: a -> String

instance ToHex Word8 where
        toHex = printf "%02x"

instance ToHex Word16 where
        toHex = printf "%04x"

instance ToHex Word32 where
        toHex = printf "%08x"

instance ToHex Word64 where
        toHex = printf "%016x"

instance ToHex Int8 where
        toHex = printf "%02x"

instance ToHex Int16 where
        toHex = printf "%04x"

instance ToHex Int32 where
        toHex = printf "%08x"

instance ToHex Int64 where
        toHex = printf "%016x"

instance (ToHex a) => ToHex [a] where
        toHex str = intercalate " " (map toHex str)
