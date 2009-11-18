{-
 -      ``GetFullProgName''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module GetFullProgName where

import Foreign
import Foreign.C

-- from http://www.haskell.org/haskellwiki/SPOJ

foreign import ccall unsafe "getProgArgv"
    getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
 
getFullProgName :: IO String
getFullProgName =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        getProgArgv p_argc p_argv
        argv <- peek p_argv
        s    <- peekElemOff argv 0 >>= peekCString
        return s
