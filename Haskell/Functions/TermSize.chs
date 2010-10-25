{-# LANGUAGE ForeignFunctionInterface #-}
----- to compile:
-- gcc -arch i386 -c Functions/TermSize.c -o Functions/TermSize_CBits.o
-- c2hs Functions/TermSize.chs
-- ghc Functions/TermSize.hs Functions/TermSize_CBits.o -main-is Functions.TermSize -o TermSize
module Functions.TermSize where

import System.Posix.Types
import Foreign.C
import Foreign.Storable
import Foreign

#include <sys/ioctl.h>

#c
typedef struct winsize winsize;
int ioctl_TIOCGWINSZ(int fd, struct winsize *wsz);
#endc

data WinSize = WinSize 
    { ws_row    :: !CUShort
    , ws_col    :: !CUShort
    } deriving (Eq, Show)

{# pointer *winsize as WinSizePtr -> WinSize #}

instance Storable WinSize where
    sizeOf    _ = {# sizeof winsize #}
    alignment _ = {# sizeof winsize #}
    peek ptr = do
        r <- {#get winsize->ws_row   #} ptr
        c <- {#get winsize->ws_col   #} ptr
        return (WinSize r c)
    poke ptr (WinSize r c) = do
        {#set winsize->ws_row   #} ptr r
        {#set winsize->ws_col   #} ptr c

foreign import ccall "sys/ioctl.h ioctl" 
    ioctl :: Fd -> CULong -> Ptr a -> IO CInt

termSize :: Fd -> IO WinSize
termSize fd = alloca $ \wsz -> do
    throwErrnoIf (<0) "ioctl" $
        {#call ioctl_TIOCGWINSZ#} (fromIntegral fd) (wsz :: Ptr WinSize)
    
    peek wsz

main = do
    ws <- termSize 0
    
    print ws