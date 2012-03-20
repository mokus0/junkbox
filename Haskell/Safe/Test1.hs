{-# LANGUAGE Safe #-}
-- should fail
module Safe.Test1 where

import Unsafe.Coerce

achoo = unsafeCoerce 123

main = putStrLn achoo