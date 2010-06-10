{-# LANGUAGE
        MultiParamTypeClasses, FunctionalDependencies,
        FlexibleInstances
  #-}

-- |Typed labels - even supports computed \"goto\"!
module Acme.Goto (Label(..), goto) where

import System.Exit

class Label l t | l -> t where
    -- |Call the specified label, returning to the caller.
    call :: l -> t

-- |Class of computation types (typically 'Monad's)
-- supporting 'goto' - i.e., call without return.
class GoTo m where
    -- |Call the specified label without returning to the caller.
    goto :: Label l (m a) => l -> m b

instance GoTo IO where
    goto l = do
        call l
        exitWith ExitSuccess

-- example usage:

-- a standard label:
data Foo = Foo

instance Label Foo (IO ()) where
    call Foo = do
        putStrLn "Foo called."
        goto (Bar 10)


-- a computed label (note the statically-enforced boundedness of the jump!):
data Bar = Bar Int

instance Label Bar (IO ()) where
    call (Bar n) = case n of
        10  -> putStrLn "Bar 10!"
        _   -> do
            putStrLn "Invalid Bar!  going to Foo!"
            goto Foo

main = do
    goto (Bar 24)
    putStrLn "dead code!"