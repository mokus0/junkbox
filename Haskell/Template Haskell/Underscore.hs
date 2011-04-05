{-# LANGUAGE TemplateHaskell #-}
module Underscore where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

underscore = QuasiQuoter
    { quoteExp = underscoreE
    , quotePat = const [p| _ |]
    , quoteType = const underscoreT
    , quoteDec = undefined
    }

underscoreE msg = [| error msg |]
underscoreT = do
    n <- newName "_"
    varT n
