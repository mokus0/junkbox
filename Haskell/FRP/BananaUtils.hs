module FRP.BananaUtils where

import Control.Arrow
import Data.Either
import Reactive.Banana
-- interesting combinators reactive-banana could easily be extended with...
-- (presumably using some more-efficient implementations)

partitionE :: (a -> Bool) -> Event t a -> (Event t a, Event t a)
partitionE p = filterE p &&& filterE (not . p)

partitionEithersE :: Event t (Either a b) -> (Event t a, Event t b)
partitionEithersE = (filterJust . fmap (either Just nothing)) &&& (filterJust . fmap (either nothing Just))
    where nothing = const Nothing

