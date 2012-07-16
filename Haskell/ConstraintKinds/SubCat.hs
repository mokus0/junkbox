{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ConstraintKinds.SubCat where

import Prelude hiding (id, (.))
import Control.Category

data SubCat cxt cat a b where
    Id  :: SubCat cxt cat a a
    Arr :: (cxt a, cxt b) => !(cat a b) -> SubCat cxt cat a b

wrap :: (cxt a, cxt b) => cat a b -> SubCat cxt cat a b
wrap = Arr

unwrap :: Category cat => SubCat cxt cat a b -> cat a b
unwrap Id      = id
unwrap (Arr f) = f

instance (Category cat, Eq (cat a b)) => Eq (SubCat cxt cat a b) where
    f == g = unwrap f == unwrap g

instance Category cat => Category (SubCat cxt cat) where
    id = Id
    f . Id = f
    Id . f = f
    Arr f . Arr g = Arr (f . g)

