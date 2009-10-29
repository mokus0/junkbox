{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    DeriveDataTypeable, RankNTypes, TypeOperators,
    ExistentialQuantification
  #-}
module Object where

import Data.IORef
import Data.Typeable
import Control.Monad.Trans

data Obj o = forall c. (c :< o) => Obj (o (Obj c))
type f :-> x = forall h. (h :< f) => f (Obj h) -> Obj h -> x

class f :< g where
    upcast   :: f (Obj h) -> g (Obj h)
    downcast :: g (Obj h) -> Maybe (f (Obj h))
    
(#) :: (f :< g) => Obj f -> (g :-> x) -> x
-- (#) :: (f :< g) => Obj f -> (g (Obj f) -> Obj f -> x) -> x
self@(Obj o) # f = f (upcast o) self

-- (##) :: Obj f -> (f :-> x) -> x
-- self@(Obj o) ## f = f o self

instance (:<) f f where
    upcast = id
    downcast = Just

data Ellipse a self = Ellipse
     { getWidth  :: self -> a
     , getHeight :: self -> a
     }

instance (:<) (Circle a) (Ellipse a) where
    upcast x = toEllipse x x
    downcast = const Nothing

data Circle a self = Circle
    { getRadius :: self -> a
    }

newCircle r = Obj $ Circle 
        { getRadius = \self -> r
        }

toEllipse c self = Ellipse
    { getWidth  = getRadius c
    , getHeight = getRadius c
    }


data Foo self = Foo
    { doFoo :: Int -> self -> Integer
    }

