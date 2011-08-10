{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- The "usual story," at least as I've heard it implied a few times, 
-- is that the 'Monad' class functions (excluding 'fail', of course) 
-- are based on encoding the Kleisli category.  That's not exactly 
-- true - 'P.return' is indeed 'id', but '.' is 'P.<=<', not 'P.>>='.
--
-- The full isomorphism is given here.
module Monads.Kleisli where

import Prelude hiding (id, (.), Monad(..))
import qualified Control.Monad as P
import Control.Category

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

-- > instance Category (Kleisli m) => Monad m where ...

return :: Category (Kleisli m) => a -> m a
return = runKleisli id

(>>=) :: Category (Kleisli m) => m a -> (a -> m b) -> m b
x >>= f = runKleisli (Kleisli f . Kleisli id) x

-- these two are included so -XRebindableSyntax will work
(>>) :: Category (Kleisli m) => m a -> m b -> m b
x >> y = x >>= const y

fail :: Category (Kleisli m) => String -> m a
fail = error

-- some related functions
bind :: Category (Kleisli m) => (a -> m b) -> (m a -> m b)
bind = flip (>>=)

liftM :: Category (Kleisli m) => (a -> b) -> m a -> m b
liftM f = bind (return . f)

join :: Category (Kleisli m) => m (m a) -> m a
join = bind id


-- forward a few Monad instances
instance Category (Kleisli IO) where
    id = Kleisli (P.return)
    Kleisli f . Kleisli g = Kleisli (f P.<=< g)

instance Category (Kleisli Maybe) where
    id = Kleisli (P.return)
    Kleisli f . Kleisli g = Kleisli (f P.<=< g)

instance Category (Kleisli []) where
    id = Kleisli (P.return)
    Kleisli f . Kleisli g = Kleisli (f P.<=< g)

