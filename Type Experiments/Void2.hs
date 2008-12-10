{-
 -      ``Void''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        EmptyDataDecls,
        FlexibleContexts,
        FlexibleInstances,
        MultiParamTypeClasses
  #-}

module Void where

import Control.Monad.Cont hiding (lift)

import Data.MRef

data Void
type Not c a = a -> c Void

lift :: a -> Not c (Not c a)
lift x = ($x)

unlift :: Not c (Not c (Not c a)) -> Not c a
unlift a c = a (lift c)

force :: MonadCont c => Not c (Not c a) -> c a
force x = callCC $ \cont -> do
        void <- x cont -- "can not" return
        void `seq` fail "how on earth did you manage to reach this error message?"

-- run :: MonadCont c => Func c a b -> a -> c b
-- run f x = force (preEval f x)

instance DefaultMRef (MVar a) (ContT r IO) a
lazy :: ( NewMRef      mr m (Maybe a)
         
         , DefaultMRef  mr c (Maybe a)
         , TakeMRef     mr c (Maybe a)
         , PutMRef      mr c (Maybe a)
         ) =>
         c a -> m (Not c (Not c a))
lazy x = do
        value <- newMRef Nothing
        
        let evaluate cont = do
                v <- takeDefaultMRef value
                x <- maybe x return v
                putMRef value (Just x)
                
                cont x
        
        return evaluate

byName :: (Monad c, Monad m) => c a -> m (Not c (Not c a))
byName x = return (\cont -> x >>= cont)

byVal :: (Monad c) => c a -> c (Not c (Not c a))
byVal x = x >>= return.lift

type Func c a b = Not c (a, Not c b)

mkFunc :: (a -> b) -> Func c a b
mkFunc f (a, b) = b (f a)

eval :: Not c (Func c a b, a, Not c b)
eval (f, x, b) = f (x, b)

compose :: Not c (Func c x y, Func c y z, Not c (Func c x z))
compose (f,g,c) = c (\(a,c) -> eval(f, a, \b -> eval(g, b, c)))
