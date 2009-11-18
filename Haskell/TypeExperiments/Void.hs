{-
 -      ``Void''
 -      (c) 2008 James Cook
 -}
{-# LANGUAGE
    EmptyDataDecls,
    FlexibleInstances,
    MultiParamTypeClasses
  #-}

module TypeExperiments.Void where

import Control.Monad.Cont hiding (lift)
import Data.StateRef

instance DefaultStateRef (IORef a) (ContT r IO) a

data Void
type Not c a = a -> c Void

lift :: a -> Not c (Not c a)
lift x c = c x

unlift :: Not c (Not c (Not c a)) -> Not c a
unlift x c = x (lift c)

force :: MonadCont c => Not c (Not c a) -> c a
force x = callCC $ \cont -> do
    void <- x cont
    void `seq` fail "took the blue pill"

type Func c a b = Not c (a, Not c b)

arr :: (a -> b) -> Func c a b
arr f (a, b) = b (f a)

eval :: Func c (Func c a b, a) b
eval ((f, a), b) = f (a, b)

eval' :: Func c a b -> a -> Not c (Not c b)
eval' f a c = f (a, c)

evalC :: MonadCont c => Func c a b -> a -> c b
evalC f a = force (eval' f a)

type CoFunc c a b = Either (Not c a) b

coArr :: (a -> b) -> Not c (Not c (CoFunc c a b))
coArr f c = c (Left (c . Right . f))

coeval :: Func c (Not c (Not c (CoFunc c a b)), a) b
coeval ((f, x), ret) = f (either ($x) ret)

coeval' :: Not c (Not c (CoFunc c a b)) -> a -> Not c (Not c b) 
coeval' f a = eval' coeval (f, a)

coevalC :: MonadCont c => Not c (Not c (CoFunc c a b)) -> a -> c b
coevalC f a = force (coeval' f a)

strictify :: Not c (Not c (CoFunc c a b)) -> Func c a b
strictify a (b, c) = a (either ($b) c)


lazify :: Func c a b -> Not c (Not c (CoFunc c a b))
lazify a b = b (Left (\ c -> a (c, \ d -> b (Right d))))
