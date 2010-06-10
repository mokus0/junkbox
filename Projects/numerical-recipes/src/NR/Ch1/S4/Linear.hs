{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module NR.Ch1.S4.Linear where

class Linear m a where
    liftLinear  :: (a -> a) -> m a -> m a
    liftLinear2 :: (a -> a -> a) -> m a -> m a -> m a

scale k = liftLinear (k*)
addL a b = liftLinear2 (+) a b
subL a b = liftLinear2 (-) a b
mulL a b = liftLinear2 (*) a b
divL a b = liftLinear2 (/) a b

instance Linear [] t
    where
        liftLinear = fmap
        liftLinear2 = zipWith

