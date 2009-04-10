{-
 -      ``SubType''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    TypeOperators,
    MultiParamTypeClasses,
    FlexibleInstances
  #-}

module SubType where

class a :> b where
    up   :: b -> a
    down :: a -> Maybe b

instance (:>) a a where
    up = id
    down = Just

instance (a1 :> a2, b1 :> b2) => (:>) (a1,b1) (a2,b2) where
    up (a,b) = (up a, up b)
    down (a,b) = do
        a <- down a
        b <- down b
        return (a,b)

instance (a1 :> a2, b1 :> b2) => (:>) (Either a1 b1) (Either a2 b2) where
    up (Left a ) = Left  (up a)
    up (Right b) = Right (up b)
    down (Left a ) = fmap Left  (down a)
    down (Right b) = fmap Right (down b)

instance (:>) a (a,b) where
    up (a,b) = a
    down = const Nothing

instance (:>) b (a,b) where
    up (a,b) = b
    down = const Nothing

instance (:>) [a] (Maybe a)
    where
        up (Nothing) = []
        up (Just x)  = [x]
        down [] = Just Nothing
        down [x] = Just (Just x)
        down _ = Nothing