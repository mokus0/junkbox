{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
{-
 -      ``Category''
 -      (c) 2008 James Cook
 -}

module TypeExperiments.Category where

import Data.List

class Category o a | a -> o where
        dom :: a -> o
        cod :: a -> o
        identity :: o -> a
        compose :: a -> a -> a

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- matrices as a category with Int objects (numbers of rows and columns) and
-- multiplication as composition
instance Num a => Category Int [[a]] where
        dom xss = length xss
        cod xss = minimum (map length xss)
        identity n = [[if r == c then 1 else 0 | c <- [1..n]] | r <- [1..n]]
        compose xss yss
                | cod xss == dom yss
                = [[r `dot` c | c <- transpose yss] | r <- xss]
                | otherwise
                = error "matrices not composable"

-- product category
instance (Category o1 a1, Category o2 a2) => Category (o1, o2) (a1, a2) where
        dom (f,g) = (dom f, dom g)
        cod (f,g) = (cod f, cod g)
        identity (c,d) = (identity c, identity d)
        compose (f1,g1) (f2,g2) = (compose f1 f2, compose g1 g2)

-- coproduct category
instance (Category o1 a1, Category o2 a2) => Category (Either o1 o2) (Either a1 a2) where
        dom = either (Left . dom) (Right . dom)
        cod = either (Left . cod) (Right . cod)
        identity = either (Left . identity) (Right . identity)
        compose (Left f) (Left g) = Left (compose f g)
        compose (Right f) (Right g) = Right (compose f g)
        compose _ _ = error "arrows not composable"
        
-- endofunction category; general case not expressible here
instance Category () (a->a) where
        dom f = ()
        cod f = ()
        identity f = id
        compose f g = f . g

-- endofunctor category; general case not expressible here
instance (Category o a) => Category (o -> o, a -> a) ((o -> o, a -> a), (o -> o, a -> a), o -> a) where
        dom (f,g,tau) = f
        cod (f,g,tau) = g
        identity f = (f,f, \o1 -> identity (fst f o1))
        compose (f, _, tau1) (_, g, tau2) = (f,g, tau2 . fst f) -- if taus are really natural, then tau2 . fst f == snd g . tau1

-- "+1" category - adds a distinct, totally disconnected, object to another category
instance (Category o a) => Category (Maybe o) (Maybe a) where
        dom = maybe Nothing (Just . dom)
        cod = maybe Nothing (Just . cod)
        identity = maybe Nothing (Just . identity)
        compose Nothing Nothing = Nothing
        compose (Just f) (Just g) = Just (compose f g)

-- one-object category
data C1o = C1o deriving (Eq, Show)
data C1a = C1a deriving (Eq, Show)

instance Category C1o C1a where
        dom C1a = C1o
        cod C1a = C1o
        identity C1o = C1a
        compose C1a C1a = C1a
