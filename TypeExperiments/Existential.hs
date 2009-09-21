{-
 -      ``Existential''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    GADTs, KindSignatures, FlexibleInstances, UndecidableInstances
  #-}

module TypeExperiments.Existential where

-- experiments with GADTs

class Foo a where
    foo :: a -> String

class Bar a where
    bar :: a -> Int

class Baz a where
    baz :: a -> [String]

data FooE where
    FooE :: Foo a => a -> FooE

data FooD a where FooD :: Foo a => FooD a
data BarD a where BarD :: Bar a => BarD a
data BazD a where BazD :: Baz a => BazD a

instance Show a => Foo a where foo = show
instance Bar [a] where bar = length
instance Baz Int where baz n = replicate (n `mod` 23) (show n)

f :: Maybe (FooD a) -> Maybe (BarD a) -> Maybe (BazD a) -> a -> IO ()
f a b c x = mapM_ putStrLn
    [ x
    | Just x <- [showFoo a x, showBar b x, showBaz c x]
    ]
    
    where
        showFoo :: Maybe (FooD a) -> a -> Maybe String
        showFoo Nothing     x = Nothing
        showFoo (Just FooD) x = Just (foo x)
        
        showBar :: Maybe (BarD a) -> a -> Maybe String
        showBar Nothing     x = Nothing
        showBar (Just BarD) x = Just (replicate (bar x) '!')
        
        showBaz :: Maybe (BazD a) -> a -> Maybe String
        showBaz Nothing     x = Nothing
        showBaz (Just BazD) x = Just (unlines [ '\t' : line | line <- baz x])


class Eep a where
    eep :: a -> String

data EepD where
    EepD1 :: Floating  t => t -> EepD
    EepD2 :: Integral  t => t -> EepD
    
instance Eep EepD where
    eep (EepD1 n) = show (n / pi)
    eep (EepD2 n) = show (n `divMod` 42)

-- eitherEep :: 
eitherEep (Left n)  = EepD1 n
eitherEep (Right n) = EepD2 n
