{-# LANGUAGE
        TypeFamilies,
        ScopedTypeVariables,
        TypeSynonymInstances,
        FlexibleContexts, FlexibleInstances, UndecidableInstances,
        MultiParamTypeClasses,
        NoMonomorphismRestriction
  #-}
{-
 - A rough sketch of a "region system" a bit like the one in Disciple, implemented
 - in Haskell.  Types parameterized by multiple regions are possible as well, but
 - require either more type hackery or more boilerplate than I'm interested in
 - messing with right now
 -}
module Regions where

import Prelude hiding (read)
import Control.Monad.Identity
import Control.Monad.ST
import Control.Monad.Fix
import Control.Monad
import Data.STRef

class Monad m => Region m r where
    inject :: a -> m (r a)
    read   :: r a -> m a

class Region m r => Mutable m r where
    write :: a -> r a -> m ()

class Region m r => Const m r
instance (Region m r, Pure r)  => Const m r

class    (Region Identity r) => Pure r
instance (Region Identity r) => Pure r

data    Lazy   t = Lazy t
newtype Strict t = Strict t

newtype Ref str s t = Mutable (STRef s (str t))

instance (Monad m) => Region m Lazy where
    inject x = return (Lazy x)
    read (Lazy x) = return x

instance (Monad m) => Region m Strict where
    inject x = return (Strict x)
    read (Strict x) = return x

instance Region (ST s) str => Region (ST s) (Ref str s) where
    inject x = do
        x <- inject x
        x <- newSTRef x
        return (Mutable x)
    read (Mutable x) = do
        x <- readSTRef x
        read x

instance Region (ST s) str => Mutable (ST s) (Ref str s) where
    write x r@(Mutable ref) = do
        x <- inject x
        writeSTRef ref x

newtype RMu r t = RMu (r (t (RMu r t)))
type Struct r t = RMu r (Structure t r)

class Structured t where
    data Structure t :: (* -> *) -> * -> *
    compose   :: Region m r => Struct r t -> m t
    decompose :: Region m r => t -> m (Struct r t)

instance Structured Int where
    newtype Structure Int r x = Int (r Int)
    compose (RMu i) = do
        Int i <- read i
        read i
    decompose i = do
        i <- inject i
        i <- inject (Int i)
        return (RMu i)

instance Structured Bool where
    newtype Structure Bool r x = SBool Bool
    compose (RMu x) = do
        SBool x <- read x
        return x
    decompose x = do
        x <- inject (SBool x)
        return (RMu x) 

instance Structured a => Structured (Maybe a) where
    data Structure (Maybe a) r x 
        = MbJust !(Struct r a)
        | MbNothing
    compose (RMu x) = read x >>= \x -> case x of
        MbJust x  -> liftM Just (compose x)
        MbNothing -> return Nothing
    decompose (Just x)  = do
        x <- decompose x
        x <- inject (MbJust x)
        return (RMu x)
    decompose Nothing   = do
        x <- inject MbNothing
        return (RMu x)

inplace f x = do
    xx <- read x
    write (f xx) x

data Foo = Foo
    { fooInt    :: Int
    , fooBar    :: Bar
    } deriving (Eq, Show)

instance Structured Foo where
    data Structure Foo r x = SFoo
        { sFooInt :: !(Struct r Int)
        , sFooBar :: !(Struct r Bar)
        }
    compose (RMu x) = do
        SFoo i b <- read x
        i <- compose i
        b <- compose b
        return  (Foo i b)
    decompose (Foo i b) = do
        i <- decompose i
        b <- decompose b
        x <- inject (SFoo i b)
        return (RMu x)

data Bar = Bar
    { barBool   :: Bool
    , barFoo    :: Maybe Foo
    } deriving (Eq, Show)

instance Structured Bar where
    data Structure Bar r x = SBar
        { sBarBool  :: !(Struct r Bool)
        , sBarFoo   :: !(Struct r (Maybe Foo))
        }
    compose (RMu x) = do
        SBar b f <- read x
        b <- compose b
        f <- compose f
        return  (Bar b f)
    decompose (Bar b f) = do
        b <- decompose b
        f <- decompose f
        x <- inject (SBar b f)
        return (RMu x)

-- composable deep-structure manipulators
withBarFoo f (RMu x) = do
    SBar b foo <- read x
    mapMaybeS f foo

mapMaybeS f (RMu mbx) = do
    mbx <- read mbx
    case mbx of 
        MbNothing -> return ()
        MbJust x  -> do
            f x

withFooInt f (RMu x) = do
    SFoo i _ <- read x
    f i

withFooBar f (RMu x) = do
    SFoo _ b <- read x
    f b

withBarFooInt = withBarFoo . withFooInt

-- example, using "recursive lvalue":
withAllFoosInFoo g = fix (\f foo -> g foo >> withFooBar (withBarFoo f) foo)
succAllIntsInFoo = withAllFoosInFoo . withFooInt $ \(RMu x) -> read x >>= \(Int x) -> inplace succ x
x = Foo 1 (Bar True (Just (Foo 2 (Bar True Nothing))))
x' = runST $ do {x <- decompose x :: ST s (Struct (Ref Strict s) Foo); succAllIntsInFoo x; compose x}
