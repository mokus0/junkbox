{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}
module TypeExperiments.Partial 
    ( Tag , newTag
    , Partial(..)
    , isComplete, extract
    , fill, fillMany
    ) where

import TypeExperiments.Dependent
import TypeExperiments.Uniq
import Unsafe.Coerce
import Control.Monad
import Control.Monad.Primitive

data Tag s a = Tag (Uniq s)
    deriving (Eq, Ord)

newTag :: PrimMonad m => m (Tag (PrimState m) a)
newTag = liftM Tag getUniq

data Partial s thing where
    Constructor ::       a -> Partial s a
    EmptySlot   :: Tag s a -> Partial s (a -> b) -> Partial s b
    FilledSlot  ::       a -> Partial s (a -> b) -> Partial s b
    Option      :: Tag s Int -> [Partial s a] -> Partial s a

isComplete :: Partial s a -> Bool
isComplete Constructor{} = True
isComplete (FilledSlot _ rest) = isComplete rest
isComplete _ = False

extract :: Partial s a -> Maybe a
extract (FilledSlot a rest) = fmap ($a) (extract rest)
extract (Constructor a) = Just a
extract _ = Nothing

fill :: Tag s a -> a -> Partial s thing -> Partial s thing
fill tag@(Tag u0) thing (Option (Tag u1) opts)
    | u0 == u1  = fill tag thing (opts !! unsafeCoerce thing)
fill tag thing (Option optTag opts) = Option optTag (map (fill tag thing) opts)
fill tag@(Tag u0) thing (EmptySlot (Tag u1) rest)
    | u0 == u1  = FilledSlot (unsafeCoerce thing)           (fill tag thing rest)
fill tag thing (EmptySlot slotTag rest) = EmptySlot slotTag (fill tag thing rest)
fill tag thing (FilledSlot      x rest) = FilledSlot      x (fill tag thing rest)
fill tag thing other = other

fillMany :: [DSum (Tag s)] -> Partial s thing -> Partial s thing
fillMany [] = id
fillMany ((DSum tag thing):rest) = fillMany rest . fill tag thing

partialMaybe = do
    justTag <- newTag
    optTag <- newTag
    let partial = Option optTag [Constructor Nothing, EmptySlot justTag (Constructor Just)]
    return (optTag, justTag, partial)

partialEither = do
    optTag <- newTag
    lTag <- newTag
    rTag <- newTag
    let partial = Option optTag [EmptySlot lTag (Constructor Left), EmptySlot rTag (Constructor Right)]
    return (optTag, lTag, rTag, partial)