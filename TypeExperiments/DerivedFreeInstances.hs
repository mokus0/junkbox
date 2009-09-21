{-
 -      ``DerivedFreeInstances''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        GADTs
  #-}

module TypeExperiments.DerivedFreeInstances where

data FreeEnum
  = Succ FreeEnum
  | Pred FreeEnum
  | ToEnum Int
--  | FromEnum FreeEnum
--  | EnumFrom FreeEnum
--  | EnumFromThen FreeEnum FreeEnum
--  | EnumFromTo FreeEnum FreeEnum
--  | EnumFromThenTo FreeEnum FreeEnum FreeEnum
  deriving (Eq, Show)

foldEnum s p t (Succ e)   = s (foldEnum s p t e)
foldEnum s p t (Pred e)   = p (foldEnum s p t e)
foldEnum s p t (ToEnum n) = t n

instance Enum FreeEnum where
        -- easily derivable (types ending in (-> a))
        succ = Succ
        pred = Pred
        toEnum = ToEnum
        
        -- not really derivable without some adjunction assumption
        -- (where Succ |- (succ :: Int -> Int), Pred |- (pred :: Int -> Int),
        -- etc, or is it vice versa?)
        fromEnum = foldEnum succ pred toEnum
        
        -- others are not derivable at all without knowledge of laws of the type;
        -- however, they have defaults.  Technically, they need not be a part of
        -- the class at all.


-- a rough cut at a GADT-based version, to help me grok the type-space
data FreeEnumG a where
        SuccG           :: FreeEnumG a                                  -> FreeEnumG a
        PredG           :: FreeEnumG a                                  -> FreeEnumG a
        ToEnumG         :: Int                                          -> FreeEnumG a
        FromEnumG       :: FreeEnumG a                                  -> FreeEnumG Int
        EnumFromG       :: FreeEnumG a                                  -> FreeEnumG [a]
        EnumFromThenG   :: FreeEnumG a -> FreeEnumG a                   -> FreeEnumG [a]
        EnumFromToG     :: FreeEnumG a -> FreeEnumG a                   -> FreeEnumG [a]
        EnumFromThenToG :: FreeEnumG a -> FreeEnumG a -> FreeEnumG a    -> FreeEnumG [a]

foldEnumG 
        :: (b -> b) -- s
        -> (b -> b) -- p
        -> (Int -> b) -- t
        -> d -- f
        -> e -- ef
        -> h -- eft
        -> i -- et
        -> j -- eftt
        -> FreeEnumG a
        -> b
----
---- At the very least, a generic fold operation for GADTs is not going to be easy to create;
----   I'm not sure, but it may not even be possible to do in a totally type-sane way.
---- 
foldEnumG s p t f ef eft et eftt (SuccG e) = s (foldEnumG s p t f ef eft et eftt e)
foldEnumG s p t f ef eft et eftt (PredG e) = p (foldEnumG s p t f ef eft et eftt e)
foldEnumG s p t f ef eft et eftt (ToEnumG i) = t i

-- these ones are particularly problematic: (FromEnumG destroys information about the previous type, and the others require deconstruction of b)
-- foldEnumG s p t f ef eft et eftt (FromEnumG e) = f (foldEnumG s p t f ef eft et eftt e)
-- foldEnumG s p t f ef eft et eftt (EnumFromG e) = ef (foldEnumG s p t f ef eft et eftt e)
-- foldEnumG s p t f ef eft et eftt (EnumFromThenG e1 e2) = eft (foldEnumG s p t f ef eft et eftt e1) (foldEnumG s p t f ef eft et eftt e2)


unFreeEnumG :: FreeEnumG a -> a
unFreeEnumG = undefined

commuteEnumG :: FreeEnumG (f a) -> f (FreeEnumG a)
commuteEnumG = undefined

instance Enum (FreeEnumG a) where
        succ                    = SuccG
        pred                    = PredG
        toEnum                  = ToEnumG
        fromEnum a              = unFreeEnumG  $ FromEnumG a
        enumFrom a              = commuteEnumG $ EnumFromG a
        enumFromThen a b        = commuteEnumG $ EnumFromThenG a b
        enumFromTo a b          = commuteEnumG $ EnumFromToG a b
        enumFromThenTo a b c    = commuteEnumG $ EnumFromThenToG a b c
