{-
 -      ``Diagonal''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE 
        TypeFamilies,
        MultiParamTypeClasses,
        FunctionalDependencies,
        UndecidableInstances,
        FlexibleInstances,
        FlexibleContexts,
        KindSignatures,
        RankNTypes,
        EmptyDataDecls
  #-}

module TypeExperiments.Diagonal where

-- the idea:  need a typeclass with an operation 'dmap' of types:
--
-- dmap :: (a -> b) -> (a, a) -> (b, b)
-- dmap :: (a -> b) -> (a,a,a) -> (b,b,b)
-- dmap :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
--  &c.
--
-- how "cleanly" can this be done?
-- polykinded type variables would be nice ;-)

-- take 1: ghc panic (6.8.2)...
-- class Diagonal c d | c -> d, d -> c where
--         type DiagSrc c
--         type DiagDst d
--         dmap :: (DiagSrc c -> DiagDst d) -> c -> d
-- 
-- instance (a ~ a', b ~ b') => Diagonal (a,a') (b,b') where
--         type DiagSrc (a,a') = a
--         type DiagDst (b,b') = b
--         dmap f (a,b) = (f a, f b)

-- take 2: ghc panic again, quite possibly same reason...
-- class Diagonal d b where
--         type DiagBase d
--         type DiagSubst d b
--         dmap :: (DiagBase d -> b) -> d -> DiagSubst d b
-- 
-- instance (a ~ a') => Diagonal (a,a') b where
--         type DiagBase (a,a') = a
--         type DiagSubst (a,a') b = (b,b)
--         dmap f (a,b) = (f a, f b)

-- this one seems to work well (except it still panics ghc
-- 6.8.2) - its type could be really confusing to an
-- end-user though.
-- class Wrap d (w :: * -> *) a | d -> w a, w a -> d where
--         wrapDiag :: d -> w a
--         unWrapDiag :: w a -> d
-- 
-- newtype Pair a = Pair (a, a)
-- instance (a ~ b) => Wrap (a,b) Pair a where
--         wrapDiag = Pair
--         unWrapDiag (Pair x) = x
-- instance Functor Pair where
--         fmap f (Pair (a,b)) = Pair (f a, f b)
-- 
-- newtype Triple a = Triple (a, a, a)
-- instance (a ~ b, a ~ c) => Wrap (a,b,c) Triple a where
--         wrapDiag = Triple
--         unWrapDiag (Triple x) = x
-- instance Functor Triple where
--         fmap f (Triple (a,b,c)) = Triple (f a, f b, f c)
-- 
-- newtype Quadruple a = Quadruple (a, a, a, a)
-- instance (a ~ b, a ~ c, a ~ d) => Wrap (a,b,c,d) Quadruple a where
--         wrapDiag = Quadruple
--         unWrapDiag (Quadruple x) = x
-- instance Functor Quadruple where
--         fmap f (Quadruple (a,b,c,d)) = Quadruple (f a, f b, f c, f d)
-- 
-- newtype WrapEither a = WrapEither (Either a a)
-- instance (a ~ b) => Wrap (Either a b) WrapEither a where
--         wrapDiag = WrapEither
--         unWrapDiag (WrapEither x) = x
-- instance Functor WrapEither where
--         fmap f (WrapEither (Left  a)) = WrapEither (Left  (f a))
--         fmap f (WrapEither (Right a)) = WrapEither (Right (f a))
-- 
-- 
-- instance Wrap [a] [] a where
--         wrapDiag = id
--         unWrapDiag = id
-- 
-- dmap :: (Wrap c w a, Wrap d w b, Functor w) => (a -> b) -> c -> d
-- dmap f = unWrapDiag . fmap f . wrapDiag

-- type family Diag
-- type instance Diag (Pair a) = (a,a)

-- data family Diag a
-- data instance Diag a           = Diag1 a
-- data instance Diag (a,b)       = (a ~ b) => Diag2 a a
-- data instance Diag (a,b,c)     = (a ~ b, a ~ c) => Diag3 a a a
-- data instance Diag (a,b,c,d)   = (a ~ b, a ~ c, a ~ d) => Diag4 a a a a
-- data instance Diag (a,b,c,d,e) = (a ~ b, a ~ c, a ~ d, a ~ e) => Diag5 a a a a a

data family DWrap r b
type family Diag r b

class (Functor (DWrap r)) => Diagonal a r b | r b -> a, a -> r b where
        wrap :: a ~ Diag r b => a -> DWrap r b
        unWrap :: a ~ Diag r b => DWrap r b -> a

data Pair
data instance DWrap Pair c = Diag2 c c
type instance Diag Pair c = (c,c)
instance (a ~ b, a ~ c, Show a) => Diagonal (a,b) Pair c where
        wrap (a,b) = Diag2 a b
        unWrap (Diag2 a b) = (a,b)

instance Functor (DWrap Pair) where
        fmap f (Diag2 a b) = Diag2 (f a) (f b)

data Triple
data instance DWrap Triple c = Diag3 c c c
type instance Diag Triple c = (c,c,c)
instance (a ~ b, a ~ c, a ~ d, Show a) => Diagonal (a,b,c) Triple d where
        wrap (a,b,c) = Diag3 a b c
        unWrap (Diag3 a b c) = (a,b,c)

instance Functor (DWrap Triple) where
        fmap f (Diag3 a b c) = Diag3 (f a) (f b) (f c)

instance Show c => Show (DWrap Pair c) where
        showsPrec p (Diag2 a b) = showParen (p > 10) showDiag
                where   showDiag = showString "Diag2 "
                                 . showsPrec 11 a
                                 . showChar ' '
                                 . showsPrec 11 b
-- dmap :: ( Diagonal c r (Base c), Diagonal d r (Base d)
--         , a ~ Base c
--         , b ~ Base d
--         ) => (a -> b) -> c -> d
dmap f x = unWrap (fmap f (wrap x))