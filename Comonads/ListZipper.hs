{-
 -      ``ListZipper''
 -      (c) 2009 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances,
    UndecidableInstances
  #-}

module ListZipper where

import Control.Functor.Categorical
import Data.List

data Z f a = LZ (f a) a (f a)
    deriving (Eq, Show)

instance CFunctor f (->) (->) => CFunctor (Z f) (->) (->) where
    cmap f (LZ ls x rs) = LZ (cmap f ls) (f x) (cmap f rs)

instance CFunctor f (->) (->) => CCopointed (Z f) (->) where
    cextract (LZ _ x _) = x

instance CExtend (Z []) (->) where
    cduplicate lz@(LZ inl x inr) = LZ ls lz rs
        where
            ls = tail
                [ LZ ls x (reverse rs' ++ inr)
                | (rs', x:ls) <- splits (x:inl)
                ]
            
            rs = tail
                [ LZ (reverse ls' ++ inl) x rs
                | (ls', x:rs) <- splits (x:inr)
                ]

instance CComonad (Z []) (->)

splits xs = zip (inits xs) (tails xs)

instance CDistributes (Z []) [] (->) where
    cdist (LZ lss xs rss) =
        [ LZ (concat lss) x (concat rss)
        | x <- xs
        ]

