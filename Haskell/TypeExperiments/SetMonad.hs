{-
 -      ``SetMonad''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        GADTs
  #-}

module TypeExperiments.SetMonad where

import qualified Data.Set as S

data SetM a where
        SetM :: Ord a => a -> SetM (S.Set a)

instance Functor SetM where
        fmap f (SetM s) = SetM (S.map f s)