{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, RankNTypes #-}
module TypeExperiments.TwoCat where

import Prelude hiding (id, (.))
import Control.Category
import Control.Functor.Categorical

class (Category mor) => TwoCat (mor :: * -> * -> *) (nat :: (* -> *) -> (* -> *) -> *) | nat -> mor where
    toNat :: (forall t. mor (f t) (g t)) -> nat f g
    fromNat :: nat f g -> mor (f t) (g t)
    
    (∙) :: nat g h -> nat f g -> nat f h
    s ∙ t = toNat (fromNat s . fromNat t) 
    
    -- is this the other composition's type ???
    -- (∘) :: nat f g -> nat f g -> nat f g
