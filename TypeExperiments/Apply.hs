{-
 -      ``Apply''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        TypeFamilies,
        FlexibleInstances,
        FlexibleContexts,
        MultiParamTypeClasses,
        FunctionalDependencies,
        UndecidableInstances,
        OverlappingInstances,
        IncoherentInstances,
        RankNTypes
  #-}

module TypeExperiments.Apply where

import Prelude hiding (curry, uncurry, map, ($), (.))
import qualified Prelude

import Control.Arrow
import qualified Data.Map as M

-- objective here: write a very generic 'curry' operation, which 'flattens' such types as:
-- (a,b) -> c
-- (a,b,c) -> (d,e) -> f
-- a -> (b, c) -> e

-- a corresponding "uncurry" would be a nice bonus, but I suspect it would be even harder.

-- I'm also using this as a scratch pad for some other stuff while learning to use type families.

class Apply f where
        type Antecedent f
        type Consequent f
        ($) :: f -> Antecedent f -> Consequent f

class ( Apply f
      , Apply g
      , Consequent f ~ Antecedent g
      , Apply (Composed f g)
      
      , Antecedent f ~ Antecedent (Composed f g)
      , Consequent g ~ Consequent (Composed f g)
      ) => Compose f g where
        
        type Composed f g
        (#) :: f -> g -> Composed f g
        (#) = flip (.)
        (.) :: g -> f -> Composed f g
        (.) = flip (#)

instance Apply (a -> b) where
        type Antecedent (a -> b) = a
        type Consequent (a -> b) = b
        f $ x = f x

instance (Ord k) => Apply (M.Map k v) where
        type Antecedent (M.Map k v) = k
        type Consequent (M.Map k v) = Maybe v
        ($) = flip M.lookup

instance (Apply a, Apply b, Antecedent a ~ Antecedent b, Consequent a ~ Consequent b) => Apply (Either a b) where
        type Antecedent (Either a b) = Antecedent a
        type Consequent (Either a b) = Consequent a
        Left f $ x = f $ x
        Right f $ x = f $ x

instance (Apply a, Apply b) => Apply (a,b) where
        type Antecedent (a,b) = (Antecedent a, Antecedent b)
        type Consequent (a,b) = (Consequent a, Consequent b)
        (f,g) $ (x,y) = (f $ x, g $ y)

newtype Diag a = Diag a

instance (Apply a, Apply b, Consequent a ~ Consequent b) => Apply (Diag (Either a b)) where
        type Antecedent (Diag (Either a b)) = (Antecedent a, Antecedent b)
        type Consequent (Diag (Either a b)) = Consequent a
        Diag (Left f)  $ (x,_) = f $ x
        Diag (Right f) $ (_,x) = f $ x

instance (Apply a, Apply b, Antecedent a ~ Antecedent b) => Apply (Diag (a,b)) where
        type Antecedent (Diag (a,b)) = Antecedent a
        type Consequent (Diag (a,b)) = (Consequent a, Consequent b)
        Diag (f,g) $ x = (f $ x, g $ x)

newtype Map a = Map a
map :: a -> Map a
map = Map

instance Apply a => Apply (Map a) where
        type Antecedent (Map a) = [Antecedent a]
        type Consequent (Map a) = [Consequent a]
        ($) (Map f) = Prelude.map (f $)
        

instance Monad m => Apply (Kleisli m a b) where
        type Antecedent (Kleisli m a b) = m a
        type Consequent (Kleisli m a b) = m b
        f $ x = x >>= runKleisli f

instance (Apply f, Apply g, Consequent f ~ Antecedent g) => Compose f g where
        type Composed f g = Antecedent f -> Consequent g
        f # g = \x -> g $ (f $ x)

class Product p where
        type ProdFst p
        type ProdRest p
        split :: p -> (ProdFst p, ProdRest p)
        prod :: ProdFst p -> ProdRest p -> p

instance Product (a,b) where
        type ProdFst (a,b) = a
        type ProdRest (a,b) = b
        split (a,b) = (a,b)
        prod = (,)

instance Product (a,b,c) where
        type ProdFst (a,b,c) = a
        type ProdRest (a,b,c) = (b,c)
        split (a,b,c) = (a,(b,c))
        prod a (b,c) = (a,b,c)

instance Product (a,b,c,d) where
        type ProdFst (a,b,c,d) = a
        type ProdRest (a,b,c,d) = (b,c,d)
        split (a,b,c,d) = (a,(b,c,d))
        prod a (b,c,d) = (a,b,c,d)

instance Product (a,b,c,d,e) where
        type ProdFst (a,b,c,d,e) = a
        type ProdRest (a,b,c,d,e) = (b,c,d,e)
        split (a,b,c,d,e) = (a,(b,c,d,e))
        prod a (b,c,d,e) = (a,b,c,d,e)

-- woo, this one bricks the compiler ;-)
--   (it should fail to compile, but it should fail more gracefully,
--    because there is no instance for (Curry (ProdRest a -> b)).)
--class Curry a where
--        type Curried a
--        curry :: a -> Curried a
--        uncurry :: Curried a -> a
--
--instance Product a => Curry (a -> b) where
--        type Curried (a -> b) = ProdFst a -> Curried (ProdRest a -> b)
--        curry f a = curry (f . prod a)


-- this seems like it should work, but it doesn't.
class Curry a b where
        curry :: a -> b
        uncurry :: b -> a

instance Curry (a -> b) (a -> b) where
        curry = id
        uncurry = id

instance (Product a, Curry (ProdRest a -> b) c) => Curry (a -> b) (ProdFst a -> c) where
        curry f a = curry (f . prod a)
        uncurry f p = case split p of
                (hd, rest) -> uncurry (f hd) rest

