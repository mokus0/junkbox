{-# LANGUAGE GADTs #-}
module Math.STLC where

import qualified Data.Map as M
import Data.Monoid

data Type b
    = BaseType b
    | Arrow (Type b) (Type b)
    deriving (Eq, Show)

instance Functor Type where
    fmap f (BaseType b) = BaseType (f b)
    fmap f (Arrow a b) = Arrow (fmap f a) (fmap f b)

data Term k v
    = Const k
    | Var v
    | Abs v (Term k v)
    | App (Term k v) (Term k v)

data Context b k v = Cxt
    { constType :: k -> Maybe (Type b)
    , varTypes  :: M.Map v (Type b)
    }

emptyCxt f = Cxt f M.empty
extend v ty cxt = cxt {varTypes = M.insert v ty (varTypes cxt)}
liftCxt :: Context b k v -> Context (Maybe b) k v
liftCxt (Cxt f m) = Cxt (fmap (fmap Just) . f) (M.map (fmap Just) m)

newtype Subst k v = Subst (M.Map v k)

check :: (Eq b, Ord v) => Context b k v -> Term k v -> Type b -> Bool
check cxt (Const   k) ty = infer cxt (Const k) == Just ty
check cxt (Var     v) ty = infer cxt (Var   v) == Just ty
check cxt (Abs  v  e) (Arrow vt et) = check (extend v vt cxt) e et
check cxt (App e1 e2) ty = case infer cxt e2 of
    Nothing   -> False
    Just e2ty -> check cxt e1 (Arrow e2ty ty)
check cxt _ _ = False

-- this strategy isn't gonna work.  Need to introduce unification type variables
infer :: Ord v => Context b k v -> Term k v -> Maybe (Type b)
infer cxt (Const   k) = constType cxt k
infer cxt (Var     v) = M.lookup v (varTypes cxt)
infer cxt (Abs  v  e) = undefined
infer cxt (App e1 e2) = undefined