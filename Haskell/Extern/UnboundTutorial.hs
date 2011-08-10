{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
  #-}
import Unbound.LocallyNameless
import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error

import Text.Parsec hiding ((<|>), Empty)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>))
data Term = Var (Name Term)
          | App Term Term
          | Lam (Bind (Name Term) Term)
  deriving Show
$(derive [''Term])
instance Alpha Term
instance Subst Term Term where
  isvar (Var v) = Just (SubstName v)
  isvar _       = Nothing
lam :: String -> Term -> Term
lam x t = Lam $ bind (string2Name x) t

var :: String -> Term
var = Var . string2Name
-- A convenient synonym for mzero
done :: MonadPlus m => m a
done = mzero

step :: Term -> MaybeT FreshM Term
step (Var _) = done
step (Lam _) = done
step (App (Lam b) t2) = do
  (x,t1) <- unbind b
  return $ subst x t2 t1
step (App t1 t2) =
      App <$> step t1 <*> pure t2
  <|> App <$> pure t1 <*> step t2
tc :: (Monad m, Functor m) => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
  ma' <- runMaybeT (f a)
  case ma' of
    Just a' -> tc f a'
    Nothing -> return a

eval :: Term -> Term
eval x = runFreshM (tc step x)
lexer    = P.makeTokenParser haskellDef
parens   = P.parens lexer
brackets = P.brackets lexer
ident    = P.identifier lexer

parseTerm = parseAtom `chainl1` (pure App)

parseAtom = parens parseTerm
        <|> var <$> ident
        <|> lam <$> (brackets ident) <*> parseTerm

runTerm :: String -> Either ParseError Term
runTerm = (id +++ eval) . parse parseTerm ""
class Pretty' p where
  ppr' :: (Applicative m, Fresh m) => p -> m Doc

instance Pretty' Term where
  ppr' (Var x)     = return . PP.text . show $ x
  ppr' (App t1 t2) = PP.parens <$> ((<+>) <$> ppr' t1 <*> ppr' t2)
  ppr' (Lam b)     = do
    (x, t) <- unbind b
    ((PP.brackets . PP.text . show $ x) <+>) <$> ppr' t
class Pretty p where
  ppr :: (Applicative m, LFresh m) => p -> m Doc

instance Pretty Term where
  ppr (Var x)     = return . PP.text . show $ x
  ppr (App t1 t2) = PP.parens <$> ((<+>) <$> ppr t1 <*> ppr t2)
  ppr (Lam b)     =
    lunbind b $ \(x,t) ->
      ((PP.brackets . PP.text . show $ x) <+>) <$> ppr t
data Exp = EVar (Name Exp)
         | EStar
         | ELam (Bind Tele Exp)
         | EApp Exp [Exp]
         | EPi (Bind Tele Exp)
  deriving Show
data Tele = Empty
          | Cons (Rebind (Name Exp, Embed Exp) Tele)
  deriving Show
$(derive [''Exp, ''Tele])

instance Alpha Exp
instance Alpha Tele
instance Subst Exp Exp where
  isvar (EVar v) = Just (SubstName v)
  isvar _        = Nothing
instance Subst Exp Tele
evar :: String -> Exp
evar = EVar . string2Name

elam :: [(String, Exp)] -> Exp -> Exp
elam t b = ELam (bind (mkTele t) b)

epi :: [(String, Exp)] -> Exp -> Exp
epi t b = EPi (bind (mkTele t) b)

earr :: Exp -> Exp -> Exp
earr t1 t2 = epi [("_", t1)] t2

eapp :: Exp -> Exp -> Exp
eapp a b = EApp a [b]

mkTele :: [(String, Exp)] -> Tele
mkTele []          = Empty
mkTele ((x,e) : t) = Cons (rebind (string2Name x, Embed e) (mkTele t))
appTele :: Tele -> Tele -> Tele
appTele Empty     t2 = t2
appTele (Cons rb) t2 = Cons (rebind p (appTele t1' t2))
  where (p, t1') = unrebind rb

type M = ErrorT String LFreshM

lookUp :: Name Exp -> Tele -> M Exp
lookUp n Empty     = throwError $ "Not in scope: " ++ show n
lookUp v (Cons rb) | v == x    = return a
                   | otherwise = lookUp v t'
  where ((x, Embed a), t') = unrebind rb
unPi :: Exp -> M (Bind Tele Exp)
unPi (EPi bnd) = return bnd
unPi e         = throwError $ "Expected pi type, got " ++ show e ++ " instead"

infer :: Tele -> Exp -> M Exp
infer g (EVar x)  = lookUp x g
infer _ EStar     = return EStar
infer g (ELam bnd) = do
  lunbind bnd $ \(delta, m) -> do
    b <- infer (g `appTele` delta) m
    return . EPi $ bind delta b
infer g (EApp m ns) = do
  bnd <- unPi =<< infer g m
  lunbind bnd $ \(delta, b) -> do
    checkList g ns delta
    multiSubst delta ns b
infer g (EPi bnd) = do
  lunbind bnd $ \(delta, b) -> do
    check (g `appTele` delta) b EStar
    return EStar

check :: Tele -> Exp -> Exp -> M ()
check g m a = do
  b <- infer g m
  checkEq b a

checkList :: Tele -> [Exp] -> Tele -> M ()
checkList _ [] Empty = return ()
checkList g (e:es) (Cons rb) = do
  let ((x, Embed a), t') = unrebind rb
  check g e a
  checkList (subst x e g) (subst x e es) (subst x e t')
checkList _ _ _ = throwError $ "Unequal number of parameters and arguments"

multiSubst :: Tele -> [Exp] -> Exp -> M Exp
multiSubst Empty     [] e = return e
multiSubst (Cons rb) (e1:es) e = multiSubst t' es e'
  where ((x,_), t') = unrebind rb
        e' = subst x e1 e
multiSubst _ _ _ = throwError $ "Unequal lengths in multiSubst" -- shouldn't happen

-- A conservative, inexpressive notion of equality, just for the sake
-- of the example.
checkEq :: Exp -> Exp -> M ()
checkEq e1 e2 = if aeq e1 e2 
                  then return () 
                  else throwError $ "Couldn't match: " ++ show e1 ++ " " ++ show e2
