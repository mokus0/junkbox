module LetRec where

import Control.Monad
import Language.Haskell.TH


-- |@letrec n@ defines a \"let rec\" function for @n@ recursive bindings.  In
-- particular, it takes @n+1@ arguments: the definitions of each of the @n@
-- recursive bindings as functions of all @n@ values, and the \"in\" part of
-- the let expression, as a function of the @n@ recursive bindings.
--
-- For example, @letrec 3@ generates the following code (with uglier
-- variable names):
-- 
-- > \f g h expr -> let x = f x y z
-- >                    y = g x y z
-- >                    z = h x y z
-- >                 in expr x y z
letrec :: Int -> DecQ
letrec n = do
    fs <- replicateM n (newName "f")
    xs <- replicateM n (newName "x")
    expr <- newName "expr"
    
    let xDecs = 
            [ valD (varP x) (normalB (appsE (varE f : map varE xs))) []
            | (f,x) <- zip fs xs
            ]
    
    lamE (map varP fs ++ [varP expr]) (letE xDecs (appsE (varE expr : map varE xs)))
    

