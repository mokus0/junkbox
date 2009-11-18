{-
 -	"infixConstructors.hs"
 -	(c) 2008 James Cook
 -}

module TypeExperiments.InfixConstructors where

import qualified Prelude
import Prelude hiding ((+), (-), (*), (/), (^))

data Expr a b
        = a :+ b
        | a :- b
        | a :* b
        | a :/ b
        | a :^ b
        deriving (Eq, Show, Read)

a +  b = a :+ b
a -  b = a :- b
a *  b = a :* b
a /  b = a :/ b
a ^  b = a :^ b
a ** b = a :^ b

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixr 8 ^
