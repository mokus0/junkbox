module Math.NumericalConstraint where

data Expr a b =
	  Var b
	| Val a
	| Add [Expr a b]
	| Mult [Expr a b]
	| Negate (Expr a b)
	| Signum (Expr a b)
	deriving (Eq, Ord, Show)

instance (Num a, Eq b, Show b) => Num (Expr a b) where
	x + y = Add [x, y]
	x - y = Add [x, negate y]
	x * y = Mult [x, y]
	negate = Negate
	abs x = x * (signum x)
	signum = Signum
	fromInteger = Val . fromInteger


data (Num a) => NumericalConstraint a b = 
	  Eq [Expr a b] 
	| Gt (Expr a b) (Expr a b)
	| Conj [NumericalConstraint a b] 
	| Disj [NumericalConstraint a b]
	deriving (Eq, Ord, Show)

x |== y = Eq [x,y]
x |/= y = Disj [Gt x y, Gt y x]