instance Num String
	where
		x + y = x ++ " " ++ y ++ " " ++ " +"
		x * y = x ++ " " ++ y ++ " " ++" *"
		x - y = x ++ " " ++ y ++ " " ++" -"
		negate x = x ++ " negate"
		abs x = x ++ " abs"
		signum x = x ++ " signum"
		fromInteger = show
		
instance Fractional String
	where
		x / y = x ++ " " ++ y ++ " " ++ "/"
		recip x = x ++ " " ++ "recip"
		fromRational = show

instance Floating String
	where
		pi = "pi"
		exp x = x ++ " " ++ "exp"
		sqrt x = x ++ " " ++ "sqrt"
		log x = x ++ " " ++ "log"
		x ** y = x ++ " " ++ y ++ " " ++ "**"
		logBase x y = x ++ " " ++ y ++ " " ++ "logBase"
		sin x = x ++ " " ++ "sin"
		tan x = x ++ " " ++ "tan"
		cos x = x ++ " " ++ "cos"
		asin x = x ++ " " ++ "asin"
		atan x = x ++ " " ++ "atan"
		acos x = x ++ " " ++ "acos"
		sinh x = x ++ " " ++ "sinh"
		tanh x = x ++ " " ++ "tanh"
		cosh x = x ++ " " ++ "cosh"
		asinh x = x ++ " " ++ "asinh"
		atanh x = x ++ " " ++ "atanh"
		acosh x = x ++ " " ++ "acosh"

eval :: (Num a) => String -> a
