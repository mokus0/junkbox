import Prelude
	hiding (sum)

-- a new "sum" that doesn't start with (noUnit-Dimensioned) zero
-- can't seem to get it to be tail-recursive when processing Dimensioned
-- types though
--     (Prelude.sum doesn't either, though, so I guess I'm no worse off)
--     (Maybe it's because I'm running in GHCI, rather than compiling?)
sum [] = 0
sum xs = foldl1 (+) xs

data Unit = Unit [(String, Integer)]
data Dimensioned x = Dim x Unit
	deriving Eq

data UnitDefinition x = Def String (Dimensioned x)

data DimensionConversionTable a = DCT [UnitDefinition a]
data ConvertibleDimensioned a = CD (DimensionConversionTable a) (Dimensioned a)

class UnitBased a where
	noUnit :: a
	mkUnit :: String -> a
	mkUnitExp :: String -> Integer -> a
	unitExp :: String -> a -> Integer
	deleteUnit :: String -> a -> a
	unit :: a -> Unit

class Normalizing a where
	normalize :: a -> a

class ShowRaw a where
	showRaw :: a -> String

instance Normalizing Unit where
	normalize = compressUnits . sortUnit

instance Normalizing (Dimensioned a) where
	normalize (Dim n u) = Dim n (normalize u)

instance UnitBased Unit where
	unitExp uName (Unit u) = foldr (\(u,n) x -> if (u == uName) then x+n else x) 0 u
	deleteUnit uName (Unit u) = Unit (filter (\(unit,exp) -> unit==uName) u)
	unit = id
	noUnit = Unit []
	mkUnit x = mkUnitExp x 1
	mkUnitExp x n = Unit[(x,n)]
	

instance (Num a) => UnitBased (Dimensioned a) where
	unitExp uName (Dim n u) = unitExp uName u
	deleteUnit uName (Dim n u) = Dim n (deleteUnit uName u)
	unit (Dim _ u) = u
	noUnit = (Dim 1 noUnit)
	mkUnit x = Dim 1 (mkUnit x)
	mkUnitExp x e = Dim 1 (mkUnitExp x e)

scale (Dim n _) = n


-- applyDefinition (Def u val) (Dim n u) = 


kg :: forall a. (UnitBased a) => a
kg = mkUnit "kg"
m :: forall a. (UnitBased a) => a
m = mkUnit "m"
sec :: forall a. (UnitBased a) => a
sec = mkUnit "sec"
_N :: forall a. (Fractional a, UnitBased a) => a
_N = kg * m / sec ^ 2

mergeUnits (Unit x) (Unit y) = Unit (x ++ y)

invertUnit (Unit x) = Unit (map (\(u, e) -> (u, negate e)) x)

sortUnit (Unit x) = Unit (sort x)
	where
		sort [] = []
		sort [x] = [x]
		sort (x:xs) = (sort $ lt x) ++ (x : (sort $ gte x))
			where
				lt x = filter (< x) xs
				gte x = filter (>= x) xs

compressUnits (Unit x) = Unit (compress x)
	where
		compress [] = []
		compress [x] = [x]
		compress ((u1,n1):us) = 
			if (u1 == u2) 
				then compress ((u1,n1+n2):(tail us))
				else (u1,n1) : (compress us)
					where (u2,n2) = head us


-- Class instance definitions

instance ShowRaw Unit where
	showRaw (Unit u) = "Unit " ++ show u

instance forall a. (Show a) => ShowRaw (Dimensioned a) where
	showRaw (Dim n u) = "Dim (" ++ (show n) ++ ") (" ++ (showRaw u) ++ ")"

instance Show Unit where
	show (Unit []) = ""
	show u = if (length negU) > 0 
		then (showUnits posU) ++ " / " ++ (showUnits negU)
		else showUnits posU
			where
				(Unit uNorm) = normalize u
				posU = filter posExp uNorm
				negU = map negateExp (filter negExp uNorm)
				posExp (_,n) = (n > 0)
				negExp (_,n) = (n < 0)
				negateExp (u,n) = (u, -n)
				showUnits [] = "1"
				showUnits [x] = showUnit x
				showUnits (x:xs) = (showUnit x) ++ " " ++ (showUnits xs)
				showUnit (u,n) = u ++ (showExp n)
				showExp n = if (n == 1) then "" else "^" ++ show n

instance (Show x) => Show (Dimensioned x) where
	show (Dim num unit) = show num ++ " " ++ show unit

instance Eq Unit where
	x == y = xNorm == yNorm
		where
			(Unit xNorm) = normalize x
			(Unit yNorm) = normalize y
	x /= y = not (x == y)

instance (Ord x) => Ord (Dimensioned x) where
	(Dim n1 u1) > (Dim n2 u2) = if (u1 == u2) then (n1 > n2) else error "Incomparable Units"
	(Dim n1 u1) < (Dim n2 u2) = if (u1 == u2) then (n1 < n2) else error "Incomparable Units"
	(Dim n1 u1) >= (Dim n2 u2) = if (u1 == u2) then (n1 >= n2) else error "Incomparable Units"
	(Dim n1 u1) <= (Dim n2 u2) = if (u1 == u2) then (n1 <= n2) else error "Incomparable Units"
	max (Dim n1 u1) (Dim n2 u2) = if (u1 == u2) then Dim (max n1 n2) u1 else error "Incomparable Units"
 	min (Dim n1 u1) (Dim n2 u2) = if (u1 == u2) then Dim (min n1 n2) u1 else error "Incomparable Units"

instance Num Unit where
	x + y = if (x==y) then x else error "Incomparable Units"
	x - y = x + y
	x * y = mergeUnits x y
	negate x = x
	signum x = Unit []
	abs x = Unit []
	fromInteger x = Unit []

instance (Num a) => Num (Dimensioned a) where
	(Dim n1 u1) + (Dim n2 u2) = Dim (n1 + n2) (u1 + u2)
	(Dim n1 u1) - (Dim n2 u2) = Dim (n1 - n2) (u1 - u2)
	(Dim n1 u1) * (Dim n2 u2) = Dim (n1 * n2) (u1 * u2)
	negate (Dim n u) = Dim (negate n) (negate u)
	signum (Dim n u) = Dim (signum n) (signum u)
	abs (Dim n u) = Dim (abs n) (abs u)
	fromInteger n = Dim (fromInteger n) (fromInteger n)
	
instance Fractional Unit where
	x / y = x * (recip y)
	recip = invertUnit
	fromRational _ = Unit []

instance (Fractional a) => Fractional (Dimensioned a) where
	(Dim n1 u1) / (Dim n2 u2) = Dim (n1/n2) (u1/u2)
	recip (Dim n u) = Dim (recip n) (recip u)
	fromRational n = Dim (fromRational n) (fromRational n)

instance Floating Unit where
	pi = noUnit
	exp x = if (x == noUnit) then noUnit else error ("exp (" ++ (show x) ++ ") makes no sense")
	sqrt x = _sqrt (normalize x)
		where
			_sqrt (Unit x) = Unit (map (`divExp` 2) x)
				where
					divExp (u, e) n = if (e `rem` n) == 0 
						then (u, e `div` n) 
						else error ((show n) ++ " does not evenly divide all exponents in \"" ++ (show (Unit x)) ++ "\"")
	log x = if (x == noUnit) then noUnit else error ("exp (" ++ (show x) ++ ") makes no sense")
	logBase x y = if (y == noUnit)
		then error ("Unit doesn't use a Floating type for its exponents")
		else error ("_ ** (" ++ (show y) ++ ") makes no sense")
	sin x = if (x == noUnit) then noUnit else error ("sin (" ++ (show x) ++ ") makes no sense")
	tan x = if (x == noUnit) then noUnit else error ("tan (" ++ (show x) ++ ") makes no sense")
	cos x = if (x == noUnit) then noUnit else error ("cos (" ++ (show x) ++ ") makes no sense")
	asin x = if (x == noUnit) then noUnit else error ("asin (" ++ (show x) ++ ") makes no sense")
	atan x = if (x == noUnit) then noUnit else error ("atan (" ++ (show x) ++ ") makes no sense")
	acos x = if (x == noUnit) then noUnit else error ("acos (" ++ (show x) ++ ") makes no sense")
	sinh x = if (x == noUnit) then noUnit else error ("sinh (" ++ (show x) ++ ") makes no sense")
	tanh x = if (x == noUnit) then noUnit else error ("tanh (" ++ (show x) ++ ") makes no sense")
	cosh x = if (x == noUnit) then noUnit else error ("cosh (" ++ (show x) ++ ") makes no sense")
	asinh x = if (x == noUnit) then noUnit else error ("asinh (" ++ (show x) ++ ") makes no sense")
	atanh x = if (x == noUnit) then noUnit else error ("atanh (" ++ (show x) ++ ") makes no sense")
	acosh x = if (x == noUnit) then noUnit else error ("acosh (" ++ (show x) ++ ") makes no sense")

instance (Floating a) => Floating (Dimensioned a) where
	pi = Dim pi pi
	exp (Dim n u) = Dim (exp n) (exp u)
	sqrt (Dim n u) = Dim (sqrt n) (sqrt u)
	log (Dim n u) = Dim (log n) (log u)
	(Dim n1 u1) ** (Dim n2 u2) = Dim (n1 ** n2) (u1 ** u2)
	logBase (Dim n1 u1) (Dim n2 u2) = Dim (logBase n1 n2) (logBase u1 u2)
	sin (Dim n u) = Dim (sin n) (sin u)
	tan (Dim n u) = Dim (tan n) (tan u)
	cos (Dim n u) = Dim (cos n) (cos u)
	asin (Dim n u) = Dim (asin n) (asin u)
	atan (Dim n u) = Dim (atan n) (atan u)
	acos (Dim n u) = Dim (acos n) (acos u)
	sinh (Dim n u) = Dim (sinh n) (sinh u)
	tanh (Dim n u) = Dim (tanh n) (tanh u)
	cosh (Dim n u) = Dim (cosh n) (cosh u)
	asinh (Dim n u) = Dim (asinh n) (asinh u)
	atanh (Dim n u) = Dim (atanh n) (atanh u)
	acosh (Dim n u) = Dim (acosh n) (acosh u)

instance Enum Unit where
	succ = id
	pred = id
	toEnum _ = noUnit
	fromEnum _ = error "Unit.fromEnum not implemented"
	enumFrom x = repeat x
	enumFromThen x y = if (x == y) then repeat x else error "Incomparable Units"
	enumFromTo x y = if (x == y) then repeat x else error "Incomparable Units"
	enumFromThenTo x y z = if (x == y) && (y == z) then repeat x else error "Incomparable Units"

instance (Enum a) => Enum (Dimensioned a) where
	succ (Dim n u) = Dim (succ n) (succ u)
	pred (Dim n u) = Dim (pred n) (pred u)
	toEnum n = Dim (toEnum n) (toEnum n)
	fromEnum (Dim n u) = fromEnum n
	enumFrom (Dim n u) = zipWith Dim (enumFrom n) (enumFrom u)
	enumFromThen (Dim n1 u1) (Dim n2 u2) = zipWith Dim (enumFromThen n1 n2) (enumFromThen u1 u2)
	enumFromTo (Dim n1 u1) (Dim n2 u2) = zipWith Dim (enumFromTo n1 n2) (enumFromTo u1 u2)
	enumFromThenTo (Dim n1 u1) (Dim n2 u2) (Dim n3 u3) = zipWith Dim (enumFromThenTo n1 n2 n3) (enumFromThenTo u1 u2 u3)
	