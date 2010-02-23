ptri = [1] : map foo ptri
	where
		foo xs = zipWith (+) ([0] ++ xs) (xs ++ [0])