
	-- Gregorian = dates AD used since 1752 in Britain & Colonies
	-- Julian = dates AD used prior to this time
	{- 
		AD = dates AD as used in British colonies
	-}
	{-
		days: 1 -> 1st through n -> nth
		months: 1 -> January through 12 -> December
		years: n -> n
	_}
data Date = (Integer d, Integer m, Integer y) =>
	  Gregorian d m y
	| Julian d m y
	| AD d m y

	{- day: a basis for date arithmetic.  converts a date to a
	   number, with Julian 1 1 1 = day 0
day :: Date -> Integer
day Julian 1 1 1 = 0
