-- Text Formatting

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
	deriving (Eq, Ord, Enum, Read, Show)

data Date = DMY Integer Integer Integer

instance Show Date where
	show = date_string

instance Eq Date where
	d1 == d2 = (day d1) == (day d2)
	d1 /= d2 = (day d1) /= (day d2)

instance Ord Date where
	compare d1 d2 = compare (day d1) (day d2)
	d1 <= d2 = (day d1) <= (day d2)
	d1 < d2 = (day d1) < (day d2)
	d1 > d2 = (day d1) > (day d2)
	d1 >= d2 = (day d1) >= (day d2)
	max d1 d2 = date $ max (day d1) (day d2)
	min d1 d2 = date $ min (day d1) (day d2)

date_string :: Date -> String
date_string (DMY d m y) = (weekday_name (weekday (DMY d m y))) ++ " " ++ (month_name m) ++ " " ++ (show d) ++ ", " ++ (show y)

month_name :: Integer -> String
month_name 1 = "January"
month_name 2 = "February"
month_name 3 = "March"
month_name 4 = "April"
month_name 5 = "May"
month_name 6 = "June"
month_name 7 = "July"
month_name 8 = "August"
month_name 9 = "September"
month_name 10 = "October"
month_name 11 = "November"
month_name 12 = "December"

weekday_name :: Integer -> String
weekday_name 1 = "Sunday"
weekday_name 2 = "Monday"
weekday_name 3 = "Tuesday"
weekday_name 4 = "Wednesday"
weekday_name 5 = "Thursday"
weekday_name 6 = "Friday"
weekday_name 7 = "Saturday"


-- Date functions

is_leap_year :: Integral a => a -> Bool
is_leap_year year	| (year < 1800) = (mod year 4) == 0
			| (year >= 1800) = ((mod year 4) == 0)
				&& (((mod year 400) == 0)
					|| (not ((mod year 100) == 0)))

days_in_year :: Integral a => a -> a
days_in_year 1752 = 355
days_in_year year = if is_leap_year year then 366 else 365

days_in_february :: Integral a => a -> a
days_in_february year = if is_leap_year year then 29 else 28

days_in_month :: Integral a => a -> a -> a
days_in_month 9 1752 = 19
days_in_month m y = days
	where days = ([31, (days_in_february y), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] ++ (repeat 0)) !! (fromInteger (toInteger m))

day :: Date -> Integer
day (DMY d 1 1) = d
day (DMY 0 1 y) = day (DMY 0 1 (y - 1)) + days_in_year (y - 1)
day (DMY d 1 y) = day (DMY 0 1 y) + d
day (DMY d m y) = if (m == 9) && (y == 1752)
			then (if (d < 3) then (639796 + d)
				else (if (d < 14) then 639799
					else (639785 + d)))
			else (day (DMY d (m - 1) y) + days_in_month (m - 1) y)

normalize_date :: Date -> Date
normalize_date (DMY d 9 1752)	| d < 3 = (DMY d 9 1752)
					| (d >= 3) && (d < 14) = (DMY 14 9 1752)
					| (d >= 14) && (d <= 30) = (DMY d 9 1752)
					| (d > 30) = normalize_date (DMY (d - 30) 10 1752)
normalize_date (DMY d 8 1752)	| d <= 31 = (DMY d 8 1752)
					| (d > 31) && (d <= 61) = normalize_date (DMY (d - 31) 9 1752)
					| d > 61 = normalize_date (DMY (d - 61) 10 1752)
normalize_date (DMY d m y)	| m == 13 = normalize_date(DMY d 1 (y + 1))
					| m < 13 = if (d <= d_i_m)
						then (DMY d m y)
						else normalize_date(DMY (d - d_i_m) (m + 1) y)
							where d_i_m = days_in_month m y

date :: Integer -> Date
date 1 = (DMY 1 1 1)
date d	| d < 639799 = normalize_date (DMY d 1 1)
	| d >= 639799 = normalize_date (DMY (d - 639785) 9 1752)

weekday :: Date -> Integer
weekday d = (mod ((day d) + 5) 7) + 1

instance Enum Date where
	succ d = date (1 + day d)
	pred d = date ((-1) + day d)
	toEnum = date . toEnum
	fromEnum = fromEnum . day
	enumFrom d = map date [(day d)..]
	enumFromThen = undefined
	enumFromTo = undefined
	enumFromThenTo = undefined