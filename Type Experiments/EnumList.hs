instance (Enum a) => Enum [a]
	where
		toEnum n = [toEnum n]
		
		fromEnum [] = 0
		fromEnum (l:_) = fromEnum l
		
		enumFrom (fr:frs) = [h:t | h <- enumFrom fr, t <- enumFrom frs]
		enumFrom _ = [[]]
		
		enumFromTo (fr:frs) (to:tos) = [h:t | h <- enumFromTo fr to, t <- enumFromTo frs tos]
		enumFromTo _ _ = [[]]
		
		enumFromThen (fr:frs) (th:ths) = [h:t | h <- enumFromThen fr th, t <- enumFromThen frs ths]
		enumFromThen _ _ = [[]]
		
		enumFromThenTo (fr:frs) (th:ths) (to:tos) = [h:t | h <- enumFromThenTo fr th to, t <- enumFromThenTo frs ths tos]
		enumFromThenTo _ _ _ = [[]]
		