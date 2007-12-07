-- Optimizing Brainfuck interpreter

{-
 -  I want to change this so that the user may choose
 - which optimizations to apply.  For example,
 - in certain BF machine configurations, it is not always
 - valid to change ">><<" to "", because the tape is
 - bounded and does not wrap around.
 - 
 - Similarly, it is not always proper to change "+-" to "".
 - 
 - For now, assume:
 -   -- infinite or wrapping tape
 -   -- unbounded or mod-X numbers
 -}

module OptBF (
		OptBF,
		runOptBF
	) where

import CoreBF
import BFCell
import BFTape

import Data.Int
import Control.Monad.ST

data OptBF a = 
	  Add a				-- add a to value in current cell
	| Go Int			-- positive values mean 'right'
	| Sub [OptBF a]			-- subroutine.  When end is reached, starts over at beginning.
	| IfEq a (OptBF a) 		-- Do action if current cell's value is the given value
	| Break				-- jump out of the current subroutine
	| Input | Output		-- read or write
	| SetCell a			-- set cell to n
	| MvMult Int a			-- *(cell + Int) += a * (*cell); *cell = 0
	deriving (Eq, Show)

-- direct transliteration from "CoreBF" to "OptBF"
toOptBF (Inc:rest) = Add 1				: (toOptBF rest)
toOptBF (Dec:rest) = Add (-1)				: (toOptBF rest)
toOptBF (GoL:rest) = Go (-1)				: (toOptBF rest)
toOptBF (GoR:rest) = Go 1				: (toOptBF rest)
toOptBF (Beg:rest) = (Sub (startSub : (toOptBF sub)))	: (toOptBF rest')
	where
		startSub = IfEq 0 Break
		(sub, rest') = splitSubProgram rest
toOptBF (End:rest) = error "Unbalanced brackets"
toOptBF (Inp:rest) = Input				: (toOptBF rest)
toOptBF (Out:rest) = Output				: (toOptBF rest)
toOptBF (Nop _:rest) = 					   toOptBF rest
toOptBF []		   = []

instance (Num a) => Read (OptBF a)
	where
		readsPrec _ str = [(Sub (readOptBF str ++ [Break]), "")]

readOptBF :: (Num a) => String -> [OptBF a]
readOptBF = hyperOptimize . toOptBF . (map charToCoreBF) 

splitSubProgram prog = checkBalance `seq` (sub, rest)
	where
		sub = init $ map snd sub'
		sub' = takeWhile ((>0) . fst) zipped
		
		rest = map snd rest'
		rest' = dropWhile ((>0) . fst) zipped
		
		zipped = zip depth prog
		depth = scanl countBrackets 1 prog 
		
		checkBalance = if notElem 0 depth
			then error "Unbalanced brackets"
			else ()
		
		countBrackets subTotal Beg = subTotal + 1
		countBrackets subTotal End = subTotal - 1
		countBrackets subTotal  _  = subTotal

hyperOptimize f =
	if reOpt
		then hyperOptimize optF
		else f
			where 
				optF = optimize f
				reOpt = (f /= optF)

optimize (Add 0 : rest) = optimize rest
optimize (Add x1 : Add x2 : rest) = optimize (Add (x1 + x2) : rest)
optimize (Go 0 : rest) = optimize rest
optimize (Go x1 : Go x2 : rest) = optimize (Go (x1 + x2) : rest)

optimize (Add _ : SetCell x : rest) = optimize (SetCell x : rest)
optimize (SetCell x1 : Add x2 : rest) = optimize (SetCell (x1 + x2) : rest)

-- [IfEq 0 Break,Add (-1),Go n,Add x,Go (-n)] -> "MvMult n x"?
optimize (Sub [IfEq 0 Break,Add (-1),Go n1,Add x,Go (n2)] : rest)
	| n1 == (-n2) = optimize (MvMult n1 x : rest)
optimize (Sub [IfEq 0 Break,Add (1),Go n1,Add x,Go (n2)] : rest)
	| n1 == (-n2) = optimize (MvMult n1 (-x) : rest)

-- Sub [IfEq n1 Break, ... ,Add n2]
-- to
-- Add (-n2), Sub [IfEq (n1-n2) Break,Add n2, ... ], Add (n2)
--{-
optimize (Sub (IfEq n1 Break : sub) : rest)
	| (isAdd add) && (length sub > 1)	= case add of
		(Add n2) -> Add (-n2) : Sub (IfEq (n1 - n2) Break : Add n2 : (...)) : optimize (Add n2 : rest)
	where
		(...)			= init sub		-- the "..." above
		add				= last sub	-- the "Add n2" above, if the guard matches
		isAdd (Add _)	= True
		isAdd other		= False
--}

optimize (Sub [IfEq n Break, Add x] : rest) = optimize (SetCell n : rest )
optimize (Sub [Break] : rest) = optimize rest
optimize (Sub f : rest) = Sub (optimize f) : optimize rest

	

optimize (other:rest)		= other : optimize rest
optimize [] = []

{-# SPECIALIZE runOptBF :: OptBF Int8 -> Tape s Int8 -> (Int8 -> ST s Int8) -> (Int8 -> ST s ()) -> ST s () #-}

runOptBF :: (Num a, Ord a) => 
	   OptBF a 			-- the program to run
	-> (Tape s a)			-- a tape to use
	-> (a -> ST s a)		-- an input function; arg is current cell value
	-> (a -> ST s ()) 		-- an output function
	-> ST s ()			-- returns: ()

runOptBF (Sub lst) tape inF outF = do
	runSubBF (cycle lst) tape inF outF

-- shouldn't be used except as an entry point
runOptBF otherCmd tape inF outF = do
	runSubBF [otherCmd, Break] tape inF outF


{-# SPECIALIZE runSubBF :: [OptBF Int8] -> Tape s Int8 -> (Int8 -> ST s Int8) -> (Int8 -> ST s ()) -> ST s () #-}
{-# INLINE runSubBF #-}

runSubBF (Add n:rest) tape inF outF = do
	cell <- currentCell tape
	incNTimes n cell
	runSubBF rest tape inF outF

runSubBF (Go n:rest) tape inF outF = do
	goN n tape
	runSubBF rest tape inF outF

runSubBF (Break:_) tape inF outF = do
	return ()

{-
runSubBF (IfEq n Break : rest) tape inF outF = do
	cell <- currentCell tape
	condition <- isN n cell
	if condition
		then return ()
		else runSubBF rest tape inF outF
-}

runSubBF (IfEq n act : rest) tape inF outF = do
	cell <- currentCell tape
	condition <- isN n cell
	if condition
		then runSubBF (act : rest) tape inF outF
		else runSubBF rest tape inF outF

runSubBF (Sub lst:rest) tape inF outF = do
	runSubBF (cycle lst) tape inF outF
	runSubBF rest tape inF outF

runSubBF (Input:rest) tape inF outF = do
	cell <- currentCell tape
	currentValue <- output cell
	inValue <- inF currentValue
	input cell inValue
	runSubBF rest tape inF outF

runSubBF (Output:rest) tape inF outF = do
	cell <- currentCell tape
	value <- output cell
	outF value
	runSubBF rest tape inF outF

runSubBF (SetCell n : rest) tape inF outF = do
	cell <- currentCell tape
	input cell n
	runSubBF rest tape inF outF

runSubBF (MvMult n x : rest) tape inF outF = do
	cellA <- currentCell tape
	cellB <- offset n tape

	a <- output cellA
	input cellA 0
	incNTimes (a * x) cellB
	runSubBF rest tape inF outF

{-
runSubBF (act:rest) tape inF outF = do
	runOptBF act tape inF outF
	runSubBF rest tape inF outF
-}

runSubBF [] tape inF outF = do
	error "Empty subroutine; optimizer did something stupid"

