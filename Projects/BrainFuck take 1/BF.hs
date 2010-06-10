module BF where

import Text.ParserCombinators.Parsec
import Data.Char

-- obviously very disorganized - i need to learn some Haskell 'style'...
-- anyway, this is my first go at a BrainFuck interpreter.
-- I have encapsulated the input and output as a permanent part of the
-- state, because I'm hoping to experiment a bit with backwards execution.

-- later, i'd like to try a completely different approach (to
-- reverse/nondeterministic execution), where values are represented as
-- systems of concurrent constraints.  Thus, the "start" state for a
-- reverse run might include some output, and each memory cell
-- "unconstrained", and as things change, constraints will be added.
-- obviously, this will be astronomically ineffecient, because it will
-- require frequently solving fantastically complex satisfiability problems,
-- but hey... BF's a toy language, so let's have some fun!

-- another idea is a "multithreaded Funge" implementation. (probably much
-- less complex than Funge-98 - although I have some nightmarish ideas for
-- a runtime-variable-topology system)

class (Eq a) => BFData a where
	incr:: a -> a
	decr:: a -> a
	doEof:: a -> a
	bfZero:: a


instance BFData Char where
	incr c = chr . (`mod` 265) $ (ord c) + 1
	decr c = chr . (`mod` 265) $ (ord c) - 1
	doEof c = chr 0
	bfZero = chr 0

-- what causes "overlapping instance" errors, and how can i avoid them?
--instance (Num a) => BFData a where
--	incr = (+ 1)
--	decr = (subtract 1)
--	doEof = const 0
--	bfZero = 0

data BFTape a = BFTape Bool [a] a [a]
	deriving (Eq, Ord, Read)

instance (Show a) => Show (BFTape a)
	where
		show (BFTape False ls c rs) = "(" ++ (show $ reverse ls) ++ "    " ++ (show c) ++ "    " ++ (show rs) ++ ")"
		show (BFTape True ls c rs) = "(..." ++ (show $ reverse ls) ++ "    " ++ (show c) ++ "    " ++ (show rs) ++ "...)"

showInstrs (BFTape False ls c rs) = "(" ++ ((reverse ls) >>= show) ++ "    " ++ (show c) ++ "    " ++ (rs >>= show) ++ ")"
showInstrs (BFTape True ls c rs) = "(..." ++ ((reverse ls) >>= show) ++ "    " ++ (show c) ++ "    " ++ (rs >>= show) ++ "...)"

tapeEndRight (BFTape False _ _ []) = True
tapeEndRight _ = False

tapeEndLeft (BFTape False [] _ _) = True
tapeEndLeft _ = False

readTape (BFTape _ _ x _) = x
writeTape x (BFTape circular ls _ rs) = BFTape circular ls x rs

changeTape f tape = writeTape (f (readTape tape)) tape

insertRight x (BFTape circular ls c rs) = BFTape circular (c:ls) x rs
insertLeft x (BFTape circular ls c rs) = BFTape circular ls x (c:rs)

moveRight (BFTape circular ls c (r:rs)) = BFTape circular (c:ls) r rs
moveRight (BFTape False _ _ []) = error "End of Tape (right)"
moveRight (BFTape True ls c []) = BFTape True [c] r rs
	where
		(r:rs) = reverse ls

moveLeft (BFTape circular (l:ls) c rs) = BFTape circular ls l (c:rs)
moveLeft (BFTape False [] _ _) = error "End of Tape (left)"
moveLeft (BFTape True [] c rs) = BFTape True ls l [c]
	where (l:ls) = reverse rs

moveTape 0 tape = tape
moveTape n tape = 
	if (n>1) 
		then moveTape (n-1) (moveRight tape)
		else moveTape (n+1) (moveLeft tape)

data BFInstr = BF_Left | BF_Right | BF_Up | BF_Down | BF_In | BF_Out | BF_Open | BF_Close | BF_Halt | BF_Noop
	deriving (Eq, Read)

bfInstrToChar BF_Left = '<'
bfInstrToChar BF_Right = '>'
bfInstrToChar BF_Up = '+'
bfInstrToChar BF_Down = '-'
bfInstrToChar BF_In = ','
bfInstrToChar BF_Out = '.'
bfInstrToChar BF_Open = '['
bfInstrToChar BF_Close = ']'
bfInstrToChar BF_Halt = '$'
bfInstrToChar BF_Noop = ' '

instance Show BFInstr where
	show = (:[]) . bfInstrToChar


	-- brainfuck machine state;
	--
	-- a tape of instructions
	-- a tape of memory cells, of type 'a'
	-- a list of input waiting to be consumed
	-- a list of output already produced
data (BFData a) => BFState a = BFState (BFTape BFInstr) (BFTape a) [a] [a]
	deriving (Eq, Read)

instance (Show a, BFData a) => Show (BFState a) where
	show (BFState instrs mem input output) = "BFState " ++ showInstrs instrs ++ " " ++ show mem ++ " " ++ show input ++ "" ++ show output

	-- basic state manipulation
bfCurrentInstruction (BFState instrTape _ _ _) = readTape instrTape

bfStateNextInstr (BFState instrTape memTape input output) =
	if (tapeEndRight instrTape) 
		then BFState (insertRight BF_Noop instrTape) memTape input output
		else BFState (moveRight instrTape) memTape input output

bfStatePrevInstr (BFState instrTape memTape input output) =
	if (tapeEndLeft instrTape) 
		then BFState (insertLeft BF_Noop instrTape) memTape input output
		else BFState (moveLeft instrTape) memTape input output

bfStateMemLeft (BFState instrTape memTape input output) =
	BFState instrTape (moveLeft memTape) input output

bfStateMemRight (BFState instrTape memTape input output) =
	BFState instrTape (moveRight memTape) input output

bfStateReadMem (BFState _ memTape _ _) = readTape memTape

bfStateChangeMem f (BFState instrTape memTape input output) =
	BFState instrTape (changeTape f memTape) input output

bfNoInput::(Num a) => a
bfNoInput = 0

bfStateReadInput (BFState _ _ (i:_) _) = i
bfStateReadInput (BFState _ (BFTape _ _ c _) [] _) = doEof c

bfStateDropInput (BFState instrTape memTape (i:input) output) =
	BFState instrTape memTape input output
bfStateDropInput (BFState instrTape memTape [] output) =
	BFState instrTape memTape [] output

bfStateWriteOutput x (BFState instrTape memTape input output) =
	BFState instrTape memTape input (output ++ [x])


bfStateGetOutput (BFState _ _ _ o) = o

	-- instruction stepping
bfFinished::(BFData a) => (BFState a) -> Bool
--bfFinished (BFState instrTape _ _ _) = tapeEndRight instrTape
bfFinished state = bfCurrentInstruction state == BF_Noop

bfStep::(BFData a) => (BFState a) -> (BFState a)
bfStep state = bfApplyInstruction (bfCurrentInstruction state) state

bfApplyInstruction::(BFData a) => BFInstr -> (BFState a) -> (BFState a)
bfApplyInstruction BF_Left state = (bfStateNextInstr . bfStateMemLeft) state
bfApplyInstruction BF_Right state = (bfStateNextInstr . bfStateMemRight) state
bfApplyInstruction BF_Up state = (bfStateNextInstr . (bfStateChangeMem incr)) state
bfApplyInstruction BF_Down state = (bfStateNextInstr . (bfStateChangeMem decr)) state
bfApplyInstruction BF_In state = (bfStateNextInstr . (bfStateChangeMem (const input)) . bfStateDropInput) state
	where
		input = bfStateReadInput state
bfApplyInstruction BF_Out state = (bfStateNextInstr . (bfStateWriteOutput (bfStateReadMem state))) state
bfApplyInstruction BF_Open state =
	if bfStateReadMem state == bfZero
		then bfJumpForward state
		else bfStateNextInstr state
bfApplyInstruction BF_Close state =
	if bfStateReadMem state /= bfZero
		then bfJumpBackward state
		else bfStateNextInstr state
bfApplyInstruction BF_Halt state = bfStateNextInstr state
bfApplyInstruction BF_Noop state = state

bfJumpForward (BFState instrTape memTape input output) =
	BFState (seekClose 0 (moveRight instrTape)) memTape input output
		where
			seekClose n tape = case (readTape tape) of
				BF_Open	-> seekClose (n+1) (moveRight tape)
				BF_Close -> if (n == 0) 
					then moveRight tape
					else seekClose (n-1) (moveRight tape)
				otherwise -> seekClose n (moveRight tape)

bfJumpBackward (BFState instrTape memTape input output) =
	BFState (seekOpen 0 (moveLeft instrTape)) memTape input output
		where
			seekOpen n tape = case (readTape tape) of
				BF_Open	-> if (n == 0) 
					then moveRight tape
					else seekOpen (n-1) (moveLeft tape)
				BF_Close -> seekOpen (n+1) (moveLeft tape)
				otherwise -> seekOpen n (moveLeft tape)

	-- here goes nothing... see if we can make a reverse-interpreter
-- bfBackStep::(BFState a) -> [BFState a]
-- bfBackStep state = bfUnApplyInstruction (bfCurrentInstruction . bfStatePrevInstr state) state

	-- "PossibleStart" - the machine is in a valid start state.
	-- True if (all of):
			-- Instruction tape is at the left end
			-- Output is empty
			-- memory is all zeroes
-- bfPossibleStart state = 
	
	-- "Impossible" - the machine is in an impossible-to-reach start-state
	-- True if (all of):
		-- Instruction tape is at left end
		-- any of:
			-- Output is non-empty
			-- memory is non-zero somewhere
-- bfImpossibleStart

-- bfUnApplyInstruction BF_Noop state = 





	-- very simple tracing utility function - not a good idea for large states;
	-- prints the *whole* state at every step of execution.
	-- also doesn't currently print the final state
bfTrace_ state = takeWhile (not . bfFinished) $ iterate bfStep state
bfTrace state = mapM_ (putStrLn . show) $ bfTrace_ state

bfLastState state = last (bfTrace_ state)
bfRun state = bfStateGetOutput (bfLastState state)

	-- basic parse function - improve later

bfMakeTape::[a] -> BFTape a
bfMakeTape [] = error "Cannot make an empty tape"
bfMakeTape list = BFTape False [] (head list) (tail list)

bfNewMachine::(BFData a) => (BFTape BFInstr) -> Int -> [a] -> (BFState a)
bfNewMachine instrs memSize input = BFState instrs (bfMakeTape $ replicate memSize bfZero) input []

runBF::(BFData a) => BFState a -> [a]
runBF = bfStateGetOutput . bfLastState

smallBF code input = bfNewMachine (bfParse code) 100 input
runSmallBF code input = runBF $ smallBF code input

standardBF code input = bfNewMachine (bfParse code) 30000 input
runStandardBF code input = runBF $ standardBF code input

infiniteBF code input = BFState (bfParse code) (bfMakeTape $ repeat bfZero) input []
runInfiniteBF code input = runBF $ infiniteBF code input

bfParse::String -> BFTape BFInstr
bfParse text = bfMakeTape parsed
	where parsed = either 
		(const (error "Bad Parse"))
		(id)
		(parse bfParser "bfParse" text)

bfParser::Parser [BFInstr]
bfParser = do
	code <- many bfpInst
	return ((foldr (++) [] code) ++ [BF_Halt])

bfpInst::Parser [BFInstr]
bfpInst = bfpLeft <|> bfpRight <|> bfpUp <|> bfpDown <|> bfpIn <|> bfpOut <|> bfpOpen <|> bfpClose <|> bfpOther

bfpLeft =	(char '<') >> (return [BF_Left])
bfpRight =	(char '>') >> (return [BF_Right])
bfpUp =		(char '+') >> (return [BF_Up])
bfpDown =	(char '-') >> (return [BF_Down])
bfpIn =		(char ',') >> (return [BF_In])
bfpOut =	(char '.') >> (return [BF_Out])
bfpOpen =	(char '[') >> (return [BF_Open])
bfpClose =	(char ']') >> (return [BF_Close])
bfpOther = anyChar >> (return [])


-- some sample stuff i hacked up in ghci

-- a 'hello world' i got off some web site.
s :: (BFData a) => BFState a
s = bfNewMachine (bfParse $ ">++++[<++++++++++++++++>-]<++++++++.>++[<++++++"
	++ "++++++++++>-]<---.+++++++..+++.>+++++[<---------------->-]<+.>+++[<+"
	++ "+++++++++++++++>-]<+++++++.++++++++++++++  ++++++++++.+++.------.----"
	++ "----.") 3 []

-- a list of the execution states of s at and following every '[' and ']'
interestingStates::(BFData a) => (BFState a) -> [BFState a]
interestingStates s = s : (takeWhile (not . bfFinished) $ map next u) 
	where 
		next st = last (map bfStep $ (st :) $ takeWhile (\state -> and [bfCurrentInstruction state /= BF_Close, bfCurrentInstruction state /= BF_Halt, bfCurrentInstruction state /= BF_Open]) $ iterate bfStep st)
		u = interestingStates s

interestingTrace s = mapM_ print $ interestingStates s

u::(BFData a) => [BFState a]
u = interestingStates s

ioStates s =s : (takeWhile (not . bfFinished) $ map next u) 
	where 
		next st = last (map bfStep $ (st :) $ takeWhile (\state -> and [bfCurrentInstruction state /= BF_In, bfCurrentInstruction state /= BF_Out]) $ iterate bfStep st)
		u = ioStates s

outputAtCount n state = bfStateGetOutput ((bfTrace_ state) !! n)

-- for more interesting code snippets, see
-- http://esoteric.sange.fi/brainfuck/bf-source/prog/short.b

-- main - for use as an interpreter.  Takes a filename as arg1,
-- connects input and output, runs program.  simple stuff.

