\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{@TicTacToe.lhs@}
\author{James Cook}
\date{2007}

\maketitle

\begin{code}

{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fno-monomorphism-restriction -fallow-incoherent-instances #-}

module TicTacToe where

	import qualified Data.Map as M
	import qualified Data.List as L
	import Data.Maybe
	import Env
	
	import Control.Monad.Reader
	import Control.Monad.State

\end{code}

Data types describing the game.  The whole state of the game is ``whose turn it is,'' ``who I am,'' and the marks on the board.

\begin{code}
	data XO = X | O
		deriving (Eq, Show)
	
	instance Enum XO
		where
			succ X = O
			succ O = X
			toEnum 0 = X
			toEnum _ = O
			fromEnum X = 0
			fromEnum O = 1

	data Int3 = I | II | III
		deriving (Eq, Ord, Show)

	type Board = M.Map (Int3,Int3) XO

	data Game = Game {
			whoseTurn :: XO,
			board :: Board
		} deriving (Eq, Show)

\end{code}

Some very basic operations and data

newGame could be pulled into some general ``class'', i suppose

\begin{code}
	
	newGame :: Game
	newGame = Game {whoseTurn = X, board = M.empty}

	mark :: Game -> (Int3, Int3) -> Game
	mark game square = game {whoseTurn = otherGuy, board = marked}
		where
			otherGuy = succ (whoseTurn game)
			marked = M.insert square (whoseTurn game) (board game)

	rows, cols, diags :: [[(Int3, Int3)]]
	rows = [[(x,y) | x <- [I, II, III]] | y <- [I, II , III]]
	cols = [[(x,y) | y <- [I, II , III]] | x <- [I, II , III]]
	diags = [[(I,I), (II,II), (III,III)], [(I,III),(II,II),(III,I)]]

\end{code}

Tic Tac Toe game logic.  These definitions encapsulate the ``rules'' of the game in a set of functions that can easily be wrapped into several different agent-based evaluation strategies.

First, the queries:
\begin{itemize}
	\item WhoseTurn : either player may ask
	\item WhoAmI : either player may ask
	\item What's in a square : either player may ask
\end{itemize}

\begin{code}

	data WhoseTurn = WhoseTurn
		deriving (Eq, Show)

	instance EnvQuery Game XO WhoseTurn XO
		where queryEnv game _ _ = whoseTurn game
	
	data WhoAmI = WhoAmI
		deriving (Eq, Show)
	
	instance EnvQuery Game XO WhoAmI XO
		where queryEnv game player _ = player
	
	instance EnvQuery Game XO (Int3,Int3) (Maybe XO)
		where queryEnv game _ square = M.lookup square (board game)

\end{code}

Second, the actions (of which there are only one):
\begin{itemize}
	\item Yield: Let the other player go (d'oh: can this be implemented without |call/cc|?)
	\item Mark a square: Only the player whose turn it is may do this. Additionally, if the square is taken, the action fails.
\end{itemize}

\begin{code}
	
	data Yield = Yield
		deriving (Eq, Show)
	
	instance EnvAction Game XO Yield
		where
			actEnv game xo Yield = undefined
	
	instance EnvAction Game XO (Int3,Int3)
		where
			actEnv game xo square
				| xo /= (whoseTurn game)
					= Right "Not your turn, bud!"
				| M.lookup square (board game) /= Nothing
					= Right "That square is taken."
				| otherwise
					= Left (mark game square)


\end{code}

Some questions:

	Does this adequately capture the game flow (in the general case), or should the ``Yield'' operation be implemented (and if so, how)?  Perhaps monad ``return'' could be used as a yield?  In that case, the agent would be unable to cache information about the state of the game, including ``plans'' it may have, without extra support from the environment.
	
	In this (admittedly trivial) case, a ``yield'' could be implicitly included in the Mark action.  Even in this case, it's not quite so simple though, if the specification is to be independent of the game's actual execution model.  For example, in order to support concurrent execution of multiple agents, there also should be a command to wait until it's the executing agent's turn.
	
	Alternatively, |yield| could be a first-class monadic action, since its implementation is highly dependent on the execution model.  Otherwise, the bots themselves probably need to be included in the game state - at the risk of smashing the stack.  I don't know that it could be implemented any better (in a non-threaded model) though, without first-class continuations.
	
\end{itemize}

\section{Running a simulation, using reader/state monad transformers}

Man, this code is ugly...  Sorry.  I'll fix it later.

One thing that may clean this up greatly is a |Monoid| instance declaration for |Maybe|-based versions of |Any| and |All|

\begin{code}
	
	winner :: Game -> Maybe XO
	winner game = msum $ do
		squares <- join [rows, cols, diags]
		return (checkSquares squares)
			where
				checkSquares squares = all $ do
					square <- squares
					return (M.lookup square (board game))
				all xs = if length (L.nub xs) == 1
					then head xs
					else Nothing
	
	runGame :: (MonadState Game m, MonadReader XO m) => m () -> m () -> Maybe XO
	runGame xBot oBot = do
		undefined -- Can this even be done, as currently specified?
			-- perhaps I ought to bite the bullet and use STM exclusively
	
	
	
\end{code}

\section{Some sample agent implementations}

I probably shouldn't be, but I'm surprised at how much type information must be explicitly given.  It makes sense, though, given the extreme degree to which the query and action classes are overloaded.

Not all the annotations here are necessary, but since I had some in some odd places, I added several more to make it more aesthetically pleasing.

\begin{code}
	
	dumbot :: ReaderT XO (State Game) ()
	dumbot = start
		where
			start :: ReaderT XO (State Game) ()
			start = do
				me :: XO <- query WhoAmI
				turn :: XO <- query WhoseTurn
				if turn == me
					then doTurn
					else do
						act Yield
						start
			doTurn :: ReaderT XO (State Game) ()
			doTurn = do
				openSquare <- findOpenSquare allSquares
				act openSquare
				start
			findOpenSquare :: [(Int3, Int3)] -> ReaderT XO (State Game) (Int3, Int3)
			findOpenSquare (s:ss) = do
				open <- isOpen s
				if open 
					then return s
					else findOpenSquare ss
			isOpen :: (Int3,Int3) -> ReaderT XO (State Game) Bool
			isOpen square = do
				mark :: Maybe XO <- query square
				return (isNothing mark)
			allSquares = [(x,y) | x <- [I,II,III], y <- [I,II,III]]
	
\end{code}


\end{document}