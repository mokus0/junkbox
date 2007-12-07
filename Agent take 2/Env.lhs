\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{@Env.lhs@}
\author{James Cook}
\date{Oct 2007}

\maketitle

%if codeOnly || showModuleHeader
%	(Haskell stuff that shouldn't go into the formatted doc)
\begin{code}

{-# OPTIONS -fglasgow-exts #-}

module Env where

import Control.Monad.State
import Control.Monad.Reader

\end{code}
%endif

\begin{abstract}
Basic sketch of moku's ideas for an ``agent-based execution'' abstraction in Haskell.  The objective is to devise:

\begin{itemize}
	\item A general execution-strategy-independent way of specifying game behavior,
	\item An abstraction for describing agent behavior in a similarly strategy-independent way, and 
	\item A framework for tying those together with various actual execution strategies.
\end{itemize}

\end{abstract}

\section{Overview}

The abstraction defined here is composed of two parts - ``queries'' and ``actions''.  These are interfaces between agent and game.  From the agent's perspective, 

Games may alternately be defined in terms of specific execution strategies if so desired, although that of course will limit the generality of that specific game.  The agent-interface may be similarly specialized.

\section{Queries}
\subsection{Base Abstraction}
Queries are how agents get information from the environment.  The core of the ``query'' concept is the |EnvQuery| class.  The meaning of the type parameters is as follows:

\begin{itemize}
	\item |e|: The type of the environment (world state)
	\item |a|: The type of an ``agent identifier'' that the query function may use to determine how to respond to the query.  Most execution strategies will attach this to a monad in which the agent runs, hopefully in such a way that the agent will not be able to ``forge'' it.
	\item |q|: The type of the ``query'' - a data type whose values describe the information that the agent is requesting.
	\item |r|: The type of the ``response'' to the query.
\end{itemize}

\begin{code}
	
class EnvQuery e a q r
	where
		queryEnv	:: e -> a -> q -> r

\end{code}

\subsection{Monadic Queries}
The preferred way to implement a query, if possible, is to implement instances of |EnvQuery|.  Standard ``Execution Strategies'' consist of mappings from |EnvQuery| to |EnvQueryM|, a class defining a monadic action, |query|, that hides the passing of state (and ideally also protects that state from unauthorized access).

If necessary, an |EnvQueryM| instance may be directly defined.

|EnvQueryM| defines one function, |query|, that takes a ``query'' and returns a ``response,'' as above, but hides the passing of the environment and agent-id.

\begin{code}
	
class (Monad m) => EnvQueryM m e a q r
	where
		query		:: q -> m r

\end{code}

\section{Actions}
\subsection{Base Abstraction}

\begin{code}

class EnvAction e a c
	where
		actEnv :: e -> a-> c -> Either e String

\end{code}

\subsection{Monadic Actions}

\begin{code}

class (Monad m) => EnvActionM m e a c
	where
		act :: c -> m ()

\end{code}

\section{A Sample Implementation}

\begin{code}

instance (MonadState e m, MonadReader a m, EnvAction e a c)
	=> EnvActionM m a e c
	where
		act cmd = do
			env <- get
			agent <- ask
			case actEnv env agent cmd of
				Left env' -> put env'
				Right err -> fail err

instance (MonadState e m, MonadReader a m, EnvQuery e a q r) 
	=> EnvQueryM m e a q r
	where
		query q = do
			env    <- get
			agent  <- ask
			return (queryEnv env agent q)

\end{code}

\section{Some open questions}

Should there be a distinction between ``action'' and ``query?''  Is this a useful abstraction at all, or would it be better to work with Monad subclasses tailored to each instance?

The thing I find attractive here is the (theoretical) ability to forget about actual implementation of the simulation environment, but is this a realistic expectation?  In simple cases, it certainly would work, but when it comes to simulations with very large state, it would be far more useful to define custom actions that modify the state in place.  In those cases, the chief value of this abstraction would probably be the uniform conceptual model used for specifying agent behaviors.

In ``nontrivial'' cases, is there any value on the simulation-side?

\end{document}