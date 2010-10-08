\documentclass[10pt,draft]{amsart}
\usepackage{subtitle}
%include polycode.fmt
%include HighLevelIteratees.fmt

\begin{document}

\title{High Level Iteratees}
\subtitle{or: An Iteratee Tutorial Tutorial}
\author{James Cook}

\maketitle

%if codeOnly || showAllCode
First, let's get the boring stuff out of the way.  This is what GHC needs to be happy with the code in this file:

\begin{code}

{-# LANGUAGE GADTs, TypeFamilies, GeneralizedNewtypeDeriving #-}
module HighLevelIteratees where
import Control.Monad.Error
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Trans
        
\end{code}
%endif

\begin{abstract}

Iteratees are an interesting and useful new discovery, but existing explanations of what they are and how to use them leave a lot to be desired.  In particular, the semantics are vague and existing tutorials and implementations encourage a coding style that is far too low-level for the average programmer.

This document presents a new high-level construction of the iteratee concept as a stack of monad transformers, from which a clear and simple semantics arise naturally, as well as an informal procedure for deriving such a construction from an existing low-level implementation.  In both cases the resulting type not only provides an operational semantics, it also provides direction in a search for a set of primitive iteratees and operations.

Once the semantics for any given iteratee implementation are specified and a corresponding set of high-level primitives is derived from that specification, real coder-friendly tutorials can be written.

\end{abstract}

\section{Introduction} % (fold)
\label{sec:intro}

Every iteratee implementation and tutorial I've seen so far is at least moderately low-level.  I find the usual presentation does quite a bit to obscure the topic.  It took quite a few implementations and introductions before I even had a good idea of what an iteratee is and why I'd use one.  Since then, every serious use I've made of iteratee libraries has involved writing at least one function that did something conceptually quite simple in a mind-bending, convoluted way.  Even if that is entirely due to my own ineptitude, the fact that I have read tutorials and failed to learn from them a better way to do such simple tasks is puzzling.

I have yet to find even a single tutorial in which every example iteratee, or even the majority of them, is written solely in terms of high level primitives.  Instead, the style they encourage involves directly implementing complex continuation-passing logic.  Until the day a comprehensive iteratee tutorial can be written without a single explicit ``feed me'' continuation, iteratees are just not ready for the average programmer.

The vast majority of this complexity is incidental to the iteratee concept and can be eliminated by further abstraction.  Toward that end, I'd like to present yet another iteratee implementation, based on a radically different approach to the problem:  defining iteratees as a stack of already well-understood monad transformers.  By doing so, I hope to develop a clear operational semantics for at least one interpretation of the iteratee concept.

This approach will be presented in three parts.  First, section \ref{sec:top_down} works through a simple top-down design process starting with a laundry list of features that iteratees should have and gradually building up a monad transformer stack that implements them.  Second, section \ref{sec:bottom_up} demonstrates a bottom up analysis looking at an existing iteratee implementation and reverse-engineering it into an equivalent monad transformer stack.  Finally, section \ref{sec:enumerators} explores one possible high-level formulation of enumerators that complements the iteratees developed in section \ref{sec:iteratees}.

This is not intended to be the one true specification or even a serious implementation.  Rather, consider this an example of an approach for formally specifying the semantics, an alternative to the usual approach where the implementation is the only specification.

The primary reason, I believe, that a treatment like this one is not already commonplace is the challenge of finding a monad transformer that effectively models interruptible computations without opening the |ContT| Pandora's Box.  The rest of the things we expect of iteratees are easy to provide with well-known monad transformers.  The |Program| monad and its associated transformer |ProgramT| (from the ``operational'' package) do exactly that.  Iteratees can be viewed as |Program| monads with a very simple instruction set consisting of just one operation:  Get more input.  I believe this is the only previously-missing piece of the ``iteratee semantics'' puzzle.

\section{Iteratees} % (fold)
\label{sec:iteratees}

% section intro (end)

\subsection{Top-down Iteratee Design} % (fold)
\label{sec:top_down}

This section will be introducing several different iteratee implementations that incrementally add features, so I'll start by defining those features in terms of a few type classes that express the primitive operations associated with them.

\subsubsection{Fundamentals:  What an iteratee \em is \em} % (fold)
\label{sub:fundamentals_what_an_iteratee_is}

    First, the bare minimum:  As I see it, to be an iteratee is to be a process that consumes input and eventually returns a response.  Thus, |Iteratee| is a subclass of |Monad| with a single operation, |getInput|, that asks for more input.  The other side of that operation, the means by which an enumerator feeds that input to the iteratee, will be discussed in another section.  A few useful operations on streams will be defined in Appendix  \ref{sec:simple_operations_on_streams} and used throughout the rest of the code.

\begin{code}

data Stream sym = EOF | Chunks [sym] deriving (Eq, Show)
type IterStream it = Stream (Symbol it)
class Monad it => Iteratee it where
    type Symbol it
    getInput  :: it (IterStream it)

\end{code}

% subsection fundamentals_what_an_iteratee_is (end)
\subsubsection{A practical consideration: look-ahead} % (fold)
\label{sub:a_practical_consideration_look_ahead}

Second, for practical iteratees we'll also want lookahead of a limited sort.  We want to be able to get a prefix of the available input without consuming all of it.  We also want to be able to see what input is available without consuming any of it or causing the enumerator to do any additional work.  Traditionally, an |unget| operation is also provided, but I prefer not to include it even though all of the implementations here could support one.  It really doesn't seem like it ought to be possible to ``put back'' arbitrary data that may or may not ever have been read from the stream in the first place.  In a real-world implementation I might expect to see an |unget| operation in a separate @.Internal@ module or something, with its use discouraged and a tacit expectation that speed freaks will probably make use of it anyway.

\begin{code}

class Iteratee it => Lookahead it where
    getSymbols  :: Int ->  it (IterStream it)
    lookahead   ::         it (IterStream it)

\end{code}


% subsection a_practical_consideration_look_ahead (end)
\subsubsection{Exception handling} % (fold)
\label{sub:exception_handling}

(Traditional) exception handling:  The |MonadError| class is actually sufficient for this purpose, but let's a new class that is explicitly about iteratees anyway just for emphasis.

\begin{code}

class Iteratee it => IterateeError it where
    type Exc it
    throw   ::    Exc it -> it a
    handle  :: (  Exc it -> it a) -> it a -> it a

\end{code}

% subsection exception_handling (end)

\subsubsection{Implementations} % (fold)
\label{sub:implementations}

% subsection implementations (end)

Now for some implementations.  Here's a minimalist iteratee, without support for any of the fancy stuff like lookahead, exceptions, etc.  Since we're working primarily with monad transformers, it's trivial for us to include an underlying monad in our simplest example, so we'll go ahead and do so.

\begin{code}

data Fetch sym a where
    Fetch :: Fetch sym (Stream sym)

newtype Iter1 sym m a = Iter1 (ProgramT (Fetch sym) m a)
    deriving (Functor, Monad, MonadTrans)

runIter1 (Iter1 p) = viewT p >>= step
    where
        step :: Monad m => ProgramViewT (Fetch sym) m a -> m a
        step (Return x)      = return x
        step (Fetch :>>= k)  = viewT (k EOF) >>= step

instance Monad m => Iteratee (Iter1 sym m) where
    type Symbol (Iter1 sym m) = sym
    getInput = Iter1 (singleton Fetch)

\end{code}

We can easily add lookahead by throwing a state monad onto the stack (note that |Iter2|'s |getInput| doesn't call |Iter1|'s |getInput| unless the |Stream| state is |EOF| or |Chunks []|):

\begin{code}

newtype Iter2 sym m a = Iter2 (Iter1 sym (StateT (Stream sym) m) a)
   deriving (Functor, Monad)
instance MonadTrans (Iter2 sym) where
    lift = Iter2 . lift . lift

runIter2 (Iter2 i) = runStateT (runIter1 i) (Chunks [])

instance Monad m => Iteratee (Iter2 sym m) where
    type Symbol (Iter2 sym m) = sym
    getInput = do
        stashed <- lookahead
        Iter2 $ if isEmpty stashed
            then do
                input <- getInput
                lift (put (takeStream 0 input))
                return input
            else do
                lift (put (Chunks []))
                return stashed

instance Monad m => Lookahead (Iter2 sym m) where
    lookahead = Iter2 (lift get)
    getSymbols n = do
        input <- getInput
        if isEOF input
            then return EOF
            else do
                let (result, rest)  = splitStreamAt n input
                    nResults        = streamLength result
                Iter2 (lift (put rest))
                if isEmpty rest && nResults < n
                    then do
                        more <- getSymbols (n - nResults)
                        return (appendStream result more)
                    else return result

\end{code}

To that we can add exception handling with |ErrorT|:

\begin{code}

newtype Iter3 e sym m a = Iter3 (ErrorT e (Iter2 sym m) a)
    deriving (Functor, Monad, MonadError e)
instance Error e => MonadTrans (Iter3 e sym) where
    lift = Iter3 . lift . lift

runIter3 (Iter3 i) = runIter2 (runErrorT i)

instance (Error e, Monad m) => Iteratee (Iter3 e sym m) where
    type Symbol (Iter3 e sym m) = sym
    getInput = Iter3 (lift getInput)

instance (Error e, Monad m) => Lookahead (Iter3 e sym m) where
    lookahead   = Iter3 (  lift    lookahead  )
    getSymbols  = Iter3 .  lift .  getSymbols

instance (Error e, Monad m) => IterateeError (Iter3 e sym m) where
    type Exc (Iter3 e sym m) = e
    throw   = throwError
    handle  = flip catchError

\end{code}

There are many other interesting monad transformers that we could put into the stack.  There are also other interesting constructors we could add to our |ProgramT|'s ``instruction'' GADT.  For example, either approach could be used to implement resumable exceptions (either by adding another |ProgramT| layer or by adding constructors to the |Fetch| GADT to reperesent exceptions).  This is why they appear to be such a natural fit in Oleg's implementation:  As we'll see later, his exception system is equivalent to the latter.

This construction of iteratees requires a much broader knowledge base to digest than the existing bottom-up presentations, and the code involves a fair amount of syntactic noise with all the lifting and newtype wrapping and unwrapping.  Ultimately, though, I find it considerably simpler to understand because it involves combining a small number of already-well-understood concepts.  And to be honest I think that most people that are able to really understand any of the existing expositions of iteratees probably can digest this one as well.

Much more importantly, having a monad transformer stack as a reference implementation allows implementors to make their iteratees vastly simpler to use;  a complete set of primitive operations can be derived as a combination of the primitive operations of each of the monad-transformer layers, minus anything the implementor prefers to keep abstract.  This benefit comes merely from the existence of this kind of model - the implementation need not be the same.  It can be quite aggressively refactored or optimized as long as it provides the set of primitives chosen from the reference model.

% section top_down (end)
\subsection{Bottom-up Iteratee Analysis} % (fold)
\label{sec:bottom_up}

That last point raises an interesting question.  If we already have an implementation of iteratees, can we easily ``retrofit'' a monad-transformer-stack semantics from which to derive an appropriate set of primitives?  Here is a somewhat informal procedure I have found useful for this purpose.

We'll start by looking at an existing implementation and rewriting it in a simple type-structural notation.  Let's use the implementation from Oleg Kiselyov's original IterateeM.hs as a worked example:

\begin{spec}

type ErrMsg = SomeException
data Stream el = EOF (Maybe ErrMsg) | Chunk [el]

data Iteratee el m a 
    =  IE_done a
    |  IE_cont  (Maybe ErrMsg)
                (Stream el -> m (Iteratee el m a, Stream el))

    
\end{spec}
Which gives us the basic equations:
\begin{eqnarray*}
ErrMsg &=& SomeException \\
Stream(el) &=& 1 + ErrMsg + List(el) \\
Iteratee(el,m,a) &=& a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)})
\end{eqnarray*}
(we won't need to look inside streams, let alone lists, but for completeness recall that $List(x) = 1 + x * List(x)$)

Now, considering this all as a symbolic algebra problem, combine them with some functions corresponding to known monad transformers and manipulate the equations till they reach a suitably simple form.  Here are some equations corresponding to a few monad transformers I've found useful:
\begin{eqnarray*}
ReaderT(r,m,a) &=& m(a) ^ r \\
WriterT(w,m,a) &=& m(a * w) \\
StateT(s,m,a) &=& ReaderT(s,m,WriterT(s,m,a)) \\
&=& m(a * s) ^ s \\
ErrorT(e,m,a) &=& m(e + a) \\
ProgramT(instr,m,a) &=& m (ProgramViewT(instr,m,a)) \\
ProgramViewT(instr,m,a) &=& a + \sum_t (instr(t) * ProgramT(instr,m,a) ^ t)
\end{eqnarray*}

The ProgramViewT equation probably requires a bit of explanation.  The $\sum_t$ component corresponds to existential quantification over a new type variable $t$.  Interpreting the $instr$ type as a function that maps each set of type parameters to the equation for all constructors that can yield that assignment of type parameters, the notation means exactly what the summation operation suggests.  For example, the following GADT:
\begin{spec}
data Foo a b where
    Bar  :: Int -> String -> Foo X Y
    Baz  :: Foo Z Z
\end{spec}
would map to the following function (written in a pseudo-Haskell style with pattern matching on the type arguments):
\begin{eqnarray*}
Foo(X,Y) &=& Int * String \\
Foo(Z,Z) &=& 1 \\
Foo(\_,\_) &=& 0
\end{eqnarray*}
so $\sum_{a,b} (Foo(a,b) * Bar(a))$ would expand to $Foo(X,Y) * Bar(X) + Foo(Z,Z) * Bar(Z)$, which (by evaluating $Foo$) simplifies to $Int * String * Bar(X) + Bar(Z)$.  So there is at least one isomorphism between $\exists a, b.$ |(Foo a b, Bar a)| and |Either| |(Int,String,Bar X)| |(Bar Z)|.

With all this in mind, here are two rewrites of the |Iteratee| equations above.  We first hypothesize, based on superficial similarities of the corresponding equations, that $Iteratee(el,m,a)$ is isomorphic to $ProgramViewT(f,n,b)$ for some $f$, $n$ and $b$:
$$ Iteratee(el,m,a) = ProgramViewT(f,n,b) $$
\begin{eqnarray*}
& & a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)}) \\
&=& b + \sum_t (f(t) * n(ProgramViewT(f,n,b)) ^ t)
\end{eqnarray*}
From here, $a = b$ is an easy assumption, which leaves:
\begin{eqnarray*}
& & (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)}) \\
&=& \sum_t (f(t) * n(ProgramViewT(f,n,b)) ^ t)
\end{eqnarray*}

There are at least two possible ways to unify these two expressions, arising from different possible choices of the function $f$.  One way is to declare that $f$ has only one value in its range: $Stream(t)$.  We can let $f$ be either:
\begin{eqnarray*}
F(el, 1 + ErrMsg) &=& Stream(el) \\
F(el, \_)         &=& 0
\end{eqnarray*}
or
\begin{eqnarray*}
F(el, 1)      &=& Stream(el) \\
F(el, ErrMsg) &=& Stream(el) \\
F(el, \_)     &=& 0
\end{eqnarray*}

These assignments correspond to GADTs:
\begin{spec}
data F el t where
    F :: Maybe ErrMsg -> F el (Stream el)
\end{spec}
or
\begin{spec}
data F el t where
    F1  :: F el (Stream el)
    F2  :: ErrMsg -> F el (Stream el)
\end{spec}
respectively.  So (informally currying F):
\begin{eqnarray*}
& & (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^{Stream(el)}) \\
&=& (1 + ErrMsg) * n(ProgramViewT(F(el),n,b) ^{Stream(el)}) \\
&=& (1 + ErrMsg) * n(Iteratee(el,m,a) ^{Stream(el)}) \\
\ \\
n(Iteratee(el,m,a)) &=& m(Iteratee(el,m,a) * Stream(el)) \\
\ \\ 
n &=& WriterT(Stream(el),m) \\
Iteratee(el,m,a) &=& ProgramViewT(F(el),WriterT(Stream(el),m), a)
\end{eqnarray*}

It is important to note that this is only an isomorphism of types, and in particular does \em NOT \em say that the |Monad| operations that would be provided by library implementations of these monad transformers are the same as the original implementation's.  The fact that we have an isomorphism of types, though, means that we can push the implementation's existing operations through to the new type.  This is also a valuable exercise because it lets us restate exactly what the implementation was doing in a language of our choosing or, as in this case, see that the type we have come up with is really not a very good fit after all.

Oleg's implementation had state-passing machinery in |>>=| which, when pushed through our isomorphism, makes enumerators responsible for knowing when the iteratee has returned unconsumed input and feeding it back to them before generating any more.  So although we can shoehorn the iteratee concept into a writer monad, doing so puts an unnecessary burden on our enumerators and really does not capture the spirit of what's going on.

Let's go back now to the choice of $f$ (which we'll start calling $g$ to emphasize that we are now making a different choice of function).  We'll try another sensible function in an effort to find a type more like a state monad.  Let the unit type be the only element of $g$'s range:
\begin{eqnarray*}
G(1 + ErrMsg) &=& 1 \\
G(\_) &=& 0
\end{eqnarray*}
or, as a GADT:
\begin{spec}
data G where
    G :: Maybe ErrMsg -> G ()
\end{spec}

We can see from its type that |G| is an operation with \em only \em side effects.  This reinforces Oleg's identification of the iteratee's continuation as a ``resumable exception''.  It requests outside intervention to make things better - more input, handle some exception, etc.  Our equations are now:
\begin{eqnarray*}
& & (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)}) \\
&=& \sum_t (G(t) * n(ProgramViewT(G,n,a)) ^ t) \\
&=& (1 + ErrMsg) * n(ProgramViewT(G,n,a)) ^ 1 \\
&=& (1 + ErrMsg) * n(ProgramViewT(G,n,a) \\
\ \\
& & n(ProgramViewT(G,n,a)  \\
&=& (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)}) \\
&=& StateT(Stream(el),m,Iteratee(el,m,a))
\end{eqnarray*}

Putting everything together\footnotemark:
\begin{eqnarray*}
& & Iteratee(el,m,a) \\
&=& a + (1 + ErrMsg) * (m(Iteratee(el,m,a) * Stream(el)) ^ {Stream(el)}) \\
&=& a + (1 + ErrMsg) * StateT(Stream(el),m,Iteratee(el,m,a)) \\
&=& a + \sum_t (G(t) * StateT(Stream(el),m,ProgramViewT(G,n,a)) ^ t) \\
&=& ProgramViewT(G,StateT(Stream(el),m),a)
\end{eqnarray*}

\footnotetext{Incidentally, the fact that this |Iteratee| is equivalent to |ProgramViewT| and not to |ProgramT| exposes a subtle problem with the implementation (although it will have been obvious to some already):  This |Iteratee| type is \em NOT \em a monad transformer.  It violates the law that requires |lift . return = return|.  |lift (return x)| will ask the enumerator for input before passing on |x| to the rest of the program.  I'm not entirely sure whether it obeys the monad laws either, though I suspect it does.  I have checked the identity laws but not associativity.}

This is a nicer conclusion than the previous one because it formalizes Oleg's informal remarks that an iteratee is a kind of a state monad.  From this definition we can see that it really is.  Keep in mind, again, that the operations we get for free from the library implementations are not \em exactly \em the same as the ones we get when we push the original type's capabilities through our isomorphism.  It's mostly a standard |ProgramT| monad, but due to the original implementation of |>>=|, the |G Nothing| operation is effectively a no-op unless the state is empty.

It is an eye-opening (and highly recommended) exercise to perform this sort of derivation for several different implementations of iteratees and compare the resulting transformer stacks.  It's particularly interesting to note just how widely varied the semantics are.

% section bottom_up (end)
% part iteratees (end)
\section{Enumerators} % (fold)
\label{sec:enumerators}

%include HighLevelEnumerators.lhs

% part enumerators (end)
\appendix

\section{Simple operations on Streams} % (fold)
\label{sec:simple_operations_on_streams}

\begin{code}

streamToList EOF         = []
streamToList (Chunks cs) = cs

streamLength  str = length  (streamToList str)
isEmpty       str = null    (streamToList str)

isEOF    EOF          = True
isEOF    _            = False

appendStream s1   s2
    | isEOF s1 && isEOF s2  = EOF
    | otherwise             = Chunks (streamToList s1 ++ streamToList s2)

splitStreamAt n EOF          = (EOF,        EOF        )
splitStreamAt n (Chunks cs)  = (Chunks xs,  Chunks ys  )
    where (xs,ys) = splitAt n cs

takeStream  n = fst  . splitStreamAt n
dropStream  n = snd  . splitStreamAt n

\end{code}

% section simple_operations_on_streams (end)

\end{document}
