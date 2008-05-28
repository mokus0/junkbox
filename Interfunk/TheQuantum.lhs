\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{The Quantum}
\author{James Cook}
\date{2008}

\maketitle

%if 0
\begin{code}
        
        -- code in here won't end up in the TeX document
        module TheQuantum where
        
\end{code}
%endif

interfunK's concept of ordering is quite advanced.  It supports simultaneously 
comparing arbitrary numbers of items of arbitrary types.  It is able to transcend
archaic notions of ``trichotomy'' and ``transitivity.''

order in interfunK is a dependently-typed function from a set of values to
either a set of values or to a new ordering function.  A well-typed and
fully-evaluated order operation will always result in a set of values.  This
set is traditionally expected to be a subset of the operation's inputs, but
there are actually no formal constraints on the result set.  For example, it
may be composed of several objects that the order operation invented out of
thin air.

I don't know whether the ordering concept will involve any special handling in
the syntax or the compiler; this source module may be repurposed to implement
general function-related stuff.  Or perhaps the name of this module will be
shifted to the antidata implementation.

\begin{code}
        
        -- your code here
        
\end{code}
	
\end{document}
