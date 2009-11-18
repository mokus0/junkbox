\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{The interfunK ``Name'' Concept}
\author{James Cook}
\date{2008}

\maketitle

%if 0
\begin{code}
        
        -- code in here won't end up in the TeX document
        module Whatever where
        
\end{code}
%endif

Names in interfunK, like everything else, are functions.  Name resolution
proceeds as follows:

First, the name being looked up is matched against all names in the current
scope (which is itself a function, of course).  This match is purely static,
probably based on some form of type-compatibility.

If multiple names match, there may be a resolution phase involving the
execution of the names, and the invocation of the ordering system (see
@TheQuantum.lhs@) on the resulting values.  Note that even after passing
through the ordering system, multiple matches may be found.  Every situation
requiring a name lookup must be prepared to accept this gracefully.

\begin{code}
        
        -- your code here
        
\end{code}
	
\end{document}
