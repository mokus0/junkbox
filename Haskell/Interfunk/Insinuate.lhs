\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{The interfunK event model}
\author{James Cook}
\date{2008}

\maketitle

%if 0
\begin{code}
        
        -- code in here won't end up in the TeX document
        module Insinuate where
        
\end{code}
%endif

An event is a thing that may happen during program execution.

It has a name (see $Whatever.lhs$), a priority (see $TheQuantum.lhs$),
and a payload (which, if the event makes it through the obstacle
course, will be executed).

Events are only partially ordered.  Beyond whatever order that is (I haven't
decided yet), there is no guarantee of any particular evaluation order.  In
particular, concurrency is possible.  Every event should probably be
implemented as an atomic transaction, though.

Note that user code may not explicitly schedule an event.  Events are
automatically scheduled by the runtime environment in order to implement
user code.  Events' names are well-defined, however, and user code may
manipulate events already scheduled, including duplicating, canceling and
reordering them.

\begin{code}
        
        -- your code here
        
\end{code}
	
\end{document}
