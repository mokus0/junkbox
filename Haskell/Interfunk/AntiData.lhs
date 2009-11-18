\documentclass[10pt]{article}
%include polycode.fmt

\begin{document}

\title{AntiData: computing from negative reality}
\author{James Cook}
\date{2008}

\maketitle

%if 0
\begin{code}
        
        -- code in here won't end up in the TeX document
        module AntiData where
        
\end{code}
%endif

interfunK does not provide any means of defining functions directly.  Instead,
a function is constructed by constructing a value from antidata.  antidata is
actually not a totally exotic concept; it is basically just the concept of
a ``free variable."  When combined with the names-as-functions concept though,
it ought to enable some sufficiently bizarre constructs.

consider the following pseudocode:

>       x := READ "foo"
>       y := READ "bar"
>       WRITE "eep" (x + y)

ignoring the interfunK name-concept for the moment, and presuming that
|foo| and |bar| are not presently defined (or by some other means are
determined to be antidata), |eep| is now defined as a function which, when
evaluated, looks up |foo| and |bar|, adds them, and returns the result.

Of course, once all the other layers of language weirdness are tacked on
(especially the name matching system), this concept should be a bit harder
to grok.

\begin{code}
        
        -- your code here
        
\end{code}
	
\end{document}
