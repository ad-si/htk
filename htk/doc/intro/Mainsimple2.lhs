% This file contains a lot of -*- mode: Latex -*-
%
\subsection{A Second Example}

\begin{comment}
\begin{code}
module Main where

import HTk    
import Random
\end{code}
\end{comment}

We have to augment our previous program in two aspects: statically, we
have to provide another button, and dynamically, we have to react to
this button being pressed by ending the program

For the first part, we create the second button just like the first
part. When we place it, we have to specify where it is going to be
placed.  We want it below the second button, and we want both buttons
to stretch out horizontally such that they are of the same length,
regardless of the size of the labels.  This is done by adding
\emph{packing options} to the \texttt{pack} command. Here,
\texttt{Side} says we want the first button at the top and the second
at the buttom, and \texttt{Fill X} specifies the stretching
behaviour mentioned above:

\begin{code}
main:: IO ()
main =
  do main <- initHTk []

     b <- newButton main [text "Press me!"]
     b2 <- newButton main [text "Close"]
     pack b [Side AtTop, Fill X]
     pack b2 [Side AtBottom, Fill X]
\end{code}

To change the dynamic behaviour, we first need the second button to
create an event with the \texttt{clicked} function. However, we need
to change the behaviour of the spawned event such that when this new
clicked event occurs, the program is finished. 

This combination of events as a case distinction --- "`when this event
occurs, do something, when the other event occurs, do something
different"' --- is achieved by the third important operation on
events, the \emph{choice} combinator \texttt{(+>) :: Event a-> Event
  a-> Event a}. Hence, we need to combine the previous dynamic
behaviour and the new behaviour by \texttt{+>}. The new behaviour,
finishing the program, is achieved by calling the \texttt{destroy}
action on \texttt{main}.  This closes the main window and lets the
program terminate gracefully:
\begin{code}
     click  <- clicked b
     click2 <- clicked b2
     spawnEvent 
      (forever 
        ((click >>> do nu_label <- mapM randomRIO (replicate 5 ('a','z'))
                       b # text nu_label
                       done)
        +> (click2 >>> destroy main)))
     finishHTk
\end{code}
Note that the choice occurs inside the \texttt{forever} (why?). We
could also have created two threads here, each listening to one
button. While in this simple situation, this would have been easier,
it is in general good practice to create only as many threads as
needed, since one otherwise tends to run into memory leaks by unused
threads lying around or even worse, nasty synchronisation problems.


%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "intro"
%%% End: 
