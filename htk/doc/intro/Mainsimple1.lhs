% This file contains a lot of -*- mode: Latex -*-
%
\subsection{A First Example}

\label{ssec:ex1}

To make this more concrete, consider a very simple example. We want to
open a window which contains just one button, which should be labelled
\textit{Press me!}. Whenever the user obligingly presses the button,
it should change its label to a different random string.

The static part of this program is fairly simple. There will be an
initialization function (which opens the window and such), and we want
to build a button with the inscription \texttt{Press me}. The
following code achieves this:

\begin{comment}
\begin{code}
module Main where

import HTk    
import Random
\end{code}
\end{comment}

\begin{code}  
main:: IO ()
main =
  do main <- initHTk []

     b <- newButton main [text "Press me!"] 
     pack b []
\end{code}
This introduces three important concepts in \HTk:
\begin{itemize}
\item firstly, the elements of the graphical user interface are
  organized hierarchically. When we create a new button, we have to
  pass it the GUI element which it is part of (here, the main window).
\item secondly, GUI elements are created with functions called
  \texttt{new}X, which take a \emph{configuration} list as
  argument. The configuration determines the visual appearance; here,
  the text which is displayed on the button
\item thirdly, creating a GUI element does not display it \textit{per
    se}. To display it, we have to explicitly place it on the screen;
  this is done with the \texttt{pack} command. This command also takes
  a list of configurations as arguments; more on that below.
\end{itemize}

To specify the dynamic behaviour, we need two ingredients: firstly, we
need to connect the external event of the user clicking the button
with an element of the data type \texttt{Event}, and secondly, we need
to set up the program such that it reacts to the occurence of this
event by changing the button's label. 

Setting up external events to produce an \texttt{Event a} is called
\emph{binding}. When we bind an external event, we specify the
external action that we wish to bind (e.g. this button being clicked,
mouse movement over this window, right button being clicked with
control-key being pressed and user doing a handstand whilst whistling
"'Auld Lang Syne"`). The general case is the \texttt{bind} function
which we will see below, but for the simple case of a button being
clicked, we can use the function \texttt{clicked :: Button a-> IO
  (Event ())}. 

The composed event we want to synchronise on is the click of the
button, followed by changing the label. The following code achieves
the desired effect:
\begin{code}
     click <- clicked b
     spawnEvent 
      (forever 
        (click >>> do nu_label <- mapM randomRIO (replicate 5 ('a','z'))
                      b # text nu_label))
     finishHTk
\end{code}     
Here, \texttt{randomRIO (replicate 5 ('a','z'))} generates a list of
five actions of type \texttt{IO Char}, and \texttt{mapM} evaluates
them to a random string of length 5. The next line sets the label to
this random string; how exactly this works will be explained below.

Two more functions require an explanation here: \texttt{forever ::
  Event a-> Event a} takes an event, and returns this event composed
with itself. Thus, synchronising on this event will synchronise on it
once, then wait for this event occuring again. The effect here is that
the effect we want to achieve occurs recurrently. Had we left out the
\texttt{forever}, our program would just wait for one button press,
change the colour of the button once and go on its merry way (in this
case, terminate). With \texttt{forever}, we have it wait for the next
button press after the first one occurs. 

Finally, \texttt{spawnEvent} takes an event, and creates a concurrent
thread which synchronises on this event. This is not strictly
necessary here, since we do nothing else, but it is good practice to
leave handling of events to threads different from the main thread.
Exactly how many threads one creates --- one for each button, or just
one for the whole GUI --- is a matter of taste and judgement.

At the end of the program, the main thread has to wait for the GUI to
finish; if it just exited, the whole program would terminate. We do
this by calling \texttt{finishHTk}. This also handles the case that
the user closes the window by external means (e.g. the close button
provided by the window manager).

Note that our program is non-terminating. If the window manager does
not provide means to close a running application, we will have to use
\texttt{kill} or \texttt{xkill} to stop it. This is clearly
unsatisfactory, so we will now provide a second button to close the
window regularly. 

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: "intro"
%%% End: 
