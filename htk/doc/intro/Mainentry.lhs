\subsubsection{Entry}

An \emph{entry box} is a box which contains one editable line of
text. If is used to input short texts, such as a name. 

As opposed to previous widgets, entries are a polymorph over the type
of values they are supposed to hold and edit, hence an entry is
created with 
\begin{xcode}
  newEntry :: (Container par, GUIValue a) => 
              par -> [Config (Entry a)] -> IO (Entry a)
\end{xcode}

The current state of the input is the \emph{value} of the entry; it is
manipulated with the functions of class \texttt{HasValue}. Note that
the values need not be strings, but must be an instance of
\texttt{GUIValue}. 

Tk only provides the basic editing functions for entry widgets. If you
want to read the value of the entry when the return key is pressed,
you have to do this yourself by binding to the \texttt{KeyPress}
event. Here is a very basic example of how to use an entry widget: we
build an entry widget, and when the return key is pressed, we change
the window title to the entered text, and clear the entry widget.

\begin{comment}
\begin{code}
module Main where

import HTk

main :: IO ()
\end{code}
\end{comment}
\begin{code}
main = 
  do main <- initHTk [text "Entry example"]

     f <- newFrame main []
     l <- newLabel f [text "Rename: "]
     e <- (newEntry f [value ""])::IO (Entry String)

     (entered, _) <-
       bind e [WishEvent [] (KeyPress (Just (KeySym "Return")))]
        
     pack f []
     pack l [PadX 10, Side AtLeft]
     pack e [PadX 10, Side AtRight]

     spawnEvent (forever (entered >>> do txt <- (getValue e) :: IO String
                                         e # value ""
                                         main # text txt >> done))

     finishHTk  
\end{code}

An alternative to using values is to use \texttt{TkVariables}. These
are variables on the Tk side, which can be directly connected to the
widget in the sense that the variable always holds the entry's state.
The advantage of this approach is that we can share values across
widgets (see \texttt{Mainhello3.hs} in \texttt{examples/simple}).

There was a way to specify the maximal length of the text to be input,
but I have forgotten how. \texttt{TBD!} 

Finally, entry widgets implement the quite flexible indexing and
selection classes (see Sect.~\ref{ssec:indices} and
\ref{ssec:selections} below).
