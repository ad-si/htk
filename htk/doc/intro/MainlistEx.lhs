\item Scrollbars
  
  I assume the esteemed reader has already seen a scrollbar. In HTk
  (and Tk), once you created a scrollbar you will need to connect it
  to the widget you want to scroll. This is done with the class
  \href{ScrollBar.html#ScrollBar.HasScroller}{HasScroller}: you create
  a scroll bar, and attach it to the scrollable widget with
  \texttt{scrollbar} option. The example below shows how to use
  scrollbars. Don't forget to pack both the scrollable widget and the
  scrollbar.


\item Listboxes
  
  A \href{ListBox.html}{listbox} has a list of several items, from
  which you can select one. Listboxes can be scrollable, which is most
  helpful since they can hold more items than are visible at a given
  time.  

  In HTk, list boxes are a polymorphic type, created with the
  following function. The contents of a list box are accessed and set
  with the configuration from the class
  \href{Configuration.html#Configuration.HasValue}: \begin{xcode}
  instance (GUIValue a, GUIValue [a]) => HasValue (ListBox a) [a]
  \end{xcode} When the user selects something from the the listbox,
  Tk's selection is set, which can be queried with the methods of the
  module \href{Selection.html}{\texttt{Select}}, in particular
  \texttt{getSelection}.\footnote{Yes, the selection classes are a bit
  over the top.} No event is generated --- if you want that, you need
  to bind the left mouse button (which generates the selection).

  Here is a short example which demonstrates the usage of listboxes,
  selections and scrollbars. Note the type constraint on the
  newListBox below --- we need this to force the type to
  \texttt{String}, since it can not be inferred.

\begin{code}
module Main (main) where

import HTk
main :: IO ()
main =
  do main <- initHTk [text "A Listbox"]
     lb  <- newListBox main [value numbers, bg "white", 
                            size (15, 10)] :: IO (ListBox String)
     pack lb [Side AtLeft]
     scb <- newScrollBar main []
     pack scb [Side AtRight, Fill Y]
     lb # scrollbar Vertical scb
     (press, _) <- bindSimple lb (ButtonPress (Just 1))
     spawnEvent (forever
       (press >> always 
          (do sel<- getSelection lb
	      putStrLn ("Selected "++ 
		        show (sel:: Maybe [Int])))))
     finishHTk where
  numbers = 
    ["One", "Two", "Three", "Four", "Five", "Six", "Seven",
     "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirtheen",
     "Fourteen", "Fifteen", "Sixteen", "Seventeen",
     "Eighteen", "Nineteen", "Twenty"]
\end{code}

\textbf{To be done: show a screenshot here.}


