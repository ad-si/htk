% This file contains a lot of -*- mode: Latex -*-
%

\subsubsection{Scrollbars}
  
  I assume the esteemed reader has already seen a scrollbar. In \HTk
  (and Tk), once you created a scrollbar you will need to connect it
  to the widget you want to scroll. This is done with the class
  \href{ScrollBar.html#ScrollBar.HasScroller}{HasScroller}: you create
  a scroll bar, and attach it to the scrollable widget with
  \texttt{scrollbar} option. The example below shows how to use
  scrollbars. Don't forget to pack both the scrollable widget and the
  scrollbar.


\subsubsection{Listboxes}
  
  A \href{ListBox.html}{listbox} has a list of several items, from
  which you can select one. Listboxes can be scrollable, which is most
  helpful since they can hold more items than are visible at a given
  time.  
  
  Just like entries, list boxes are a polymorphic type, created with
  the function \texttt{newListBox} with the by now familiar signature.
  As opposed to entries, list boxes (quite obviously) have lists of
  values:
\begin{xcode}
  instance (GUIValue a, GUIValue [a]) => HasValue (ListBox a) [a]
\end{xcode}
When the user selects something from the the listbox, Tk's selection
is set, which can be queried with the methods of the module
\href{Selection.html}{\texttt{Select}}, in particular
\texttt{getSelection}. No event is generated \textit{per se} --- if
you want that, you need to bind the left mouse button (which generates
the selection).

  Here is a short example which demonstrates the usage of listboxes,
  selections and scrollbars. Note the type constraint on the
  \texttt{newListBox} below --- we need this to force the type to
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
     lb # selectMode Extended
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

The position of entries in a list box can be indexed with instances of
the class \texttt{HasIndex}. For more on indices, see
Section~\ref{ssec:indices}, but the important instances here are
\begin{xcode}
instance HasIndex (ListBox a) Int Int
instance HasIndex (ListBox a) EndOfText Int
instance (Eq a, GUIValue a) => 
         HasIndex (ListBox [a]) (ListBoxElem a) Int  
\end{xcode}
In other words, the index is a number (starting with 0), the
\texttt{EndOfText} (only constructor of the synonymous data type), or
the element itself. 

A configuration particular to list boxes is the \emph{selection
  mode}. The \texttt{SelectMode} is an enumeration of four
constructor, which determines the way elements are selected in a list
box: 
\begin{itemize}
\item \texttt{Single} means a single element can be selected;
\item \texttt{Browse} means a single element can be selected, and the
  selection can be dragged with the mouse;
\item \texttt{Multiple} means more than one element can be selected
  by toggling the selection state (so to select three elements you
  have to select each of them in turn);
\item \texttt{Extended} means more than one element can be selected,
  with the first selection forming the so-called \emph{anchor}; shift
  and first button selects the range from the anchor to that entry;
  and control and first button toggles the selection state of single
  items. (This is the most common behaviour of list boxes in other GUI
  toolkits.)
\end{itemize}
Selections are handled by the selection classes (see
Section~\ref{ssec:selection}) below. You can set the selection, or
query the current selection as in the code above.

\ToBeDone{Show a screenshot here.}


