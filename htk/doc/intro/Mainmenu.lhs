% This file contains a lot of -*- mode: Latex -*-
%
\section{Menues}
\label{sec:menues}

Menues in \HTk are nots straightforward, mainly due to Tk's awkward
way of handling menues. Rather surprisingly, menues need \emph{not} be
packed.

Menues are modelled by the type
\href{Menu.html#Menu.Menu}{\texttt{Menu}}. Menues can either be the
normal things which appear in a menu bar (typically at the top of a
window), or attached to a single button, or pop-up menus which pop up
out of nowhere. Actually, these are typically considered bad interface
design, since they rarely confirm to user expectations (i.e. the user
has no way of knowing wether a pop-up menu will appear). Try to use
pop-up menues only as optional, convenient short-cuts. While we're at
it, try also to use one menu bar only, at the top of the window, since
several menu bars in one window will be confusing, and try not to
change the menus while the user isn't looking.

Menus in \HTk can contain:
\begin{itemize}
\item simple commands (which are instance of the \texttt{HasClicked}
  class),
\item submenues (cascades),
\item checkbuttons (boolean toggles),
\item radiobuttons (multiple exclusive selections),
\item and separators.
\end{itemize}

Checkbuttons and radio buttons have a state, which can be
access by attaching a \emph{Tk variable} to them (class
\href{HTk.html#HTk.HasVariable}{\texttt{HasVariable}}; a
\texttt{TkVariable} is created with
\href{HTk.html#HTk.createTkVariable}{\texttt{createTkVariable}}).

Here, we construct a typical menu. We start with creating a window and
all that:
\begin{code}
module Main (main) where
import HTk

main :: IO ()
main =
 do main <- initHTk [text "Menus!", size(300, 300)]
\end{code}
Now, we create the element holding the menu, and attach it as a
menubar to the window. The second parameter to \texttt{createMenu} is
a boolean determining wether this is a tear-off menu\footnote{This should be a
  configuration, but for tedious technical reasons isn't.} (i.e. a
menu which you can tear off from its menu button, and keep open).
\begin{code}
    menubar <- createMenu main False []
    main # menu menubar
\end{code}
Each pull-down menu in the menubar is in fact a submenu; we first
create such a submenu, and then attach a menu to it:
\begin{code}
    pd1 <- createMenuCascade menubar [text "First Menu"]
    m1 <- createMenu menubar False []
    pd1 # menu m1
\end{code}
Since creating a pull-down menu is such a common task, the utility
function \texttt{createPulldownMenu} has been provided; it does
exactly what the previous three lines have done, and will be used in
the rest of this wee example. Now we create the three simple menu
items, followed by a separator and a submenu of two more items: 
\begin{code}
    c1 <- createMenuCommand m1 [text "First Menu Point"]
    c2 <- createMenuCommand m1 [text "Second Menu Point"]
    createMenuSeparator m1 []
    s   <- createPulldownMenu m1 [text "Submenu"]
    c31 <- createMenuCommand s [text "First Subpoint"]
    c32 <- createMenuCommand s [text "Second Subpoint"]
\end{code}    
  
Now we create a second pulldown menu with the a check button, and a
group of three radio buttons. Note that Tk variables can hold all
types which are instances of the class \texttt{GUIValue}, in
particular strings and characters (and note how we have to
disambiguate the overloaded numerals).
\begin{code}
    m2 <- createPulldownMenu menubar [text "Buttons"]
    v1 <- createTkVariable True
    c1 <- createMenuCheckButton m2 [text "I am cool", variable v1]
 
    createMenuSeparator m2 []

    v2 <- createTkVariable (0::Int)
    r1 <- createMenuRadioButton m2 [text "No milk or sugar", value (0::Int),
                                    variable v2]
    r2 <- createMenuRadioButton m2 [text "Milk, no sugar", value (1::Int),
                                    variable v2]
    r3 <- createMenuRadioButton m2 [text "Sugar and milk", value (2::Int),
                                    variable v2]
\end{code}
This code in itself defines the menu. To query the state of the
variables, we bind the check button, and spawn a thread which reads
the variables. However, you do not need to bind anything to a button
if you do not want to react on it being pressed, and just want to read
the value of the variable at some point. A menu command is an instance
of class \texttt{HasCommand}, so you can use \texttt{clicked} to bind
a simple event to it (see
Sect.~\href{ssec:teenage-clicks}).

\begin{code}                                  
    cl <- clicked c1
    spawnEvent (forever (
         (cl >>> do val1 <- readTkVariable v1
                    val2 <- readTkVariable v2
                    putStrLn ("v1: "++  show val1++ 
                              ", v2: "++ show val2))))
    finishHTk
\end{code}

The source for the preceding example is in
\texttt{htk/doc/intro/Mainmenu.lhs}. Other uses of menues--- in
particular pop-up menues--- can be found in
\texttt{htk/examples/simple/Mainmenu.hs}.

The reader will probably agree that this is a lot of code for such a
simple task, and quite unnecessarily so. For the convenient design of
menus, we recommend the menu modules from the toolkit (see
Sect.~\ref{ssec:forms-menus}), which are at an abstraction level more
suitable for a functional language.

