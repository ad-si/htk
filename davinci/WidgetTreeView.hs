{- #########################################################################

MODULE        : WidgetTreeView
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : A Browser for Widget Trees


   ######################################################################### -}


module WidgetTreeView (
        newWidgetTreeView

        ) where

import HTk
import DaVinci
import GUIIntrinsics

import Debug(debug)


-- ---------------------------------------------------------------------------
-- Building a Tree
-- ---------------------------------------------------------------------------

newWidgetTreeView :: GUIObject o => o -> IO Graph
newWidgetTreeView o  = do
        g <- newGraph [gapwidth 4, gapheight 40,text "Widget Tree"]
        newTree g (toGUIObject o)
        displayGraph g
        improveAll g
        return g


newTree :: Graph -> GUIOBJECT -> IO Node
newTree g o = do
        n  <- newWidgetNode g o
        chl <- getChildObjects o
        foreach chl (newSubtree g o n)
        return n


newSubtree :: Graph -> GUIOBJECT -> Node -> GUIOBJECT -> IO ()
newSubtree g o p c = do
        n <- newTree g c
        newEdge Nothing p n [pattern SolidLine]
        done    


newWidgetNode :: Graph -> GUIOBJECT -> IO Node
newWidgetNode g o = do {
        k <- getObjectKind o;
        newNode g Nothing [text (name k), shape Textual]
}  where n = (show . objectID) o
         name k = show k ++ "[" ++ n ++ "]"

