{- #########################################################################

MODULE        : DaVinciPopupMenu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : alpha
DESCRIPTION   : DaVinci Popup Menu.

( Much abbreviated by George Russell )

   ######################################################################### -}


module DaVinciPopupMenu (
   popupSelectionNode,
   popupSelectionEdge      
   
   ) where

import SIM
import DaVinciCore
import DaVinciEvent
import DaVinciNode
import DaVinciEdge

import Debug(debug)


-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

popupSelectionNode :: Graph -> IA Node
popupSelectionNode g =  
      listenDaVinci ev >>>= \(PopupSelectionNodeInf nid _) -> getNode g nid
        where ev = (g,PopupSelectionNode)


popupSelectionEdge :: Graph -> IA Edge
popupSelectionEdge g =  
        listenDaVinci ev >>>= \(PopupSelectionEdgeInf eid _) -> getEdge g eid
        where ev = (g,PopupSelectionEdge)
                

                
