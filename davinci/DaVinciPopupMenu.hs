{- #########################################################################

MODULE        : DaVinciPopupMenu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : alpha
DESCRIPTION   : DaVinci Popup Menu.


   ######################################################################### -}


module DaVinciPopupMenu (
        AppMenu,
        Application,

        popupInterActor,        

        popupSelectionNode,
        popupSelectionEdge      

        ) where

import SIM
import DaVinciCore
import DaVinciEvent
import DaVinciNode
import DaVinciEdge

import Menu

import Debug(debug)



-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

type AppMenu a = Menu (Application a)

type Application a = Graph -> a -> IO ()


-- ---------------------------------------------------------------------------
-- Popup Menu Interactor
-- ---------------------------------------------------------------------------

popupInterActor :: DaVinci 
        -> Graph 
        -> AppMenu a                    -- popup menu
        -> Maybe a                      -- popup selection
        -> (Graph -> IA a)              -- popup event 
        -> IO ()
popupInterActor daVinci graph menu lastMenu popupsel = do
        menuEvent <- getTrigger menu
        interactor (events daVinci graph menuEvent lastMenu)
 where  
   events daVinci graph menuEvent lastNodeOpt iact = 
         destroyed daVinci >>> 
            do
               destroy menu
               stop iact
      +> destroyed graph >>> 
            do
               destroy menu
               stop iact
      +> popupsel graph >>>= 
            (\ newNode -> 
               become iact 
                  (events daVinci graph menuEvent (Just newNode) iact)
               )
      +> menuEvent >>>= 
            (\ menuAction -> (incase lastNodeOpt) 
               (\ lastNode -> menuAction graph lastNode)
               )



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
                

                
