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
popupInterActor dav g mn e popupsel = do
        mne <- getTrigger mn
        interactor (events dav g mne e)
 where  events dav g mne e iact = 
                destroyed dav >>> do{destroy mn; stop iact}
          +>    destroyed g   >>> do{destroy mn; stop iact}
          +>    popupsel g    >>>= (\e' -> 
                    become iact (events dav g mne (Just e') iact))
          +>    mne           >>>= \f -> (incase e) (\e' -> f g e')



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
                

                
