{- #########################################################################

MODULE        : XSelection
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : XSelections 

TO BE DONE    : Events that signals that the selection has been changed!


   ######################################################################### -}


module XSelection (
        module Index,
        module Selection,

        HasXSelection(..),

        XSelection(..),

        clearXSelection,
        getXSelection,
        getXSelectionOwner,
        setXSelectionOwner,

        lostXSelection

) where

import SIM
import GUICore
import Index
import Selection
import Screen
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
-- Class HasXSelection 
-- --------------------------------------------------------------------------

class HasSelection w => HasXSelection w where
        exportSelection         :: Bool -> Config w
        getExportSelection      :: w -> IO Bool
        exportSelection b w     = cset w "exportSelection" b
        getExportSelection w    = cget w "exportSelection"


-- --------------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------------

data XSelection = PRIMARY | CLIPBOARD deriving (Eq, Ord, Show, Read)

type TargetType = String        -- STRING, ATOM, INTEGER ...



-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance GUIValue XSelection where
        cdefault = PRIMARY


-- --------------------------------------------------------------------------
-- XSelection Commands
-- --------------------------------------------------------------------------

clearXSelection :: Screen -> XSelection -> IO ()
clearXSelection (Screen win) sel = 
        execMethod win (\nm  ->  ["selection clear -displayof " ++ show nm ++ " -selection " ++ show sel])


getXSelection :: GUIValue a => Screen -> XSelection -> TargetType -> IO a
getXSelection (Screen win) sel tp = 
        evalMethod win (\nm  ->  ["selection get -displayof " ++ show nm ++ " -selection " ++ show sel ++ " -type " ++ tp])


getXSelectionOwner :: Screen -> XSelection -> IO (Maybe Window)
getXSelectionOwner (Screen win) sel = do {
        str <- evalMethod win (\nm -> ["selection own -displayof " ++ show nm ++ " -selection "]);
        case dropWhile (isSpace) str of
                "" -> return Nothing
                nm -> lookupWindow nm
        }


setXSelectionOwner :: Window -> XSelection -> IO ()
setXSelectionOwner win sel = 
        execMethod win (\nm  -> 
           ["selection own -selection -command {} " ++ show sel ++ " " ++ show nm])


-- --------------------------------------------------------------------------
-- XSelection Events
-- --------------------------------------------------------------------------

lostXSelection :: Window -> IA ()
lostXSelection win = userinteraction win "LostXSelection" Notice >>> done











