{- #########################################################################

MODULE        : CanvasTag
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : CanvasTag

TO BE DONE    : The canvas tag must maintain information on the constituent
                canvas items. A move of the canvas tag must then in turn
                imply that the cashed canvas coordinates are updated!!!

                The tags configure option must be implemented.


   ######################################################################### -}


module CanvasTag (
        module CanvasItem,

        CanvasTag,

        TaggedCanvasItem(..),

        SearchSpec,
        allItems,
        aboveItem,
        belowItem,
        withTag,
        closest,
        enclosed,
        overlapping,

        newCanvasTag,

        addCanvasTag,
        removeCanvasTag
        

        ) where

import Concurrency
import GUICore
import Canvas
import CanvasItem
import CanvasItemAux
import Debug(debug)


-- --------------------------------------------------------------------------
-- Class TaggedCanvasItem
-- --------------------------------------------------------------------------

class CanvasItem w => TaggedCanvasItem w where
        tags :: [CanvasTag] -> Config w
        tags = undefined                        -- TBD

                
-- --------------------------------------------------------------------------
-- Tags
-- --------------------------------------------------------------------------

newtype CanvasTag = CanvasTag GUIOBJECT


-- --------------------------------------------------------------------------
-- Configuration Options
-- --------------------------------------------------------------------------

newCanvasTag :: SearchSpec -> [Config CanvasTag] -> IO CanvasTag
newCanvasTag (SearchSpec cmd) ol = do
        spec' <- cmd
        wid <- createGUIObject (CANVASITEM (CANVASTAG spec') []) tagMethods
        configure (CanvasTag wid) ol

                
-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance Eq CanvasTag where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject CanvasTag where 
        toGUIObject (CanvasTag wid) = wid
        cname _ = "CanvasTag"

instance Destructible CanvasTag where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive CanvasTag

instance CanvasItem CanvasTag

instance Synchronized CanvasTag where
        synchronize w = synchronize (toGUIObject w)

                
-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

addCanvasTag :: CanvasItem w => SearchSpec -> w -> IO ()
addCanvasTag (SearchSpec cmd) tag = do {
        spec' <- cmd;
        execMethod tag (\tnm -> tkAddTag tnm spec')
        }


removeCanvasTag :: CanvasItem i => i -> CanvasTag -> IO () 
removeCanvasTag ci tag = do {
        mtname <- getObjectName (toGUIObject tag);
        incase mtname (\tnm -> execMethod ci (\cnm -> tkDTag cnm tnm))
        }


                
-- --------------------------------------------------------------------------
--  SearchSpec
-- --------------------------------------------------------------------------

data SearchSpec = SearchSpec (IO String)

allItems :: SearchSpec
allItems = SearchSpec (return "all")

aboveItem ::  CanvasItem w => w -> SearchSpec 
aboveItem w = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject w);
        return ("above [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

belowItem ::  CanvasItem w => w -> SearchSpec 
belowItem w = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject w);
        return ("below [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

withTag ::  CanvasItem w => w -> SearchSpec 
withTag w = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject w);
        return ("withtag [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })


closest :: Distance -> Distance -> SearchSpec
closest x y = SearchSpec (return ("closest " ++ show x ++ " " ++ show y))


enclosed :: Position -> Position -> SearchSpec
enclosed p1 p2 = SearchSpec (return ("enclosed " ++ show p1 ++ " " ++ show p2))


overlapping :: Position -> Position -> SearchSpec
overlapping p1 p2 = 
        SearchSpec (return ("overlapping " ++ show p1 ++ " " ++ show p2))


getCanvasTagOrID :: GUIOBJECT -> IO CanvasTagOrID
getCanvasTagOrID wid = do {
        nm <- getObjectName wid;
        case nm of
                (Just (CanvasItemName name tid)) -> return tid  
                _ -> raise objectNotPacked      
}


-- --------------------------------------------------------------------------
-- Methods
-- --------------------------------------------------------------------------

tagMethods = canvasitemMethods {createCmd = tkCreateTag}


-- --------------------------------------------------------------------------
-- Unparsing of Commands
-- --------------------------------------------------------------------------

tkCreateTag :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateTag (CANVASITEM (CANVASTAG spec) []) (CanvasItemName name tid) oid _ = [
        declVar tid, 
        " set " ++ vname ++ " t" ++ show oid,
        show name ++ " addtag " ++ show tid ++ " " ++ spec
        ]       
        where vname = (drop 1 (show tid))


tkAddTag :: ObjectName -> String -> TclScript
tkAddTag (CanvasItemName name tid) spec =
         [declVar tid, show name ++ " addtag " ++ show tid ++ " " ++ spec]


tkDTag :: ObjectName -> ObjectName -> TclScript
tkDTag (CanvasItemName name cid) (CanvasItemName _ tid) = [
        declVar tid, 
        declVar cid, 
        show name ++ " dtag " ++ show cid ++ " " ++ show tid
        ]


declVar :: CanvasTagOrID -> TclCmd
declVar tid = "global " ++ (drop 1 (show tid))
