-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

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

  createCanvasTag,

  addCanvasTag,
  removeCanvasTag

) where

import Core
import Canvas
import CanvasItem
import CanvasItemAux
import Destructible
import Synchronized
import Computation
import Geometry


-- -----------------------------------------------------------------------
-- class TaggedCanvasItem
-- -----------------------------------------------------------------------

class CanvasItem w => TaggedCanvasItem w where
        tags :: [CanvasTag] -> Config w
        tags cts item =
          mapM (\ct -> do
                         CanvasItemName name tid <-
                           getObjectName (toGUIObject ct)
                         cset item "tag" (show tid)) cts >> return item


-- -----------------------------------------------------------------------
-- tags
-- -----------------------------------------------------------------------

newtype CanvasTag = CanvasTag GUIOBJECT


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

createCanvasTag :: Canvas -> [Config CanvasTag] -> IO CanvasTag
createCanvasTag cnv ol =
  do
    wid <- createGUIObject (toGUIObject cnv) (CANVASITEM CANVASTAG [])
                           tagMethods
    configure (CanvasTag wid) ol

                
-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq CanvasTag where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject CanvasTag where 
  toGUIObject (CanvasTag wid) = wid
  cname _ = "CanvasTag"

instance Destroyable CanvasTag where
  destroy   = destroy . toGUIObject

instance CanvasItem CanvasTag

instance Synchronized CanvasTag where
  synchronize w = synchronize (toGUIObject w)

                
-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

addCanvasTag :: CanvasItem w => SearchSpec -> w -> IO ()
addCanvasTag (SearchSpec cmd) tag =
  do
    spec' <- cmd
    execMethod tag (\tnm -> tkAddTag tnm spec')

removeCanvasTag :: CanvasItem i => i -> CanvasTag -> IO () 
removeCanvasTag ci tag =
  do
    tnm <- getObjectName (toGUIObject tag)
    execMethod ci (\cnm -> tkDTag cnm tnm)


-- -----------------------------------------------------------------------
--  SearchSpec
-- -----------------------------------------------------------------------

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
getCanvasTagOrID wid =
  do
    nm <- getObjectName wid
    case nm of
      CanvasItemName name tid -> return tid  
      _ -> error "CanvasTag (getCanvasTagOrID) : not a canvas item name"


-- -----------------------------------------------------------------------
-- methods
-- -----------------------------------------------------------------------

tagMethods = canvasitemMethods {createCmd = tkCreateTag}


-- -----------------------------------------------------------------------
-- unparsing of commands
-- -----------------------------------------------------------------------

tkCreateTag :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
               [ConfigOption] -> TclScript
tkCreateTag _ (CANVASITEM CANVASTAG []) (CanvasItemName name tid) oid _ =
  [declVar tid, " set " ++ vname ++ " t" ++ show oid]
  where vname = (drop 1 (show tid))

tkAddTag :: ObjectName -> String -> TclScript
tkAddTag (CanvasItemName name tid) spec =
  [declVar tid, show name ++ " addtag " ++ show tid ++ " " ++ spec]


tkDTag :: ObjectName -> ObjectName -> TclScript
tkDTag (CanvasItemName name cid) (CanvasItemName _ tid) =
  [declVar tid, declVar cid, 
   show name ++ " dtag " ++ show cid ++ " " ++ show tid]

declVar :: CanvasTagOrID -> TclCmd
declVar tid = "global " ++ (drop 1 (show tid))
