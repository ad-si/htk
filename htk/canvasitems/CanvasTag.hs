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

---
-- A canvas item can have several tags (handlers for a set of canvas
-- items).
class CanvasItem w => TaggedCanvasItem w where
---
-- Sets the tags for the specified canvas item.
  tags :: [CanvasTag] -> Config w
  tags cts item =
    mapM (\ct -> do
                   CanvasItemName name tid <-
                     getObjectName (toGUIObject ct)
                   cset item "tag" (show tid)) cts >> return item


-- -----------------------------------------------------------------------
-- tags
-- -----------------------------------------------------------------------

---
-- The <code>CanvasTag</code> datatype.
newtype CanvasTag = CanvasTag GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

---
-- Constructs a new canvas tag.
-- @param cnv     - the parent canvas.
-- @param cnf     - the list of configuration options for this canvas tag.
-- @return result - A canvas tag.
createCanvasTag :: Canvas -> [Config CanvasTag] -> IO CanvasTag
createCanvasTag cnv cnf =
  do
    wid <- createGUIObject (toGUIObject cnv) (CANVASITEM CANVASTAG [])
                           tagMethods
    configure (CanvasTag wid) cnf

                
-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

{-
instance Eq CanvasTag where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)
-}

---
-- Internal.
instance GUIObject CanvasTag where 
---
-- Internal.
  toGUIObject (CanvasTag wid) = wid
---
-- Internal.
  cname _ = "CanvasTag"

---
-- A canvas tag can be destroyed.
instance Destroyable CanvasTag where
---
-- Destroys a canvas tag.
  destroy = destroy . toGUIObject

---
-- A canvas tag is a canvas item (any canvas item is an instance of the
-- abstract <code>class CanvasItem</code>).
instance CanvasItem CanvasTag

---
-- You can synchronize on a canvas tag.
instance Synchronized CanvasTag where
---
-- Synchronizes on a canvas tag.
  synchronize w = synchronize (toGUIObject w)

                
-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

---
-- Adds the canvas items identified by the <code>SearchSpec</code> to
-- the tag.
-- @param spec    - the search specification.
-- @param tag     - the tag to add items to.
-- @return result - None.
addCanvasTag :: SearchSpec -> CanvasTag -> IO ()
addCanvasTag spec@(SearchSpec cmd) tag =
  do
    spec' <- cmd
    execMethod tag (\tnm -> tkAddTag tnm spec')

---
-- Removes a canvas item from a canvas tag.
-- @param item    - the item to remove from the tag.
-- @param tag     - the tag to remove the item from.
-- @return result - None.
removeCanvasTag :: CanvasItem i => i -> CanvasTag -> IO () 
removeCanvasTag item tag =
  do
    tnm <- getObjectName (toGUIObject tag)
    execMethod item (\cnm -> tkDTag cnm tnm)


-- -----------------------------------------------------------------------
--  SearchSpec
-- -----------------------------------------------------------------------

---
-- The <code>SearchSpec</code> datatype
-- (see <code>CanvasTag.addCanvasTag</code>).
data SearchSpec = SearchSpec (IO String)

---
-- Adds all objects in the canvas.
-- @return result - A <code>SearchSpec</code> object.
allItems :: SearchSpec
allItems = SearchSpec (return "all")

---
-- Adds the item just above the given item in the display list.
-- @param item    - the item below the item to add.
-- @return result - A <code>SearchSpec</code> object.
aboveItem ::  CanvasItem item => item -> SearchSpec 
aboveItem item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("above [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

---
-- Adds the item just below in the given item in the display list.
-- @param item    - the item above the item to add.
-- @return result - A <code>SearchSpec</code> object.
belowItem ::  CanvasItem item => item -> SearchSpec 
belowItem item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("below [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

---
-- Adds the item(s) identified by the given handler (which can also be
-- another canvas tag).
-- @param item    - the canvas item handler.
-- @return result - A <code>SearchSpec</code> object.
withTag ::  CanvasItem item => item -> SearchSpec 
withTag item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("withtag [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

---
-- Adds the item closest to the given position.
-- @param pos     - the concerned position.
-- @return result - A <code>SearchSpec</code> object.
closest :: Position -> SearchSpec
closest pos@(x, y) =
  SearchSpec (return ("closest " ++ show x ++ " " ++ show y))

---
-- Adds the items enclosed in the specified region.
-- @param pos1    - the upper left position of the region.
-- @param pos2    - the lower right position of the region.
-- @return result - A <code>SearchSpec</code> object.
enclosed :: Position -> Position -> SearchSpec
enclosed pos1 pos2 =
  SearchSpec (return ("enclosed " ++ show pos1 ++ " " ++ show pos2))

---
-- Adds the items overpalling the specified region.
-- @param pos1    - the upper left position of the region.
-- @param pos2    - the lower right position of the region.
-- @return result - A <code>SearchSpec</code> object.
overlapping :: Position -> Position -> SearchSpec
overlapping pos1 pos2 = 
  SearchSpec (return ("overlapping " ++ show pos1 ++ " " ++ show pos2))

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
