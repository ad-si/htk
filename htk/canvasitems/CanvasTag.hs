
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

-- | A canvas item can have several tags (handlers for a set of canvas
-- items).
class CanvasItem w => TaggedCanvasItem w where
  -- | Sets the tags for the specified canvas item.
  tags :: [CanvasTag] -> Config w
  tags cts item =
    mapM (\ct -> do
                   CanvasItemName name tid <-
                     getObjectName (toGUIObject ct)
                   cset item "tag" (show tid)) cts >> return item


-- -----------------------------------------------------------------------
-- tags
-- -----------------------------------------------------------------------

-- | The @CanvasTag@ datatype.
newtype CanvasTag = CanvasTag GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

-- | Constructs a new canvas tag.
createCanvasTag :: Canvas 
   -- ^ the parent canvas.
   -> [Config CanvasTag] 
   -- ^ the list of configuration options for this canvas tag.
   -> IO CanvasTag
   -- ^ A canvas tag.
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

-- | Internal.
instance GUIObject CanvasTag where 
  -- | Internal.
  toGUIObject (CanvasTag wid) = wid
  -- | Internal.
  cname _ = "CanvasTag"

-- | A canvas tag can be destroyed.
instance Destroyable CanvasTag where
  -- | Destroys a canvas tag.
  destroy = destroy . toGUIObject

-- | A canvas tag is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem CanvasTag

-- | You can synchronize on a canvas tag.
instance Synchronized CanvasTag where
  -- | Synchronizes on a canvas tag.
  synchronize w = synchronize (toGUIObject w)

                
-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Adds the canvas items identified by the @SearchSpec@ to
-- the tag.
addCanvasTag :: SearchSpec 
   -- ^ the search specification.
   -> CanvasTag 
   -- ^ the tag to add items to.
   -> IO ()
   -- ^ None.
addCanvasTag spec@(SearchSpec cmd) tag =
  do
    spec' <- cmd
    execMethod tag (\tnm -> tkAddTag tnm spec')

-- | Removes a canvas item from a canvas tag.
removeCanvasTag :: CanvasItem i => i 
   -- ^ the item to remove from the tag.
   -> CanvasTag 
   -- ^ the tag to remove the item from.
   -> IO () 
   -- ^ None.
removeCanvasTag item tag =
  do
    tnm <- getObjectName (toGUIObject tag)
    execMethod item (\cnm -> tkDTag cnm tnm)


-- -----------------------------------------------------------------------
--  SearchSpec
-- -----------------------------------------------------------------------

-- | The @SearchSpec@ datatype
-- (see @CanvasTag.addCanvasTag@).
data SearchSpec = SearchSpec (IO String)

-- | Adds all objects in the canvas.
allItems :: SearchSpec
   -- ^ A @SearchSpec@ object.
allItems = SearchSpec (return "all")

-- | Adds the item just above the given item in the display list.
aboveItem ::  CanvasItem item => item 
   -- ^ the item below the item to add.
   -> SearchSpec 
   -- ^ A @SearchSpec@ object.
aboveItem item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("above [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

-- | Adds the item just below in the given item in the display list.
belowItem ::  CanvasItem item => item 
   -- ^ the item above the item to add.
   -> SearchSpec 
   -- ^ A @SearchSpec@ object.
belowItem item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("below [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

-- | Adds the item(s) identified by the given handler (which can also be
-- another canvas tag).
withTag ::  CanvasItem item => item 
   -- ^ the canvas item handler.
   -> SearchSpec 
   -- ^ A @SearchSpec@ object.
withTag item = SearchSpec (do {
        tid <- getCanvasTagOrID (toGUIObject item);
        return ("withtag [" ++ declVar tid ++ "; list " ++ show tid ++ "]")
        })

-- | Adds the item closest to the given position.
closest :: Position 
   -- ^ the concerned position.
   -> SearchSpec
   -- ^ A @SearchSpec@ object.
closest pos@(x, y) =
  SearchSpec (return ("closest " ++ show x ++ " " ++ show y))

-- | Adds the items enclosed in the specified region.
enclosed :: Position 
   -- ^ the upper left position of the region.
   -> Position 
   -- ^ the lower right position of the region.
   -> SearchSpec
   -- ^ A @SearchSpec@ object.
enclosed pos1 pos2 =
  SearchSpec (return ("enclosed " ++ show pos1 ++ " " ++ show pos2))

-- | Adds the items overpalling the specified region.
overlapping :: Position 
   -- ^ the upper left position of the region.
   -> Position 
   -- ^ the lower right position of the region.
   -> SearchSpec
   -- ^ A @SearchSpec@ object.
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
