-- | The @module CanvasItem@ exports basic classes and
-- general functionality on canvas items.
module CanvasItem (

  Canvas,

  BitMapHandle(..),
  BitMap,

  HasCoords(..),

  CanvasItem,
  FilledCanvasItem(..),
  SegmentedCanvasItem(..),

  moveItem,
  scaleItem,

  raiseItem,
  lowerItem,
  putItemOnTop,
  putItemAtBottom,

  itemsNotOnSameCanvas

) where

import Core
import Configuration
import Geometry
import Colour
import Image
import BitMap
import Canvas
import Char(isSpace)
import Computation
import Synchronized


-- -----------------------------------------------------------------------
-- class CanvasItem, etc.
-- -----------------------------------------------------------------------

-- | Any canvas item is an instance of the abstract
-- @class CanvasItem@.
class GUIObject w => CanvasItem w

-- | You can set the coords (position \/ size) of a canvas item on the
-- parent canvas.
class HasCoords w where
  -- Sets the coord(s) of a canvas item on the parent canvas.
  coord           :: Coord -> Config w
  -- Gets the coord(s) of a canvas item on the parent canvas.
  getCoord        :: w -> IO Coord

-- | Any canvas item has coords on the parent canvas.
instance CanvasItem w => HasCoords w where
  -- Sets the coord(s) of a canvas item on the parent canvas.
  coord co item =
    do
      try (execMethod item (\nm -> tkCoordItem nm co))
      return item
  -- Gets the coord(s) of a canvas item on the parent canvas.
  getCoord item =  evalMethod item (\nm -> tkGetCoordItem nm)

-- | Any canvas item has a filling, outline, outline width and stipple
-- configuration.
class CanvasItem w => FilledCanvasItem w where 
  -- Sets the filling of a canvas item.
  filling         :: ColourDesignator c => c -> Config w
  -- Gets the filling of a canvas item.
  getFilling      :: w -> IO Colour
  -- Sets the outline colour of a canvas item.
  outline         :: ColourDesignator c => c -> Config w
  -- Gets the outline colour of a canvas item.
  getOutline      :: w -> IO Colour
  -- Sets the stipple configuration of a canvas item.
  stipple         :: BitMapHandle -> Config w
  -- Gets the stipple configuration of a canvas item.
  getStipple      :: w -> IO BitMapHandle
  -- Sets the outline width of a canvas item.
  outlinewidth    :: Distance -> Config w
  -- Gets the outline width of a canvas item.
  getOutlineWidth :: w -> IO Distance
  filling c w      = cset w "fill" (toColour c)
  getFilling w     = cget w "fill"
  outline c w      = cset w "outline" (toColour c)
  getOutline w     = cget w "outline"
  stipple b w      = setBitMapHandle w "stipple" b True
  getStipple w     = getBitMapHandle w "stipple"
  outlinewidth b w = cset w "width" b
  getOutlineWidth w = cget w "width"

-- | Segmented canvas items have a splinesteps and smooth configuration.
class CanvasItem w => SegmentedCanvasItem w where
  -- Sets the number of line segments that approximate the spline.
  splinesteps     :: Int -> Config w
  -- Gets the number of line segments that approximate the spline.
  getSplinesteps  :: w -> IO Int
  -- Sets the smooth configuration (if @true@ a spline curve is
  -- drawn around the points).
  smooth          :: Bool -> Config w
  -- Gets the actual smooth setting.
  getSmooth       :: w -> IO Bool
  splinesteps c w  = cset w "splinesteps" c
  getSplinesteps w = cget w "splinesteps"
  smooth c w       = cset w "smooth" c
  getSmooth w      = cget w "smooth"


-- -----------------------------------------------------------------------
-- canvas item operations
-- -----------------------------------------------------------------------

-- | Moves a canvas item horizontally and vertically by the given
-- distances.
moveItem :: (Synchronized w, CanvasItem w) =>
            w -> Distance -> Distance -> IO ()
moveItem item x y =
  synchronize item (execMethod item (\nm -> tkMoveItem nm x y))

-- | Scales a canvas item horizontally and vertically by the given
-- distances.
scaleItem :: (Synchronized w, CanvasItem w) =>
             w -> Distance -> Distance -> Double -> Double -> IO ()
scaleItem item x y xs ys =
  synchronize item (execMethod item (\nm -> tkScaleItem nm x y xs ys))


-- -----------------------------------------------------------------------
-- layering operations
-- -----------------------------------------------------------------------

-- | Moves an item above another item in the display list.
raiseItem :: (CanvasItem ci,CanvasItem w) => ci -> w -> IO ()
raiseItem item1 item2 =
  do
    onSameCanvas item1 item2
    nm2 <- getObjectName (toGUIObject item2)
    execMethod item1 (\nm1 -> tkRaiseItem nm1 (Just nm2))

-- | Moves an item below another item in the display list.
lowerItem :: (CanvasItem ci,CanvasItem w) => ci -> w -> IO ()
lowerItem item1 item2 =
  do
    onSameCanvas item1 item2
    nm2 <- getObjectName (toGUIObject item2)
    execMethod item1 (\nm1 -> tkLowerItem nm1 (Just nm2))

-- | Puts an item on top of the display list.
putItemOnTop :: CanvasItem w => w -> IO ()
putItemOnTop item = execMethod item (\nm -> tkRaiseItem nm Nothing)

-- | Puts an items at bottom of the display list.
putItemAtBottom :: CanvasItem ci => ci -> IO ()
putItemAtBottom item = execMethod item (\nm -> tkLowerItem nm Nothing)


-- -----------------------------------------------------------------------
-- utility 
-- -----------------------------------------------------------------------

-- | Raises an exception if two given items do not have the same parent
-- canvas.
onSameCanvas :: (CanvasItem i1,CanvasItem i2) => i1 -> i2 -> IO ()
onSameCanvas i1 i2 =
  do
    c1 <- getParentObjectID (toGUIObject i1)
    c2 <- getParentObjectID (toGUIObject i2)
    unless (c1 == c2) (raise itemsNotOnSameCanvas)

-- | Exception raised by @CanasItem.onSameCanvas@.
itemsNotOnSameCanvas :: IOError
itemsNotOnSameCanvas = 
  userError "the two canvas items are not on the same canvas"


-- -----------------------------------------------------------------------
-- unparsing of commands
-- -----------------------------------------------------------------------

tkMoveItem :: ObjectName -> Distance -> Distance -> TclScript
tkMoveItem (CanvasItemName nm item) x y =
  [declVar item, 
   show nm ++ " move " ++ show item ++ " " ++ show x ++ " " ++ show y {-,
   show nm ++ " coords " ++ show item-}]
tkMoveItem _ _ _ = []
{-# INLINE tkMoveItem #-}

tkScaleItem :: ObjectName -> Distance -> Distance -> Double -> Double ->
               TclScript
tkScaleItem (CanvasItemName nm item) x y xs ys =
  [declVar item,
   show nm ++ " scale " ++ show item ++ " " ++
   show x ++ " " ++ show y ++ " " ++
   show xs ++ " " ++ show ys,
   show nm ++ " coords " ++ show item]
tkScaleItem _ _ _ _ _ = []
{-# INLINE tkScaleItem #-}

tkCoordItem :: ObjectName -> Coord -> TclScript
tkCoordItem (CanvasItemName nm item) co =
  [declVar item,
   show nm ++ " coords " ++ show item ++ " " ++ show (toGUIValue co)]
tkCoordItem _ _ = []
{-# INLINE tkCoordItem #-}

tkGetCoordItem :: ObjectName -> TclScript
tkGetCoordItem (CanvasItemName nm item) =
  [declVar item,   
   show nm ++ " coords " ++ show item]
tkGetCoordItem _ = []

tkRaiseItem :: ObjectName -> Maybe ObjectName -> TclScript
tkRaiseItem (CanvasItemName nm item) Nothing =
  [declVar item, show nm ++ " raise " ++ show item]
tkRaiseItem (CanvasItemName nm item1) (Just (CanvasItemName _ item2)) =
  [declVar item1, declVar item2,    
   show nm ++ " raise " ++ show item1 ++ " " ++ show item2]
tkRaiseItem _ _ = []
{-# INLINE tkRaiseItem #-}

tkLowerItem :: ObjectName -> Maybe ObjectName -> TclScript
tkLowerItem (CanvasItemName nm item) Nothing  =[
        declVar item,show nm ++ " lower " ++ show item]
tkLowerItem (CanvasItemName nm item1) (Just (CanvasItemName _ item2)) =
  [declVar item1, declVar item2,  
   show nm ++ " lower " ++ show item1 ++ " " ++ show item2]
tkLowerItem _ _  = []
{-# INLINE tkLowerItem #-}

declVar tid = "global " ++ (drop 1 (show tid))
