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
-- class CanvasItem
-- -----------------------------------------------------------------------

class HasCoords w where
  coord           :: Coord -> Config w
  getCoord        :: w -> IO Coord

class GUIObject w => CanvasItem w

instance CanvasItem w => HasCoords w where
  coord co item =
    do
      try (execMethod item (\nm -> tkCoordItem nm co))
      return item
  getCoord item =  evalMethod item (\nm -> tkGetCoordItem nm)

class CanvasItem w => FilledCanvasItem w where 
  filling         :: ColourDesignator c => c -> Config w
  getFilling      :: w -> IO Colour
  outline         :: ColourDesignator c => c -> Config w
  getOutline      :: w -> IO Colour
  stipple         :: BitMapHandle -> Config w
  getStipple      :: w -> IO BitMapHandle
  outlinewidth    :: Distance -> Config w
  getOutlineWidth :: w -> IO Distance
  filling c w      = cset w "fill" (toColour c)
  getFilling w     = cget w "fill"
  outline c w      = cset w "outline" (toColour c)
  getOutline w     = cget w "outline"
  stipple b w      = setBitMapHandle w "stipple" b True
  getStipple w     = getBitMapHandle w "stipple"
  outlinewidth b w = cset w "width" b
  getOutlineWidth w = cget w "width"


class CanvasItem w => SegmentedCanvasItem w where
  splinesteps     :: Int -> Config w
  getSplinesteps  :: w -> IO Int
  smooth          :: Bool -> Config w
  getSmooth       :: w -> IO Bool
  splinesteps c w  = cset w "splinesteps" c
  getSplinesteps w = cget w "splinesteps"
  smooth c w       = cset w "smooth" c
  getSmooth w      = cget w "smooth"


-- -----------------------------------------------------------------------
-- canvas item operations
-- -----------------------------------------------------------------------

moveItem :: (Synchronized w, CanvasItem w) =>
            w -> Distance -> Distance -> IO ()
moveItem item x y =
  synchronize item (execMethod item (\nm -> tkMoveItem nm x y))

scaleItem :: (Synchronized w, CanvasItem w) =>
             w -> Distance -> Distance -> Double -> Double -> IO ()
scaleItem item x y xs ys =
  synchronize item (execMethod item (\nm -> tkScaleItem nm x y xs ys))


-- -----------------------------------------------------------------------
-- layering operations
-- -----------------------------------------------------------------------

raiseItem :: (CanvasItem ci,CanvasItem w) => ci -> w -> IO ()
raiseItem item1 item2 =
  do
    onSameCanvas item1 item2
    nm2 <- getObjectName (toGUIObject item2)
    execMethod item1 (\nm1 -> tkRaiseItem nm1 (Just nm2))

lowerItem :: (CanvasItem ci,CanvasItem w) => ci -> w -> IO ()
lowerItem item1 item2 =
  do
    onSameCanvas item1 item2
    nm2 <- getObjectName (toGUIObject item2)
    execMethod item1 (\nm1 -> tkLowerItem nm1 (Just nm2))

putItemOnTop :: CanvasItem w => w -> IO ()
putItemOnTop item = execMethod item (\nm -> tkRaiseItem nm Nothing)

putItemAtBottom :: CanvasItem ci => ci -> IO ()
putItemAtBottom item = execMethod item (\nm -> tkLowerItem nm Nothing)


-- -----------------------------------------------------------------------
-- utility 
-- -----------------------------------------------------------------------

onSameCanvas :: (CanvasItem i1,CanvasItem i2) => i1 -> i2 -> IO ()
onSameCanvas i1 i2 =
  do
    c1 <- getParentObjectID (toGUIObject i1)
    c2 <- getParentObjectID (toGUIObject i2)
    unless (c1 == c2) (raise itemsNotOnSameCanvas)

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
