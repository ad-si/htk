{-# LANGUAGE FlexibleInstances #-}

-- | This module provides access to bitmap resources.
module HTk.Components.BitMap (

  BitMap,
  newBitMap,

  BitMapHandle(..),
  HasBitMap(..),
  BitMapDesignator(..),

  errmap,
  gray50,
  gray25,
  hourglass,
  info,
  questhead,
  question,
  warning,

  setBitMapHandle,
  getBitMapHandle,
  stringToBitMapHandle

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import Data.Char(isDigit)
import Util.Computation
import Events.Synchronized
import Events.Destructible

-- -----------------------------------------------------------------------
-- BitMap designators
-- -----------------------------------------------------------------------

-- | The @BitMapHandle@ datatype - a handle for a bitmap
-- resource.
data BitMapHandle =
          Predefined String
        | BitMapHandle BitMap
        | BitMapFile String

-- | Internal.
class BitMapDesignator d where
  toBitMap :: d -> BitMapHandle

-- | Internal.
instance BitMapDesignator BitMapHandle where
  toBitMap = id

-- | Internal.
instance BitMapDesignator BitMap where
  toBitMap h = BitMapHandle h

-- | A string is a handle for a bitmap file.
instance BitMapDesignator [Char] where
  toBitMap h = BitMapFile h


-- -----------------------------------------------------------------------
-- BitMap'ed widgets
-- -----------------------------------------------------------------------

-- | Containers for bitmaps instantiate the @class HasBitMap@.
class GUIObject w => HasBitMap w where
  bitmap          :: BitMapDesignator d => d -> Config w
  getBitMap       :: w -> IO BitMapHandle
  bitmap d w      = setBitMapHandle w "bitmap" (toBitMap d) True
  getBitMap w     = getBitMapHandle w "bitmap"


-- -----------------------------------------------------------------------
-- type BitMap
-- -----------------------------------------------------------------------

-- | The @BitMap@ datatype.
newtype BitMap = BitMapWDG GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new bitmap object and returns a handler.
-- The bitmap object can be packed like a widget, then it is implicitely
-- displayed inside a label widget.
newBitMap :: [Config BitMap]
   -- ^ the list of configuration options for this bitmap  object.
   -> IO BitMap
   -- ^ A bitmap object.
newBitMap confs =
  do
    w <- createWidget ROOT LABEL
    configure (BitMapWDG w) confs


-- -----------------------------------------------------------------------
-- predefined Tk BitMaps
-- -----------------------------------------------------------------------

-- | A handle for the predefined \"error\" bitmap.
errmap :: BitMapHandle
errmap = Predefined "error"

-- | A handle for the predefined \"gray50\" bitmap.
gray50 :: BitMapHandle
gray50 = Predefined "gray50"

-- | A handle for the predefined \"gray25\" bitmap.
gray25 :: BitMapHandle
gray25 = Predefined "gray25"

-- | A handle for the predefined \"hourglass\" bitmap.
hourglass :: BitMapHandle
hourglass = Predefined "hourglass"

-- | A handle for the predefined \"info\" bitmap.
info :: BitMapHandle
info = Predefined "info"

-- | A handle for the predefined \"questhead\" bitmap.
questhead :: BitMapHandle
questhead = Predefined "questhead"

-- | A handle for the predefined \"question\" bitmap.
question :: BitMapHandle
question = Predefined "question"

-- | A handle for the predefined \"warning\" bitmap.
warning :: BitMapHandle
warning = Predefined "warning"


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject BitMap where
  toGUIObject (BitMapWDG w) = w
  cname _ = "BitMap"

-- | A bitmap object can be destroyed.
instance Destroyable BitMap where
  -- Destroys a bitmap object.
  destroy = destroy . toGUIObject

-- | A bitmap object has standard widget properties
-- (concerning focus, cursor \/ if implicitely displayed inside a label
-- widget).
instance Widget BitMap

-- | A bitmap object has a configureable border (if implicitely displayed
-- inside a label widget).
instance HasBorder BitMap

-- | A bitmap object has a configureable foreground and background colour
-- (if implicitely displayed inside a label widget).
instance HasColour BitMap where
  legalColourID = hasForeGroundColour

-- | You can specify the size of the containing label, if the bitmap is
-- implicitely displayed inside a label widget.
instance HasSize BitMap

-- | Bitmaps can be read from files.
instance HasFile BitMap where
  -- Specifies the bitmap\'s file path.
  filename fname w =
    execTclScript [tkBitMapCreate no fname] >> cset w "image" no
    where no = getObjectNo (toGUIObject w)
  -- Gets the bitmap\'s file name.
  getFileName w = evalTclScript [tkGetBitMapFile no]
    where no = getObjectNo (toGUIObject w)

-- | You can synchronize on a bitmap object.
instance Synchronized BitMap where
  -- Synchronizes on a bitmap object.
  synchronize (BitMapWDG w) = synchronize w


-- -----------------------------------------------------------------------
-- auxiliary functions
-- -----------------------------------------------------------------------

-- | Internal.
setBitMapHandle :: GUIObject w => w -> ConfigID -> BitMapHandle ->
                   Bool -> IO w
setBitMapHandle w cnm (Predefined d) _ = cset w cnm d
setBitMapHandle w cnm (BitMapFile f) _ = cset w cnm ('@':f)
setBitMapHandle w _ (BitMapHandle h) True =
  cset w "image" (getObjectNo (toGUIObject h))
setBitMapHandle w cnm (BitMapHandle h) False =
  do
    fname <- getFileName h
    setBitMapHandle w cnm (BitMapFile fname) False
    return w
{-
   the last parameter determines whether integer numbers are acceptable
   as bitmap denotations or not. If not, we use the corresponding file
   name associated with the widget! Numbers are allowed for labels and
   buttons, but not for windows!
-}

-- | Internal.
getBitMapHandle :: GUIObject w => w -> ConfigID -> IO BitMapHandle
getBitMapHandle w cnm = cget w cnm >>= stringToBitMapHandle

-- | Internal.
stringToBitMapHandle :: String -> IO BitMapHandle
stringToBitMapHandle "" = return (Predefined "")
stringToBitMapHandle ('@':tl) = return (BitMapFile tl)
stringToBitMapHandle (str @ (x:tl)) | isDigit x =
  lookupGUIObject (read str)      >>= return . BitMapHandle . BitMapWDG
stringToBitMapHandle str = return (Predefined str)


-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkBitMapCreate :: Int -> String -> String
tkBitMapCreate no f = "image create bitmap " ++ show no ++ " -file " ++ show f
{-# INLINE tkBitMapCreate #-}

tkGetBitMapFile :: Int -> String
tkGetBitMapFile no = (show no) ++ " cget -file "
{-# INLINE tkGetBitMapFile #-}
