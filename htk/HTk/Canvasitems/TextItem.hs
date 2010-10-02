{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /text/ canvas item.
-- A text container on a canvas widget.
module HTk.Canvasitems.TextItem (

  TextItem,
  createTextItem

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Canvasitems.CanvasItem
import HTk.Canvasitems.CanvasTag
import HTk.Canvasitems.CanvasItemAux
import Util.Computation
import Events.Destructible
import Events.Synchronized


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @TextItem@ datatype.
newtype TextItem = TextItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

-- | Constructs a new text item.
createTextItem :: Canvas
   -- ^ the parent canvas.
   -> [Config TextItem]
   -- ^ the list of configuration options for this text item.
   -> IO TextItem
   -- ^ A text item.
createTextItem cnv cnf =
  createCanvasItem cnv TEXTITEM TextItem cnf [(-1,-1)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject TextItem where
  toGUIObject (TextItem w) = w
  cname _ = "TextItem"

-- | A text item can be destroyed.
instance Destroyable TextItem where
  -- Destroys a text item.
  destroy = destroy . toGUIObject

-- | A text item is a canvas item (any canvas item is an instance of the
-- abstract @class CanvasItem@).
instance CanvasItem TextItem

-- | An oval item is a filled canvas item (it has filling, outline width,
-- and stipple configurations).
instance FilledCanvasItem TextItem where
  -- Dummy.
  outline c w  = return w
  -- Dummy.
  getOutline w = return cdefault

-- | A text item can have several tags (handlers for a set of canvas
-- items).
instance TaggedCanvasItem TextItem

-- | You can specify the position of a text item.
instance HasPosition TextItem where
  -- Sets the position of a text item.
  position = itemPositionD2
  -- Gets the position of a text item.
  getPosition = getItemPositionD2

-- | You can specify the width of a text item.
instance HasSize TextItem where
  -- Dummy.
  height _ w = return w
  -- Dummy.
  getHeight _ = return 1

-- | A text item has a configureable text justification.
instance HasJustify TextItem

-- | You can specify the font of a text item.
instance HasFont TextItem

-- | You can specify the anchor position of a text item.
instance HasCanvAnchor TextItem where
  -- Sets the anchor position of a text item.
  canvAnchor a w = cset w "anchor" a
  -- Gets the anchor position of a text item.
  getCanvAnchor w = cget w "anchor"

-- | You can synchronize on a text item.
instance Synchronized TextItem where
  -- Synchronizes on a text item.
  synchronize = synchronize . toGUIObject

-- | A text item is a container for text.
instance GUIValue b => HasText TextItem b where
  -- Sets the displayed text.
  text t w   = cset w "text" t
  -- Gets the displayed text.
  getText w  = cget w "text"

-- | An anchor defines where a text item is placed relative to the
-- given position.
instance HasAnchor TextItem
