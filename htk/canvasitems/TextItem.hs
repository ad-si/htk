{- #######################################################################

MODULE        : TextItem
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Text items

   #################################################################### -}


module TextItem (

  module CanvasItem,

  TextItem,
  createTextItem

) where

import Core
import Configuration
import CanvasItem
import CanvasTag
import CanvasItemAux
import Computation
import Destructible
import Synchronized


-- -----------------------------------------------------------------------
-- TextItem
-- -----------------------------------------------------------------------

newtype TextItem = TextItem GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

createTextItem :: Canvas -> [Config TextItem] -> IO TextItem
createTextItem cnv ol = createCanvasItem cnv TEXTITEM TextItem ol [(0,0)]


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIObject TextItem where 
  toGUIObject (TextItem w) = w
  cname _ = "TextItem"

instance Destroyable TextItem where
  destroy   = destroy . toGUIObject

instance CanvasItem TextItem

instance FilledCanvasItem TextItem where
  outline c w  = return w
  getOutline w = return cdefault

instance TaggedCanvasItem TextItem

instance HasPosition TextItem where
  position     = itemPositionD2
  getPosition  = getItemPositionD2

instance HasSize TextItem where
  height _ w  = return w
  getHeight _ = return 1

instance HasJustify TextItem

instance HasFont TextItem

instance HasCanvAnchor TextItem where
  canvAnchor a w = cset w "anchor" a
  getCanvAnchor w = cget w "anchor"

instance Synchronized TextItem where
  synchronize = synchronize . toGUIObject

instance GUIValue b => HasText TextItem b where
  text t w   = cset w "text" t
  getText w  = cget w "text"