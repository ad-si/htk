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

---
-- HTk's <strong>embedded canvas windows</strong>.<br>
-- A container for widgets on a canvas widget.
module EmbeddedCanvasWin (

  module CanvasItem,

  EmbeddedCanvasWin,
  createEmbeddedCanvasWin 

) where

import Core
import BaseClasses
import Configuration
import CanvasItem
import CanvasTag
import CanvasItemAux
import Computation
import Synchronized
import Destructible
import ReferenceVariables


-- -----------------------------------------------------------------------
-- embedded window
-- -----------------------------------------------------------------------

---
-- The <code>EmbeddedCanvasWin</code> datatype.
newtype EmbeddedCanvasWin = EmbeddedCanvasWin GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

---
-- Constructs a new embedded canvas window.
-- @param cnv     - the parent canvas.
-- @param wid     - the child widget.
-- @param cnf     - the list of configuration options for this embedded
--                  canvas window.
-- @return result - An embedded canvas window.
createEmbeddedCanvasWin :: Widget w => Canvas -> w ->
                                       [Config EmbeddedCanvasWin] ->
                                       IO EmbeddedCanvasWin 
createEmbeddedCanvasWin cnv wid cnf =
  do
    cit <- createCanvasItem cnv EMBEDDEDCANVASWIN EmbeddedCanvasWin cnf
                            [(-1,-1)]
    sub_nm <- getObjectName (toGUIObject wid)
    CanvasItemName nm tid <- getObjectName (toGUIObject cit)
    execTclScript ["global " ++ (drop 1 (show tid)),
                   show nm ++ " itemconfigure " ++ show tid ++
                   " -window " ++ show sub_nm]
    return cit


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject EmbeddedCanvasWin where 
---
-- Internal.
  toGUIObject (EmbeddedCanvasWin w) = w
---
-- Internal.
  cname _         = "EmbeddedCanvasWin"

---
-- An embedded canvas window can be destroyed.
instance Destroyable EmbeddedCanvasWin where
---
-- Destroys an embedded canvas window.
  destroy = destroy . toGUIObject

---
-- An embedded canvas window is a canvas item (any canvas item is an
-- instance of the abstract <code>class CanvasItem</code>).
instance CanvasItem EmbeddedCanvasWin

---
-- An embedded canvas window can have several tags (handlers for a set of
-- canvas items).
instance TaggedCanvasItem EmbeddedCanvasWin

---
-- You can specify the position of a bitmap item.
instance HasPosition EmbeddedCanvasWin where
---
-- Sets the position of the embedded canvas window.
  position = itemPositionD2
---
-- Gets the position of the embedded canvas window.
  getPosition  = getItemPositionD2

---
-- You can specify the size of an embedded canvas window.
instance HasSize EmbeddedCanvasWin

---
-- Dummy instance.
instance Widget EmbeddedCanvasWin where
  cursor s w      = return w
  getCursor w     = return cdefault
  takeFocus b w   = return w
  getTakeFocus w  = return cdefault

---
-- You can synchronize on an embedded canvas window.
instance Synchronized EmbeddedCanvasWin where
---
-- Synchronizes on an embedded canvas window.
  synchronize = synchronize . toGUIObject

---
-- You can specify the anchor position of an embedded canvas window.
instance HasCanvAnchor EmbeddedCanvasWin where
---
-- Sets the anchor position of an embedded canvas window.
  canvAnchor a w = cset w "anchor" a
---
-- Gets the anchor position of an embedded canvas window.
  getCanvAnchor w = cget w "anchor"
