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

newtype EmbeddedCanvasWin = EmbeddedCanvasWin GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

createEmbeddedCanvasWin :: Widget w => Canvas -> w ->
                                       [Config EmbeddedCanvasWin] ->
                                       IO EmbeddedCanvasWin 
createEmbeddedCanvasWin cnv w ol =
  do
    cit <- createCanvasItem cnv EMBEDDEDCANVASWIN EmbeddedCanvasWin ol
                            [(-1,-1)]
    sub_nm <- getObjectName (toGUIObject w)
    CanvasItemName nm tid <- getObjectName (toGUIObject cit)
    execTclScript ["global " ++ (drop 1 (show tid)),
                   show nm ++ " itemconfigure " ++ show tid ++
                   " -window " ++ show sub_nm]
    return cit


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance GUIObject EmbeddedCanvasWin where 
  toGUIObject (EmbeddedCanvasWin w) = w
  cname _         = "EmbeddedCanvasWin"

instance Destroyable EmbeddedCanvasWin where
  destroy   = destroy . toGUIObject

instance CanvasItem EmbeddedCanvasWin

instance TaggedCanvasItem EmbeddedCanvasWin

instance HasPosition EmbeddedCanvasWin where
  position = itemPositionD2
  getPosition  = getItemPositionD2

instance HasSize EmbeddedCanvasWin

instance Widget EmbeddedCanvasWin where
  cursor s w      = return w
  getCursor w     = return cdefault
  takeFocus b w   = return w
  getTakeFocus w  = return cdefault

instance Synchronized EmbeddedCanvasWin where
  synchronize w = synchronize (toGUIObject w)

instance HasCanvAnchor EmbeddedCanvasWin where
  canvAnchor a w = cset w "anchor" a
  getCanvAnchor w = cget w "anchor"
