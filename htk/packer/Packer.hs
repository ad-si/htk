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
-- Packing of widgets - HTk supports Tk's standard packer and grid packer.
module Packer (

  Container,

  --standard packer
  pack,

  --grid packer
  grid,

  AbstractWidget(..)

) where

import GUIObject
import GUIObjectKind
import Resources
import BaseClasses(Widget)
import ReferenceVariables
import PackOptions
import GridPackOptions
import Wish
import Core


-- -----------------------------------------------------------------------
-- abstract class Container
-- -----------------------------------------------------------------------

---
-- Container widgets instantiate the abstract <code>class Container</code>
-- to enable packing.
class GUIObject a => Container a


-- -----------------------------------------------------------------------
-- grid packer
-- -----------------------------------------------------------------------

---
-- Packs a widget via the grid geometry manager.
-- @param wid     - the widget to pack.
-- @param opts    - the grid pack options.
-- @return result - None.
grid :: Widget w => w -> [GridPackOption] -> IO ()
grid wid opts =
  do
    let (GUIOBJECT _ ostref) = toGUIObject wid
    ost <- getRef ostref
    meth <- withRef ostref methods
    execTclScript ((gridCmd meth) (objectname ost) opts)


-- -----------------------------------------------------------------------
-- standard packer
-- -----------------------------------------------------------------------

---
-- Packs a widget via the pack geometry manager.
-- @param wid     - the widget to pack.
-- @param opts    - the pack options.
-- @return result - None.
pack :: Widget w => w -> [PackOption] -> IO ()
pack wid opts =
  do
    let obj = toGUIObject wid
    meth <- getMethods obj
    nm <- getObjectName obj
    pobj' <- getParentObject wid
    case pobj' of
      Nothing -> execTclScript ((packCmd meth) nm opts)
      Just pobj ->
        do
          kind <- getObjectKind pobj
          case kind of
            BOX Vertical Rigid ->
              execTclScript ((packCmd meth) nm (opts ++ [Side AtTop]))
            BOX Horizontal Rigid ->
              execTclScript ((packCmd meth) nm (opts ++
                                                [Side AtLeft]))
            BOX Vertical Flexible ->
              execTclScript ((packCmd meth) nm (opts ++
                                                [Side AtTop, Fill Both,
                                                 Expand On]))
            BOX Horizontal Flexible ->
              execTclScript ((packCmd meth) nm (opts ++
                                                [Side AtLeft, Fill Both,
                                                 Expand On]))
            _ -> execTclScript ((packCmd meth) nm opts)


data AbstractWidget = NONE
instance GUIObject AbstractWidget where
  toGUIObject _ = ROOT
instance Container AbstractWidget
