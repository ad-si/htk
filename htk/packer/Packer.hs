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
  grid

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

grid :: Widget w => w -> [GridPackOption] -> IO ()
grid w gridpackopts =
  do
    let (GUIOBJECT _ ostref) = toGUIObject w
    ost <- getRef ostref
    meth <- withRef ostref methods
    execTclScript ((gridCmd meth) (objectname ost) gridpackopts)


-- -----------------------------------------------------------------------
-- standard packer
-- -----------------------------------------------------------------------

pack :: Widget w => w -> [PackOption] -> IO ()
pack w packopts =
  do
    let obj = toGUIObject w
    meth <- getMethods obj
    nm <- getObjectName obj
    pobj' <- getParentObject w
    case pobj' of
      Nothing -> execTclScript ((packCmd meth) nm packopts)
      Just pobj ->
        do
          kind <- getObjectKind pobj
          case kind of
            BOX Vertical Rigid ->
              execTclScript ((packCmd meth) nm (packopts ++ [Side AtTop]))
            BOX Horizontal Rigid ->
              execTclScript ((packCmd meth) nm (packopts ++
                                                [Side AtLeft]))
            BOX Vertical Flexible ->
              execTclScript ((packCmd meth) nm (packopts ++
                                                [Side AtTop, Fill Both,
                                                 Expand On]))
            BOX Horizontal Flexible ->
              execTclScript ((packCmd meth) nm (packopts ++
                                                [Side AtLeft, Fill Both,
                                                 Expand On]))
            _ -> execTclScript ((packCmd meth) nm packopts)
