-- | Packing of widgets - HTk supports Tk\'s standard packer and grid packer.
module HTk.Kernel.Packer (

  Container,

  --standard packer
  pack,

  --grid packer
  grid,

  AbstractWidget(..)

) where

import HTk.Kernel.GUIObject
import HTk.Kernel.Resources
import HTk.Kernel.BaseClasses(Widget)
import Reactor.ReferenceVariables
import HTk.Kernel.PackOptions
import HTk.Kernel.GridPackOptions
import HTk.Kernel.Core


-- -----------------------------------------------------------------------
-- abstract class Container
-- -----------------------------------------------------------------------

-- | Container widgets instantiate the abstract @class Container@
-- to enable packing.
class GUIObject a => Container a


-- -----------------------------------------------------------------------
-- grid packer
-- -----------------------------------------------------------------------

-- | Packs a widget via the grid geometry manager.
grid :: Widget w => w
   -- ^ the widget to pack.
   -> [GridPackOption]
   -- ^ the grid pack options.
   -> IO ()
   -- ^ None.
grid wid opts =
  do
    let (GUIOBJECT _ ostref) = toGUIObject wid
    ost <- getRef ostref
    meth <- withRef ostref methods
    execTclScript ((gridCmd meth) (objectname ost) opts)


-- -----------------------------------------------------------------------
-- standard packer
-- -----------------------------------------------------------------------

-- | Packs a widget via the pack geometry manager.
pack :: Widget w => w
   -- ^ the widget to pack.
   -> [PackOption]
   -- ^ the pack options.
   -> IO ()
   -- ^ None.
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
  cname _ = "AbstractWidget"
instance Container AbstractWidget
