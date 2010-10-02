-- | A container widget with a preset packing orientation (for simple
-- packing).
module HTk.Containers.Box (

  Box,

  newBox,
  newHBox,
  newVBox,
  newHFBox,
  newVFBox

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import Events.Destructible
import Util.Computation
import Events.Synchronized
import HTk.Kernel.Packer


-- -----------------------------------------------------------------------
-- horizontal/vertical box
-- -----------------------------------------------------------------------

-- | The @Box@ datatype.
data Box = Box GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

-- | Constructs a new box and returns a handler.
newBox :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> Flexibility
   -- ^ the flexibility of the box.
   -> [Config Box]
   -- ^ the list of configuration options for this box.
   -> IO Box
   -- ^ A box.
newBox par fl cnf =
  do
    w <- createWidget (toGUIObject par) (BOX cdefault fl)
    configure (Box  w) cnf

-- | Constructs a new box with horizontal packing order and rigid
-- flexibility and returns a handler.
newHBox :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Box]
   -- ^ the list of configuration options for this box.
   -> IO Box
   -- ^ A box.
newHBox par cnf = newBox par Rigid ((orient Horizontal) : cnf)

-- | Constructs a new box with vertical packing order and rigid
-- flexibility and returns a handler.
newVBox :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Box]
   -- ^ the list of configuration options for this box.
   -> IO Box
   -- ^ A box.
newVBox par cnf = newBox par Rigid ((orient Vertical) : cnf)

-- | Constructs a new flexible box with horizontal packing order and returns
-- a handler.
newHFBox :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Box]
   -- ^ the list of configuration options for this box.
   -> IO Box
   -- ^ A box.
newHFBox par cnf = newBox par Flexible ((orient Horizontal) : cnf)

-- | Constructs a new flexible box with vertical packing order and returns
-- a handler.
newVFBox :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Box]
   -- ^ the list of configuration options for this box.
   -> IO Box
   -- ^ A box.
newVFBox par cnf = newBox par Flexible ((orient Vertical) : cnf)


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq Box where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject Box where
  toGUIObject (Box w) = toGUIObject w
  cname _ = "Box"

-- | A box can be destroyed.
instance Destroyable Box where
  --  Destroys a box.
  destroy = destroy . toGUIObject

-- | You can synchronize on a box object.
instance Synchronized Box where
  --  Synchronizes on a box object.
  synchronize = synchronize . toGUIObject

-- | A box has standard widget properties
-- (concerning focus, cursor).
instance Widget Box

-- | A box is a container for widgets. You can pack widgets to
-- a box via pack or grid command in the @module Packer@.
instance Container Box

-- | A box has a configureable border.
instance HasBorder Box

-- | A box has a configureable background colour.
instance HasColour Box where
  legalColourID = hasBackGroundColour

-- | A box\'es packing orientation is configureable.
instance HasOrientation Box where
  --  Sets the box\'es packing orientation.
  orient or box@(Box w) =
    do
      BOX or' fl <- getObjectKind w
      setObjectKind w (BOX or fl)
      return box
  --  Gets the box\'es packing orientation.
  getOrient (Box w) =
    do
      BOX or _ <- getObjectKind w
      return or

-- | You can specify the size of a box.
instance HasSize Box
