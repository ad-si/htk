-- | HTk\'s /ComboBox/.
-- Only available when using tixwish.
module ComboBox (

  ComboBox,
  newComboBox,

  pick

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Computation
import Synchronized
import Destructible
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @ComboBox@ datatype.
newtype GUIValue a => ComboBox a = ComboBox GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- combo box creation
-- -----------------------------------------------------------------------

-- | Constructs a new combo box and returns a handler.
newComboBox :: (GUIValue a, Container par) =>
   par 
   -- ^ the list of configuration options for this
   -- combo box.
   -> Bool 
   -- ^ true if the user should be allowed to type into the
   -- entry of the ComboBox. 
   -> [Config (ComboBox a)] 
   -> IO (ComboBox a)
   -- ^ A combo box.
newComboBox par editable cnf =
  do
    w <- createGUIObject (toGUIObject par) (COMBOBOX editable)
                         comboBoxMethods
    configure (ComboBox w) cnf


-- -----------------------------------------------------------------------
-- combo box specific commands and options
-- -----------------------------------------------------------------------

-- | Sets the index item in the listbox to be the current value of the
-- ComboBox.
pick :: GUIValue a => Int -> Config (ComboBox a)
pick i cb = execMethod cb (\nm -> tkPick nm i) >> return cb


-- -----------------------------------------------------------------------
-- combo box methods
-- -----------------------------------------------------------------------

comboBoxMethods :: Methods
comboBoxMethods = Methods (cgetCmd defMethods)
                          (csetCmd defMethods)
                          tkCreateComboBox
                          (packCmd defMethods)
                          (gridCmd defMethods)
                          (destroyCmd defMethods)
                          (bindCmd defMethods)
                          (unbindCmd defMethods)
                          (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- combo box instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject (ComboBox a) where 
  toGUIObject (ComboBox f) = f
  cname _ = "ComboBox"

-- | The value of a combo box is the list of the displayed objects (these
-- are instances of class @GUIValue@ and therefore instances
-- of class @Show@).
instance (GUIValue a, GUIValue [a]) => HasValue (ComboBox a) [a] where
  value vals w =
    execMethod w (\nm -> tkInsert nm 0 (map toGUIValue vals)) >> return w
  -- Gets the list of displayed objects.
  getValue w = evalMethod w (\nm -> tkGet nm)

-- | A combo box has standard widget properties (focus, cursor, ...).
instance Widget (ComboBox a)

-- | A combo box widget can be destroyed.
instance Destroyable (ComboBox a) where
  -- Destroys a combo box widget.
  destroy = destroy . toGUIObject

-- | A combo box widget has a configureable border.
instance HasBorder (ComboBox a)

-- | A combo box widget has a text anchor.
instance HasAnchor (ComboBox a)

-- | A combo box widget has a background colour.
instance HasColour (ComboBox a) where 
  legalColourID = hasBackGroundColour

-- | You can specify the size of a combo box widget-
instance HasSize (ComboBox a)

-- | You can synchronize on a combo box widget.
instance Synchronized (ComboBox a) where
  -- Synchronizes on a combo box widget.
  synchronize = synchronize . toGUIObject

-- | A combo box widget is a stateful widget, it can be enabled or disabled.
instance HasEnable (ComboBox a)


-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkCreateComboBox :: ObjectName -> ObjectKind -> ObjectName ->
                    ObjectID -> [ConfigOption] -> TclScript
tkCreateComboBox _ (COMBOBOX editable) name _ opts =
  ["tixComboBox " ++ show name ++ " -editable " ++ show editable ++
   showConfigs opts]
tkCreateComboBox _ _ _ _ _ = []
{-# INLINE tkCreateComboBox #-}

tkInsert ::  ObjectName -> Int -> [GUIVALUE] -> TclScript
tkInsert name inx elems = 
  [tkDelete name "0" "end",
   show name ++ " subwidget listbox insert " ++ show inx ++ " " ++
   showElements elems]
{-# INLINE tkInsert #-}

tkDelete :: ObjectName -> String -> String -> TclCmd
tkDelete name first last =
  show name ++ " subwidget listbox delete " ++ first ++ " " ++ last
{-# INLINE tkDelete #-}

tkGet :: ObjectName -> TclScript
tkGet name = [show name ++ " subwidget entry get"]
{-# INLINE tkGet #-}

showElements :: [GUIVALUE] -> String
showElements = concatMap (++ " ") . (map show) 
{-# INLINE showElements #-}

tkPick :: ObjectName -> Int -> TclScript
tkPick name index = [show name ++ " pick " ++ show index]
{-# INLINE tkPick #-}
