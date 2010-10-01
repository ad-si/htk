{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /option menu/ widget.
-- A simple clip up menu displaying a set of radiobuttons.
module HTk.Widgets.OptionMenu (

  OptionMenu,
  newOptionMenu

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Menuitems.MenuItem
import Events.Destructible
import Util.Computation
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @OptionMenu@ datatype.
newtype OptionMenu a = OptionMenu GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new option menu and returns a handler.
newOptionMenu :: (Container par, GUIValue a) =>
   par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [a]
   -- ^ the list of selectable elements.
   -> [Config (OptionMenu a)]
   ->
   IO (OptionMenu a)
   -- ^ An option menu.
newOptionMenu par el cnf =
  do
    wid <- createGUIObject (toGUIObject par) (OPTIONMENU el')
                           optionMenuMethods
    configure (OptionMenu wid) cnf
  where el' = map toGUIValue el


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject (OptionMenu a) where
  toGUIObject (OptionMenu  w) = w
  cname _ = "OptionMenu"

-- | An option menu can be destroyed.
instance Destroyable (OptionMenu a) where
  -- Destroys an option menu.
  destroy = destroy . toGUIObject

-- | An option menu has standard widget properties
-- (concerning focus, cursor).
instance Widget (OptionMenu a)

-- | An option menu has a configureable border.
instance HasBorder (OptionMenu a)

-- | An option menu has a normal foreground and background colour and an
-- active\/disabled foreground and background colour.
instance HasColour (OptionMenu a) where
  legalColourID = buttonColours

-- | An option menu is a stateful widget, it can be enabled or disabled.
instance HasEnable (OptionMenu a)

-- | You can specify the font of an option menu.
instance HasFont (OptionMenu a)

-- | You can specify the size of an option menu.
instance HasSize (OptionMenu a)

-- | An option menu has a value (the selected element), that corresponds to
-- a polymorphic @TkVariable@.
instance GUIValue a => HasValue (OptionMenu a) a where
  -- Sets the option menu\'s value (the selected element).
  value v w =
    setTclVariable ((tvarname . objectID . toGUIObject) w) v >> return w
  -- Gets the option menu\'s value.
  getValue w = getTclVariable ((tvarname . objectID . toGUIObject) w)

-- | An option menu can have a tooltip (only displayed if you are using
-- tixwish).
instance HasTooltip (OptionMenu a)

-- | An option menu has a text anchor.
instance HasAnchor (OptionMenu a)


-- -----------------------------------------------------------------------
-- OptionMenu methods
-- -----------------------------------------------------------------------

optionMenuMethods = defMethods { cleanupCmd = tkCleanupOptionMenu,
                                 createCmd = tkCreateOptionMenu,
                                 csetCmd = tkSetOptionMenuConfigs }


-- -----------------------------------------------------------------------
-- Unparsing of Tk commands
-- -----------------------------------------------------------------------

tvarname :: ObjectID -> String
tvarname oid = "v" ++ (show oid)

tkDeclOptionMenuVar :: GUIOBJECT -> WidgetName
tkDeclOptionMenuVar = WidgetName . tvarname . objectID

tkCreateOptionMenu :: ObjectName -> ObjectKind -> ObjectName ->
                      ObjectID -> [ConfigOption] -> TclScript
tkCreateOptionMenu _ (OPTIONMENU els) name oid confs =
  ["set " ++ tvarname oid ++ " " ++ firstElem els,
   "tk_optionMenu " ++ show name ++ " " ++ tvarname oid ++ " " ++
   concatMap (++ " ") (map show els)] ++
  tkSetOptionMenuConfigs name confs
  where firstElem [] = ""
        firstElem ((GUIVALUE _ x):l) = x

tkSetOptionMenuConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetOptionMenuConfigs name @ (ObjectName wname) confs =
  (csetCmd defMethods) name confs ++
  (csetCmd defMethods) (ObjectName (wname ++ ".menu"))
                       (filter isMenuConfig confs)
  where isMenuConfig ("foreground",_) = True
        isMenuConfig ("background",_) = True
        isMenuConfig ("activebackground",_) = True
        isMenuConfig ("activeforeground",_) = True
        isMenuConfig ("disabledforeground",_) = True
        isMenuConfig ("font",_) = True
        isMenuConfig (_,_) = False

tkCleanupOptionMenu :: ObjectID -> ObjectName -> TclScript
tkCleanupOptionMenu oid _ = tkUndeclVar (tvarname oid)
{-# INLINE tkCleanupOptionMenu #-}
