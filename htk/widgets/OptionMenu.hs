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

module OptionMenu (

  OptionMenu,
  newOptionMenu

) where

import Core
import BaseClasses(Widget)
import Configuration
import GUIValue
import Button
import MenuItem
import Destructible
import Computation
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

newtype OptionMenu a = OptionMenu GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

newOptionMenu :: (Container par, GUIValue a) =>
                 par -> [a] -> [Config (OptionMenu a)] ->
                 IO (OptionMenu a)
newOptionMenu par el confs =
  do
    wid <- createGUIObject (toGUIObject par) (OPTIONMENU el')
                           optionMenuMethods
    configure (OptionMenu wid) confs
  where el' = map toGUIValue el


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance GUIObject (OptionMenu a) where 
  toGUIObject (OptionMenu  w) = w
  cname _ = "OptionMenu"

instance Destroyable (OptionMenu a) where
  destroy   = destroy . toGUIObject
 
instance Widget (OptionMenu a)

instance HasBorder (OptionMenu a)

instance HasColour (OptionMenu a) where
  legalColourID = buttonColours

instance HasEnable (OptionMenu a)

instance HasFont (OptionMenu a) 

instance HasSize (OptionMenu a)

instance GUIValue a => HasValue (OptionMenu a) a where
  value v w =
    setTclVariable ((tvarname . objectID . toGUIObject) w) v >> return w
  getValue w = getTclVariable ((tvarname . objectID . toGUIObject) w)

---
-- An option menu can have a tooltip.
instance HasTooltip (OptionMenu a)

---
-- An option menu has an anchor.
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
