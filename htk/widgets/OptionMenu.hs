{- #########################################################################

MODULE        : OptionMenu
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : OptionMenu Widget

TO BE DONE    : should be an instance of class Reactive so that 
                one can track changes to the value of the option menu,
                that are due to the user and not the application (we
                got valueSet for the latter).


   ######################################################################### -}


module OptionMenu (
        OptionMenu,
        newOptionMenu

        ) where

import Concurrency
import GUICore
import GUIValue
import Packer
import Button
import MenuItem
import Debug(debug)


-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

newtype OptionMenu a = OptionMenu GUIOBJECT deriving Eq


-- --------------------------------------------------------------------------
-- Creation
-- --------------------------------------------------------------------------

newOptionMenu :: GUIValue a => [a] -> [Config (OptionMenu a)] -> IO (OptionMenu a)
newOptionMenu el confs = do {
        wid <- createGUIObject (OPTIONMENU el') optionMenuMethods;
        configure (OptionMenu wid) confs;
}       where el' = map toGUIValue el


-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance GUIObject (OptionMenu a) where 
        toGUIObject (OptionMenu  w) = w
        cname _ = "OptionMenu"

instance Destructible (OptionMenu a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (OptionMenu a)
 
instance Widget (OptionMenu a)

instance ChildWidget (OptionMenu a)

instance HasBorder (OptionMenu a)

instance HasColour (OptionMenu a) where
        legalColourID = buttonColours

instance HasEnable (OptionMenu a)

instance HasFont (OptionMenu a) 

instance HasSize (OptionMenu a)

instance GUIValue a => Variable OptionMenu a where
        setVar w v      = synchronize w (
                setTclVariable ((tvarname . objectID . toGUIObject) w) v)
        getVar w        = synchronize w (
                getTclVariable ((tvarname . objectID . toGUIObject) w) )
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })


instance Synchronized (OptionMenu a) where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
-- OptionMenu Methods
-- --------------------------------------------------------------------------

optionMenuMethods = defMethods {
                cleanupCmd = tkCleanupOptionMenu,
                createCmd = tkCreateOptionMenu,
                csetCmd = tkSetOptionMenuConfigs
                }


-- --------------------------------------------------------------------------
-- Unparsing of Tk Commands
-- --------------------------------------------------------------------------

tvarname :: ObjectID -> String
tvarname oid = "v" ++ (show oid)


tkDeclOptionMenuVar :: GUIOBJECT -> WidgetName
tkDeclOptionMenuVar = WidgetName . tvarname . objectID 
        

tkCreateOptionMenu :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> 
                        TclScript
tkCreateOptionMenu (OPTIONMENU els) name oid confs = [
        "set " ++ tvarname oid ++ " " ++ firstElem els,
        "tk_optionMenu " ++ show name ++ " " ++ tvarname oid ++ " " ++
          (concatMap (++ " ") (map show els))
        ] ++
        tkSetOptionMenuConfigs name confs
        where firstElem [] = ""
              firstElem ((GUIVALUE _ x):l) = x


tkSetOptionMenuConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetOptionMenuConfigs name @ (ObjectName wname) confs = 
        (csetCmd defMethods) name confs ++
        (csetCmd defMethods) (ObjectName (wname ++ ".menu")) 
                                (filter isMenuConfig confs)
        where   isMenuConfig ("foreground",_) = True
                isMenuConfig ("background",_) = True
                isMenuConfig ("activebackground",_) = True
                isMenuConfig ("activeforeground",_) = True
                isMenuConfig ("disabledforeground",_) = True
                isMenuConfig ("font",_) = True
                isMenuConfig (_,_) = False
                

tkCleanupOptionMenu :: ObjectID -> ObjectName -> TclScript
tkCleanupOptionMenu oid _ = tkUndeclVar (tvarname oid) 
{-# INLINE tkCleanupOptionMenu #-}

