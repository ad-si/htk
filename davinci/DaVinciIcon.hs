{- #########################################################################

MODULE        : DaVinciIcon
AUTHOR        : Carla Blanck Purper,  
                Einar W. Karlsen
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : Encapsulation of DaVinci Icons.

                We reuse the IconBar of HTk. IconBar's are therefore
                created as any other iconbar, and finally installed
                as an iconbar of a designated graph by calling the
                iconbar command.


   ######################################################################### -}


module DaVinciIcon (
        module IconBar,

        Graph,

        iconbar
           
        ) where

import SIM
import GUICore
import IconBar
import DaVinciCore
import BitMap

import Debug(debug)




-- ---------------------------------------------------------------------------
-- Install Icon Bar Command
-- ---------------------------------------------------------------------------

iconbar :: IconBar a -> Config Graph
iconbar ib g = do {
        el <- getIconButtons ib;
        it <- sequence (map toIconType (reverse el));
        withGraph g (return (createIcons it));
        withGraph g (return (activateIcons (map iconId it)));
        return g
} where iconId (IconType id _ _) = id
        createIcons :: [IconType] -> String
        createIcons x = "app_menu(create_icons(" ++ show x ++ "))"
        activateIcons :: [String] -> String
        activateIcons ids = ("app_menu(activate_icons("++show ids ++"))")
        

-- ---------------------------------------------------------------------------
-- Aux Commands
-- ---------------------------------------------------------------------------

toIconType :: Button a -> IO IconType
toIconType ie = do {
        bnm <- getBitMapName ie;
        return (IconType (show oid) bnm "")
} where oid = objectID (toGUIObject ie)

getBitMapName :: Button a -> IO String
getBitMapName bm = do {
        bnm <- try (getBitMap bm); 
        case bnm of
                (Right (BitMapFile fnm)) -> return fnm
                _  -> return ""
}


-- ---------------------------------------------------------------------------
-- DaVinci Commands Data Definition
-- ---------------------------------------------------------------------------

data IconType =  
          IconType String String String 
        | Blank


-- ---------------------------------------------------------------------------
-- Unparsing of DaVinci Commands
-- ---------------------------------------------------------------------------

instance Show IconType where
        showsPrec d Blank r = "blank" ++ r
        showsPrec d (IconType icon file z) r = 
                "icon_entry("++show icon++comma++show file++comma++show z++")" 
                ++ r


comma      = "," 
