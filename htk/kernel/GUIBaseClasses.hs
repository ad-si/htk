{- #########################################################################

MODULE        : GUIBaseClasses
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic classes for defining layout of widgets and items. 


   ######################################################################### -}


module GUIBaseClasses (
        module Resources,

        Destructible(..), 
        HasReceiveEV(..),
        HasReceiveIO(..),

        GUIOBJECT, 
        GUIObject(..),
        ConfigID,

        Toggle(..),
        toggle,

        ToplevelWindow(..),

        Relief(..),
        HasBorder(..),

        Colour,
        ColourDesignator(..),
        HasColour(..),

        background,
        getBackground,

        foreground,
        getForeground,

        activeBackground,
        getActiveBackground,

        activeForeground,
        getActiveForeground,

        disabledForeground,
        getDisabledForeground,
        bg,fg,
        hasBackGroundColour,hasForeGroundColour,

        Distance,
        HasSize(..),
        HasPosition(..),
        HasGeometry(..),

        Font,
        FontDesignator(..),
        FontFamily(..),
        FontWeight,
        FontSlant,
        FontWidth,
        FontSpacing,
        HasFont(..),

        HasText(..),

        HasUnderline(..),

        illegalGUIValue,
        Variable(..),
        value ,
        getValue,
        updValue,

        HasJustify(..), 

        HasGrid(..), 

        HasOrientation(..),

        HasFile(..),

        HasAlign(..),

        HasIncrement(..)

) where

import SIM
import Dynamics
import GUIState
import GUIRealise
import Resources
import GUIObject
import Char
import Debug(debug)

-- --------------------------------------------------------------------------
-- Class Toplevel Window 
-- --------------------------------------------------------------------------           
class GUIObject w => ToplevelWindow w where
        iconify :: w -> IO ()
        deiconify :: w -> IO ()
        withdraw :: w -> IO ()
        putWinOnTop :: w -> IO ()
        putWinAtBottom :: w -> IO ()


-- --------------------------------------------------------------------------
-- Coloured 
-- --------------------------------------------------------------------------

class GUIObject w => HasColour w where
        legalColourID :: w -> ConfigID -> Bool
        setColour :: w -> ConfigID -> Colour -> IO w
        getColour :: w -> ConfigID -> IO Colour
        legalColourID _ "background" = True
        legalColourID _ _ = False
        setColour w cid col = 
                if legalColourID w cid then cset w cid col else return w 
        getColour w cid = 
                if legalColourID w cid then cget w cid else return cdefault 


background :: (ColourDesignator c, HasColour w) => c -> Config w
background c w = setColour w "background" (toColour c)

getBackground :: HasColour w => w -> IO Colour
getBackground w = getColour w "background" 

foreground :: (ColourDesignator c, HasColour w) => c -> Config w
foreground c w = setColour w "foreground" (toColour c)

getForeground :: HasColour w => w -> IO Colour
getForeground w = getColour w "foreground"

activeBackground :: (ColourDesignator c, HasColour w) => c -> Config w
activeBackground c w = setColour w "activebackground" (toColour c)

getActiveBackground :: HasColour w => w -> IO Colour    
getActiveBackground w = getColour w "activebackground"  
 
activeForeground :: (ColourDesignator c, HasColour w) => c -> Config w
activeForeground c w = setColour w "activeforeground" (toColour c)

getActiveForeground :: HasColour w => w -> IO Colour    
getActiveForeground w = getColour w "activeforeground"    

disabledForeground :: (ColourDesignator c, HasColour w) => c -> Config w
disabledForeground c w = setColour w "disabledforeground" (toColour c)

getDisabledForeground :: HasColour w => w -> IO Colour
getDisabledForeground w = getColour w "disabledforeground"

fg :: (ColourDesignator c, HasColour w) => c -> Config w
fg = foreground

bg :: (ColourDesignator c, HasColour w) => c -> Config w
bg = background

hasBackGroundColour :: HasColour w => w -> ConfigID -> Bool
hasBackGroundColour w "background" = True
hasBackGroundColour w _ = False

hasForeGroundColour :: HasColour w => w -> ConfigID -> Bool
hasForeGroundColour w "background" = True
hasForeGroundColour w "foreground" = True
hasForeGroundColour w _ = False

-- --------------------------------------------------------------------------
-- Geometry 
-- --------------------------------------------------------------------------

class GUIObject w => HasSize w where
        width      :: Distance -> Config w
        getWidth   :: w -> IO Distance
        height     :: Distance -> Config w
        getHeight  :: w -> IO Distance
        size       :: Size -> Config w
        getSize    :: w -> IO Size
        width s w   = cset w "width" s
        getWidth w  = tkcget w "width"
        height s w  = cset w "height" s
        getHeight w = tkcget w "height"
        size (x,y) w =width x w >> height y w
        getSize w =  getWidth w >>= \ x -> getHeight w >>= \ y -> return (x,y)


class GUIObject w => HasPosition w where
        position    :: Position -> Config w
        getPosition :: w -> IO Position


class (HasSize w, HasPosition w) => HasGeometry w where
        geometry    :: Geometry -> Config w
        getGeometry :: w -> IO Geometry


-- --------------------------------------------------------------------------
-- Has Border 
-- --------------------------------------------------------------------------

class GUIObject w => HasBorder w where
        borderwidth     :: Distance -> Config w
        getBorderwidth  :: w -> IO Distance
        relief          :: Relief -> Config w
        getRelief       :: w -> IO Relief
        borderwidth s w  = cset w "borderwidth" s
        getBorderwidth w = cget w "borderwidth"
        relief r w       = cset w "relief" r
        getRelief w      = cget w "relief"


-- --------------------------------------------------------------------------
-- Text Labelled Widgets 
-- --------------------------------------------------------------------------

class (GUIObject w,GUIValue v) => HasText w v where
        text       :: v -> Config w
        getText    :: w -> IO v
        text t w    = cset w "text" t
        getText w   = cget w "text"

class GUIObject w => HasFont w where
        font     :: FontDesignator f => f -> Config w
        getFont  :: w -> IO Font
        font f w  = cset w "font" (toFont f)
        getFont w = cget w "font"

class GUIObject w => HasUnderline w where  {- actually for buttons + labels -}
        underline      :: Int -> Config w
        getUnderline   :: w -> IO Int
        wraplength     :: Int -> Config w
        getWraplength  :: w -> IO Int
        underline i w   = cset w "underline" i
        getUnderline w  = cget w "underline"
        wraplength l w  = cset w "wraplength" l
        getWraplength w = cget w "wraplength" 

class GUIObject w => HasJustify w where
        justify     :: Justify -> Config w
        getJustify  :: w -> IO Justify
        justify js w = cset w "justify" js
        getJustify w = cget w "justify"


-- --------------------------------------------------------------------------
-- Stateful Widgets 
-- --------------------------------------------------------------------------

value       :: Variable w a => a -> Config (w a)
value a w = setVar w a >> return w      

getValue    :: Variable w a => w a -> IO a
getValue = getVar

updValue    :: Variable w a => w a -> (a -> a) -> IO a
updValue w f = getValue w >>= \ v -> 
                let v' = (f v) in do {
                        configure w [value v'];
                        return v'
                        }


-- --------------------------------------------------------------------------
-- Grid 
-- --------------------------------------------------------------------------

class GUIObject w => HasGrid w where
        grid       :: Toggle -> Config w
        getGrid    :: w -> IO Toggle
        grid b w    = cset w "setgrid" b
        getGrid w   = cget w "setgrid"


-- --------------------------------------------------------------------------
-- Orientation 
-- --------------------------------------------------------------------------

class GUIObject w => HasOrientation w where
        orient      :: Orientation -> Config w
        getOrient   :: w -> IO Orientation
        orient o w   = cset w "orient" o
        getOrient w  = cget w "orient"

-- --------------------------------------------------------------------------
-- File 
-- --------------------------------------------------------------------------

class GUIObject w => HasFile w where
        filename :: String -> Config w
        getFileName :: w -> IO String
        

-- --------------------------------------------------------------------------
-- Align 
-- --------------------------------------------------------------------------
        
class GUIObject w => HasAlign w where
        align     :: Alignment -> Config w
        getAlign  :: w -> IO Alignment
        align a w  = cset w "align" a
        getAlign w = cget w "align"


-- --------------------------------------------------------------------------
-- Increment   (canvas region, scales)
-- --------------------------------------------------------------------------
        
class HasIncrement w a where
        increment       :: a -> Config w
        getIncrement    :: w -> IO a

