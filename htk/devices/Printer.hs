{- #########################################################################

MODULE        : Printer
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A simple printer utitity wrapped around Tk's print feature.
                This means - it only works for canvas items (sorry).


   ######################################################################### -}



module Printer (
        HasPostscript(..),

        PostScript,
        postscript,
        pageheight,
        pagewidth,
        pagex,
        pagey,
        rotate,
        pageAnchor,
        getPageAnchor,

        ColourMode(..),
        colourmode

        ) where

import Thread
import GUICore
import Char(isSpace)
import Debug(debug)

-- --------------------------------------------------------------------------
-- HasPostscript Objects 
-- -------------------------------------------------------------------------- 

class GUIObject w => HasPostscript w where
        postscript :: w -> [Config PostScript] -> IO ()
        postscript target confs  = do {
                wid <- createGUIObject POSTSCRIPT defMethods;
                configure (PostScript wid) confs;
                args <- lookupConfigs wid;
                catch 
                        (execMethod target (\nm -> [tkPostScript nm args]))
                        (\e -> destroy wid >> raise e); 
                destroy wid
        }where tkPostScript :: ObjectName -> [ConfigOption] -> TclCmd
               tkPostScript name args = 
                   show name ++ " postscript " ++ showConfigs args


-- --------------------------------------------------------------------------
-- Postscript Objects 
-- -------------------------------------------------------------------------- 

newtype PostScript = PostScript GUIOBJECT

-- --------------------------------------------------------------------------
-- ColourModes 
-- -------------------------------------------------------------------------- 

data ColourMode = FullColourMode | GrayScaleMode | MonoChromeMode
        deriving (Eq,Ord,Enum)

instance GUIValue ColourMode where
        cdefault = FullColourMode

instance Read ColourMode where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'c':'o':'l':'o':'r':xs -> [(FullColourMode,xs)]
        'g':'r':'a':'y':xs -> [(GrayScaleMode,xs)]
        'm':'o':'n':'o':xs -> [(MonoChromeMode,xs)]
        _ -> []

instance Show ColourMode where
   showsPrec d p r = 
      (case p of 
         FullColourMode -> "color"
         GrayScaleMode -> "gray"
         MonoChromeMode -> "mono"
        ) ++ r

-- --------------------------------------------------------------------------
--  Configuation Options
-- -------------------------------------------------------------------------- 

colourmode :: ColourMode -> Config PostScript
colourmode cmode w = cset w "colormode" cmode

pageheight :: Distance -> Config PostScript
pageheight h w = cset w  "pageheight" h

pagewidth :: Distance -> Config PostScript
pagewidth h w = cset w  "pagewidth" h

pagex :: Distance -> Config PostScript
pagex h w = cset w  "pagex" h

pagey :: Distance -> Config PostScript
pagey h w = cset w  "pagey" h

rotate :: Bool -> Config PostScript
rotate r w = cset w  "rotate" r

pageAnchor :: Anchor -> Config PostScript
pageAnchor anch w = cset w  "pageanchor" anch

getPageAnchor :: PostScript -> IO Anchor
getPageAnchor w = cget w "pageanchor"


-- --------------------------------------------------------------------------
--  Instantiations
-- -------------------------------------------------------------------------- 

instance GUIObject PostScript where
        toGUIObject (PostScript w) = w
        cname _ = "PostScript"

instance HasSize PostScript

instance HasFile PostScript where
        filename fname w = cset w  "file" fname
        getFileName w = cget w  "file"
