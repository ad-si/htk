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
  pswidth,
  psheight,
  pssize,
  psfile,

  ColourMode(..),
  colourmode

) where


import Core
import Char(isSpace)
import Computation
import Configuration
import Destructible
import Geometry
import Resources
import Packer


-- -----------------------------------------------------------------------
-- HasPostscript class
-- -----------------------------------------------------------------------

class GUIObject w => HasPostscript w where
  postscript :: w -> [CreationConfig PostScript] -> IO ()
  postscript target confs =
    do
      confstr <- showCreationConfigs confs
      try
        (execMethod target (\nm -> [tkPostScript nm confstr]))
      done
    where tkPostScript :: ObjectName -> String -> TclCmd
          tkPostScript name confstr = 
            show name ++ " postscript " ++ confstr


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

data PostScript = PostScript


-- -----------------------------------------------------------------------
-- ColourModes
-- -----------------------------------------------------------------------

data ColourMode =
  FullColourMode | GrayScaleMode | MonoChromeMode deriving (Eq,Ord,Enum)

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


-- -----------------------------------------------------------------------
-- Configuation Options
-- -----------------------------------------------------------------------

colourmode :: ColourMode -> CreationConfig PostScript
colourmode cmode = return ("colormode " ++ show cmode)

pageheight :: Distance -> CreationConfig PostScript
pageheight h = return ("pageheight " ++ show h)

pagewidth :: Distance -> CreationConfig PostScript
pagewidth h = return ("pagewidth " ++ show h)

pagex :: Distance -> CreationConfig PostScript
pagex h = return ("pagex " ++ show h)

pagey :: Distance -> CreationConfig PostScript
pagey h = return ("pagey " ++ show h)

rotate :: Bool -> CreationConfig PostScript
rotate r = return ("rotate" ++ show r)

pageAnchor :: Anchor -> CreationConfig PostScript
pageAnchor anch = return ("pageanchor" ++ show anch)

pswidth :: Distance -> CreationConfig PostScript
pswidth w = return ("width " ++ show w)

psheight :: Distance -> CreationConfig PostScript
psheight h = return ("height " ++ show h)

pssize :: Size -> CreationConfig PostScript
pssize (w, h) =
  do
    wstr <- pswidth w
    hstr <- psheight h
    return (wstr ++ " -" ++ hstr)

psfile :: String -> CreationConfig PostScript
psfile fnm = return ("file " ++ fnm)
