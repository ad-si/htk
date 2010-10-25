-- | This module provides funtionality for postscript export of the contents
-- of canvas widgets.
module HTk.Devices.Printer (

  HasPostscript(..),

  PostScript,
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


import Data.Char(isSpace)
import Control.Exception

import HTk.Kernel.Core
import HTk.Kernel.Geometry
import HTk.Kernel.Resources

-- -----------------------------------------------------------------------
-- HasPostscript class
-- -----------------------------------------------------------------------

-- | Widgets that support postscript export instantiate the
-- @class HasPostscript@.
class GUIObject w => HasPostscript w where
  -- Exports postscript from the given widget.
  postscript :: w -> [CreationConfig PostScript] -> IO ()
  postscript target confs =
    do
      confstr <- showCreationConfigs confs
      try
        (execMethod target (\nm -> [tkPostScript nm confstr]))
        :: IO (Either SomeException ())
      return ()
    where tkPostScript :: ObjectName -> String -> TclCmd
          tkPostScript name confstr =
            show name ++ " postscript " ++ confstr


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @PostScript@ datatype.
data PostScript = PostScript


-- -----------------------------------------------------------------------
-- ColourModes
-- -----------------------------------------------------------------------

-- | The @ColourMode@ datatype.
data ColourMode =
  FullColourMode | GrayScaleMode | MonoChromeMode deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue ColourMode where
  cdefault = FullColourMode

-- | Internal.
instance Read ColourMode where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'c':'o':'l':'o':'r':xs -> [(FullColourMode,xs)]
        'g':'r':'a':'y':xs -> [(GrayScaleMode,xs)]
        'm':'o':'n':'o':xs -> [(MonoChromeMode,xs)]
        _ -> []

-- | Internal.
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

-- | Sets the colourmode.
colourmode :: ColourMode -> CreationConfig PostScript
colourmode cmode = return ("colormode " ++ show cmode)

-- | Sets the page height.
pageheight :: Distance -> CreationConfig PostScript
pageheight h = return ("pageheight " ++ show h)

-- | Sets the page width.
pagewidth :: Distance -> CreationConfig PostScript
pagewidth h = return ("pagewidth " ++ show h)

-- | Sets the output x coordinate of the anchor point.
pagex :: Distance -> CreationConfig PostScript
pagex h = return ("pagex " ++ show h)

-- | Sets the output y coordinate of the anchor point.
pagey :: Distance -> CreationConfig PostScript
pagey h = return ("pagey " ++ show h)

-- | If @True@, rotate so that X axis isthe long direction of the
-- page.
rotate :: Bool -> CreationConfig PostScript
rotate r = return ("rotate" ++ show r)

-- | Sets the page anchor.
pageAnchor :: Anchor -> CreationConfig PostScript
pageAnchor anch = return ("pageanchor" ++ show anch)

-- | Sets the width of the area to print.
pswidth :: Distance -> CreationConfig PostScript
pswidth w = return ("width " ++ show w)

-- | Sets the height of the area to print.
psheight :: Distance -> CreationConfig PostScript
psheight h = return ("height " ++ show h)

-- | Sets the width and height of the area to print.
pssize :: Size -> CreationConfig PostScript
pssize (w, h) =
  do
    wstr <- pswidth w
    hstr <- psheight h
    return (wstr ++ " -" ++ hstr)

-- | Sets the filename of the output file.
psfile :: String -> CreationConfig PostScript
psfile fnm = return ("file " ++ fnm)
