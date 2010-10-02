-- | The @module Screen@ exports general functionality on the
-- screen\'s properties.
module HTk.Devices.Screen (

  Screen(..),
  getScreenHeight,
  getScreenWidth,
  getScreenManager,

  VisualClass(..),
  getScreenVisual

) where

import HTk.Kernel.Core
import HTk.Kernel.Geometry(Distance)
import Data.Char(isSpace)
import HTk.Containers.Window


-- -----------------------------------------------------------------------
-- Screen
-- -----------------------------------------------------------------------

-- | The @Screen@ datatype.
newtype Screen w = Screen w


-- -----------------------------------------------------------------------
-- Screen dimensions
-- -----------------------------------------------------------------------

-- | Gets the height of the screen.
getScreenHeight :: Window a => Screen a
   -- ^ the concerned screen.
   -> IO Distance
   -- ^ The screen\'s height.
getScreenHeight scr@(Screen win) =
        evalMethod win (\nm -> ["winfo screenheight " ++ show nm])

-- | Gets the width of the screen.
getScreenWidth :: Window a => Screen a
   -- ^ the concerned screen.
   -> IO Distance
   -- ^ The screen\'s width.
getScreenWidth scr@(Screen win)=
        evalMethod win (\nm -> ["winfo screenwidth " ++ show nm])

-- | Gets the visual properties of the screen.
getScreenVisual :: Window a => Screen a
   -- ^ the concerned screen.
   -> IO VisualClass
   -- ^ The visual properties.
getScreenVisual scr@(Screen win) =
        evalMethod win (\nm -> ["winfo screenvisual " ++ show nm])

-- | Gets the screen manager from a screen.
getScreenManager :: Window a => Screen a
   -- ^ the concerned screen.
   -> IO String
   -- ^ A textual representation of the screen manager.
getScreenManager (Screen win) =
        evalMethod win (\nm -> ["winfo manager " ++ show nm])


-- -----------------------------------------------------------------------
-- Screen Colours
-- -----------------------------------------------------------------------

-- | The @VisualClass@ datatype (see
-- @Screen.getScreenVisual@).
data VisualClass =
          DirectColour
        | GrayScale
        | PseudoColour
        | StaticColour
        | StaticGray
        | TrueColour
        deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue VisualClass where
        cdefault = DirectColour

-- | Internal.
instance Read VisualClass where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'd':'i':'r':'e':'c':'t':'c':'o':'l':'o':'r':xs -> [(DirectColour,xs)]
        'g':'r':'a':'y':'s':'c':'a':'l':'e':xs -> [(GrayScale,xs)]
        'p':'s':'e':'u':'d':'o':'c':'o':'l':'o':'r':xs -> [(PseudoColour,xs)]
        's':'t':'a':'t':'i':'c':'c':'o':'l':'o':'r':xs -> [(StaticColour,xs)]
        's':'t':'a':'t':'i':'c':'g':'r':'a':'y':xs -> [(StaticGray,xs)]
        't':'r':'u':'e':'c':'o':'l':'o':'r':xs -> [(TrueColour,xs)]
        _ -> []

-- | Internal.
instance Show VisualClass where
   showsPrec d p r =
      (case p of
         DirectColour -> "directcolor"
         GrayScale -> "grayscale"
         PseudoColour -> "pseudocolor"
         StaticColour -> "staticcolor"
         StaticGray -> "staticgray"
         TrueColour -> "truecolor"
        ) ++ r
