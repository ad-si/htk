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

---
-- The <code>module Screen</code> exports general functionality on the
-- screen's properties.
module Screen (

  Distance,
  Screen(..),
  getScreenHeight,
  getScreenWidth,
  getScreenManager,

  VisualClass(..),
  getScreenVisual

) where

import Core
import Geometry(Distance)
import Char(isSpace)
import Window


-- -----------------------------------------------------------------------
-- Screen
-- -----------------------------------------------------------------------

---
-- The <code>Screen</code> datatype.
newtype Window w => Screen w = Screen w


-- -----------------------------------------------------------------------
-- Screen dimensions
-- -----------------------------------------------------------------------

---
-- Gets the height of the screen.
-- @param scr     - the concerned screen.
-- @return result - The screen's height.
getScreenHeight :: GUIObject a => Screen a -> IO Distance
getScreenHeight scr@(Screen win) = 
        evalMethod win (\nm -> ["winfo screenheight " ++ show nm])

---
-- Gets the width of the screen.
-- @param scr     - the concerned screen.
-- @return result - The screen's width.
getScreenWidth :: GUIObject a => Screen a -> IO Distance
getScreenWidth scr@(Screen win)= 
        evalMethod win (\nm -> ["winfo screenwidth " ++ show nm])

---
-- Gets the visual properties of the screen.
-- @param scr     - the concerned screen.
-- @return result - The visual properties.
getScreenVisual :: GUIObject a => Screen a -> IO VisualClass     
getScreenVisual scr@(Screen win) = 
        evalMethod win (\nm -> ["winfo screenvisual " ++ show nm])      

---
-- Gets the screen manager from a screen.
-- @param scr     - the concerned screen.
-- @return result - A textual representation of the screen manager.
getScreenManager :: GUIObject a => Screen a -> IO String 
getScreenManager (Screen win) = 
        evalMethod win (\nm -> ["winfo manager " ++ show nm])   
        

-- -----------------------------------------------------------------------
-- Screen Colours 
-- -----------------------------------------------------------------------

---
-- The <code>VisualClass</code> datatype (see
-- <code>Screen.getScreenVisual</code>).
data VisualClass = 
          DirectColour
        | GrayScale
        | PseudoColour
        | StaticColour
        | StaticGray
        | TrueColour
        deriving (Eq,Ord,Enum)

---
-- Internal.
instance GUIValue VisualClass where
---
-- Internal.
        cdefault = DirectColour

---
-- Internal.
instance Read VisualClass where
---
-- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'd':'i':'r':'e':'c':'t':'c':'o':'l':'o':'r':xs -> [(DirectColour,xs)]
        'g':'r':'a':'y':'s':'c':'a':'l':'e':xs -> [(GrayScale,xs)]
        'p':'s':'e':'u':'d':'o':'c':'o':'l':'o':'r':xs -> [(PseudoColour,xs)]
        's':'t':'a':'t':'i':'c':'c':'o':'l':'o':'r':xs -> [(StaticColour,xs)]
        's':'t':'a':'t':'i':'c':'g':'r':'a':'y':xs -> [(StaticGray,xs)]
        't':'r':'u':'e':'c':'o':'l':'o':'r':xs -> [(TrueColour,xs)]
        _ -> []

---
-- Internal.
instance Show VisualClass where
---
-- Internal.
   showsPrec d p r = 
      (case p of 
         DirectColour -> "directcolor"
         GrayScale -> "grayscale"
         PseudoColour -> "pseudocolor"
         StaticColour -> "staticcolor"
         StaticGray -> "staticgray"
         TrueColour -> "truecolor"
        ) ++ r
