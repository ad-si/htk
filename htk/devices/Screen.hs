{- #######################################################################

MODULE        : Screen
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Screen specific properties 

   #################################################################### -}


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

newtype Window w => Screen w = Screen w


-- -----------------------------------------------------------------------
-- Screen dimensions
-- -----------------------------------------------------------------------

getScreenHeight :: GUIObject a => Screen a -> IO Distance
getScreenHeight (Screen win) = 
        evalMethod win (\nm -> ["winfo screenheight " ++ show nm])

getScreenWidth :: GUIObject a => Screen a -> IO Distance
getScreenWidth (Screen win)= 
        evalMethod win (\nm -> ["winfo screenwidth " ++ show nm])

getScreenVisual :: GUIObject a => Screen a -> IO VisualClass     
getScreenVisual (Screen win) = 
        evalMethod win (\nm -> ["winfo screenvisual " ++ show nm])      
        
getScreenManager :: GUIObject a => Screen a -> IO String 
getScreenManager (Screen win) = 
        evalMethod win (\nm -> ["winfo manager " ++ show nm])   
        

-- -----------------------------------------------------------------------
-- Screen Colours 
-- -----------------------------------------------------------------------

data VisualClass = 
          DirectColour
        | GrayScale
        | PseudoColour
        | StaticColour
        | StaticGray
        | TrueColour
        deriving (Eq,Ord,Enum)

instance GUIValue VisualClass where
        cdefault = DirectColour

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
