{- #########################################################################

MODULE        : Resources

AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Misc. resource types of relevance for Haskell-Tk.


   ######################################################################### -}


module Resources (
        module Colour,
        module Cursor,
        module Font,
        module GUIEvent,
        module GUIValue,
        module Geometry,

        State(..),

        Justify(..),

        Relief(..),

        Toggle(..),
        toggle,

        Orientation(..),

        Alignment(..)

) where

import Colour
import Cursor
import Font
import Geometry
import GUIValue
import GUIEvent
import Char
import Dynamics
import Debug(debug)


-- --------------------------------------------------------------------------
--  State 
-- --------------------------------------------------------------------------

data State = Disabled | Active | Normal deriving (Eq,Ord,Enum)
 
instance GUIValue State where
        cdefault = Disabled

instance Read State where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'd':'i':'s':'a':'b':'l':'e':'d': xs -> [(Disabled,xs)]
        'a':'c':'t':'i':'v':'e': xs -> [(Active,xs)]
        'n':'o':'r':'m':'a':'l': xs -> [(Normal,xs)]
        _ -> []

instance Show State where
   showsPrec d p r = 
      (case p of 
          Disabled -> "disabled"
          Active -> "active"
          Normal -> "normal"
        ) ++ r



-- --------------------------------------------------------------------------
--  Justify 
-- --------------------------------------------------------------------------

data Justify = JustLeft | JustCenter | JustRight deriving (Eq,Ord,Enum)

instance GUIValue Justify where
        cdefault = JustLeft

instance Read Justify where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(JustLeft,xs)]
        'c':'e':'n':'t':'e':'r':xs -> [(JustCenter,xs)]
        'r':'i':'g':'h':'t':xs -> [(JustRight,xs)]
        _ -> []

instance Show Justify where
   showsPrec d p r = 
      (case p of 
        JustLeft -> "left"
        JustCenter -> "center"
        JustRight -> "right"
        ) ++ r


-- --------------------------------------------------------------------------
--  Relief 
-- --------------------------------------------------------------------------

data Relief = Groove | Ridge | Flat | Sunken | Raised deriving (Eq,Ord,Enum)

instance GUIValue Relief where
        cdefault = Flat

instance Read Relief where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'g':'r':'o':'o':'v':'e':xs -> [(Groove,xs)]
        'r':'i':'d':'g':'e':xs -> [(Ridge,xs)]
        'f':'l':'a':'t':xs -> [(Flat,xs)]
        's':'u':'n':'k':'e':'n':xs -> [(Sunken,xs)]
        'r':'a':'i':'s':'e':'d':xs -> [(Raised,xs)]
        _ -> []

instance Show Relief where
   showsPrec d p r = 
      (case p of 
        Groove -> "groove"
        Ridge -> "ridge"
        Flat -> "flat"
        Sunken -> "sunken"
        Raised -> "raised"
        ) ++ r


-- --------------------------------------------------------------------------
--  Orientation 
-- --------------------------------------------------------------------------

data Orientation = Horizontal | Vertical deriving (Eq,Ord,Enum)

instance GUIValue Orientation where
        cdefault = Horizontal

instance Read Orientation where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'h':'o':'r':'i':'z':'o':'n':'t':'a':'l':xs -> [(Horizontal,xs)]
        'v':'e':'r':'t':'i':'c':'a':'l':xs -> [(Vertical,xs)]
        _ -> []
 
instance Show Orientation where
   showsPrec d p r = 
      (case p of 
        Horizontal -> "horizontal"
        Vertical -> "vertical"
        ) ++ r


-- --------------------------------------------------------------------------
--  Toggle 
-- --------------------------------------------------------------------------

data Toggle = Off | On deriving (Eq,Ord)

instance GUIValue Toggle where
        cdefault = Off

instance Read Toggle where
   readsPrec p b =
     case dropWhile (isSpace) b of
        '0':xs -> [(Off,xs)]
        '1':xs -> [(On,xs)]
        _ -> []

instance Show Toggle where
   showsPrec d p r = 
      (case p of 
        Off -> "0" 
        On -> "1"
        ) ++ r


toggleT :: TyCon 
toggleT = mkTyCon "Resources" "Toggle"

instance Typeable Toggle where
        typeOf _ = mkTypeTag toggleT []


toggle :: Toggle -> Toggle
toggle On = Off
toggle Off = On


-- --------------------------------------------------------------------------
--  Alignment 
-- --------------------------------------------------------------------------

data Alignment = Top | InCenter | Bottom | Baseline deriving (Eq,Ord,Enum)

instance GUIValue Alignment where
        cdefault = Top                                  -- Check

instance Read Alignment where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'c':'e':'n':'t':'e':'r':xs -> [(InCenter,xs)]
        't':'o':'p': xs -> [(Top,xs)]
        'b':'o':'t':'t':'o':'m':xs -> [(Bottom,xs)]
        'b':'a':'s':'e':'l':'i':'n':'e':xs -> [(Baseline,xs)]
        _ -> []

instance Show Alignment where
   showsPrec d p r = 
      (case p of 
        Top -> "top"
        InCenter -> "center"
        Bottom -> "bottom"
        Baseline -> "baseline"
        ) ++ r

