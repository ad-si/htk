{- #########################################################################

MODULE        : DaVinciClasses
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  {ewk}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Nodes


   ######################################################################### -}


module DaVinciClasses (
        module Font,

        DaVinciObject(..),
        TypedDaVinciObject(..),

        DaVinciNode(..),
        Shape(..),
        Border(..),

        DaVinciEdge(..),
        Pattern(..),
        ArrowHead(..)

        ) where

import Computation

import GUICore
import Font

import Char(isSpace,toLower)
import Line(ArrowHead(..))

import DaVinciCore
import Debug(debug)



-- ---------------------------------------------------------------------------
--  DaVinci Objects
-- ---------------------------------------------------------------------------

class GUIObject w => DaVinciObject w where
        getGraphContext    :: w -> IO Graph
        getDaVinciObjectID :: w -> IO String


class GUIObject w => TypedDaVinciObject w where
        typename        :: String -> Config w
        getTypeName     :: w -> IO String
        

-- ---------------------------------------------------------------------------
--  DaVinci Node
-- ---------------------------------------------------------------------------

class GUIObject w => DaVinciNode w where
        border          :: Border -> Config w
        getBorder       :: w -> IO Border
        shape           :: Shape -> Config w
        getShape        :: w -> IO Shape        
        fontfamily      :: FontFamily -> Config w
        fontstyle       :: (Maybe FontWeight,Maybe FontSlant) -> Config w

        border s w      = cset w "BORDER" s
        getBorder w     = cget w "BORDER"

        shape s w       = cset w "_GO" s
        getShape w      = cget w "_GO"
        
        fontfamily fam w =
                cset w "FONTFAMILY" (RawData (map toLower (show fam)))

        fontstyle (Nothing,Nothing) w = 
                return w
        fontstyle (Nothing,Just slant) w =
                cset w "FONTSTYLE" (FontStyle cdefault slant)
        fontstyle (Just weight,Nothing) w =
                cset w "FONTSTYLE" (FontStyle weight cdefault)
        fontstyle (Just weight,Just slant) w =
                cset w "FONTSTYLE" (FontStyle weight slant)



-- ---------------------------------------------------------------------------
--  DaVinci Edge
-- ---------------------------------------------------------------------------

-- data ArrowHead = BothEnds | LastEnd | FirstEnd | NoHead 

class GUIObject w => DaVinciEdge w where
        arrowhead       :: ArrowHead -> Config w
        getArrowhead    :: w -> IO ArrowHead
        pattern         :: Pattern -> Config w
        getPattern      :: w -> IO Pattern

        arrowhead s g = cset g "_DIR" (arrowHeadToDir s)
        getArrowhead g = cget g "_DIR" >>= return . dirToArrowHead
        pattern s g = cset g "EDGEPATTERN" s
        getPattern g = cget g "EDGEPATTERN"



-- ---------------------------------------------------------------------------
--  ArrowHead
-- ---------------------------------------------------------------------------

arrowHeadToDir BothEnds = RawData "both"
arrowHeadToDir LastEnd = RawData "normal"
arrowHeadToDir FirstEnd = RawData "inverse"
arrowHeadToDir NoHead = RawData "none"  

dirToArrowHead (RawData "both") = BothEnds
dirToArrowHead (RawData "normal") = LastEnd
dirToArrowHead (RawData "inverse") = FirstEnd
dirToArrowHead (RawData "none")   = NoHead
dirToArrowHead _                  = cdefault


-- ---------------------------------------------------------------------------
-- Shape
-- ---------------------------------------------------------------------------

data Shape       = Box | Circle | Ellipse | Rhombus | Textual | Iconic
        deriving (Eq,Ord,Enum)

instance Read Shape where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'b':'o':'x':xs -> [(Box,xs)]
        'c':'i':'r':'c':'l':'e':xs -> [(Circle,xs)]
        'e':'l':'l':'i':'p':'s':'e':xs -> [(Ellipse,xs)]
        'r':'h':'o':'m':'b':'u':'s':xs -> [(Rhombus,xs)]
        't':'e':'x':'t':xs -> [(Textual,xs)]
        'i':'c':'o':'n':xs -> [(Iconic,xs)]
        _ -> []

instance Show Shape where
   showsPrec d p r = 
      (case p of 
        Box -> "box" 
        Circle -> "circle"
        Ellipse -> "ellipse"
        Rhombus -> "rhombus"
        Textual -> "text"
        Iconic -> "icon"
        ) ++ r

instance GUIValue Shape where
        cdefault = Box


-- ---------------------------------------------------------------------------
--  FontStyle
-- ---------------------------------------------------------------------------

data FontStyle  = FontStyle FontWeight FontSlant

instance Read FontStyle  where  
   readsPrec p b =
     case dropWhile (isSpace) b of
        'n':'o':'r':'m':'a':l':xs -> [((FontStyle NormalWeight Roman),xs)]
        'i':'t':'a':'l':'i':'c':xs -> [((FontStyle NormalWeight Italic),xs)]
        'b':'o':'l':'d':'-':'i':'t':'a':'l':'i':'c':xs -> [((FontStyle Bold Italic),xs)]
        'b':'o':'l':'d':xs -> [((FontStyle Bold Roman),xs)]
        _ -> []

instance Show FontStyle  where  
   showsPrec d p r = 
      (case p of 
        (FontStyle NormalWeight Roman) -> "normal" 
        (FontStyle Bold Roman) -> "bold"
        (FontStyle NormalWeight Italic) -> "italic"
        (FontStyle Bold Italic) -> "bold-italic"
        ) ++ r

instance GUIValue FontStyle where
        cdefault = FontStyle cdefault cdefault



-- ---------------------------------------------------------------------------
--  Border
-- ---------------------------------------------------------------------------

data Border = SingleBorder | DoubleBorder
        deriving (Eq,Ord,Enum)

instance Read Border where
   readsPrec p b =
     case dropWhile (isSpace) b of
        's':'i':'n':'g':'l':'e':xs -> [(SingleBorder,xs)]
        'd':'o':'u':'b':'l':'e':xs -> [(DoubleBorder,xs)]
        _ -> []

instance Show Border where
   showsPrec d p r = 
      (case p of 
        SingleBorder -> "single" 
        DoubleBorder -> "double"
        ) ++ r


instance GUIValue Border where
        cdefault = SingleBorder


-- ---------------------------------------------------------------------------
--  Pattern
-- ---------------------------------------------------------------------------

data Pattern  = SolidLine | DottedLine | DashedLine | ThickLine | DoubleLine
        deriving (Eq,Ord,Enum)

instance Read Pattern where
   readsPrec p b =
     case dropWhile (isSpace) b of
        's':'o':'l':'i':'d':xs -> [(SolidLine,xs)]
        'd':'o':'t':'t':'e':'d':xs -> [(DottedLine,xs)]
        'd':'a':'s':'h':'e':'d':xs -> [(DashedLine,xs)]
        't':'h':'i':'c':'k':xs -> [(ThickLine,xs)]
        'd':'o':'u':'b':'l':'e':xs -> [(DoubleLine,xs)]
        _ -> []

instance Show Pattern where
   showsPrec d p r = 
      (case p of 
        SolidLine -> "solid" 
        DottedLine -> "dotted"
        DashedLine -> "dashed"
        ThickLine -> "thick"
        DoubleLine -> "double"
        ) ++ r

instance GUIValue Pattern where
        cdefault = SolidLine
