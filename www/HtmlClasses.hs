{- #########################################################################

MODULE        : HtmlClasses
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Classes.

   ######################################################################### -}


module HtmlClasses (
        GUIObject(..),
        HasName(..),

        HtmlElem(..),
        HasAlt(..),
        HasAlign(..),
        HasSpace(..),
        HasThickness(..),
        HasHtmlBorder(..),
        HasHtmlValue(..),
        HasFontSize(..),

        HtmlList(..),

        HtmlHyperLink(..),
        HtmlRelation(..),

        Direction(..),
        HtmlTableElem(..),
        HtmlTableCell(..),
        HtmlRowElem(..),
        HasHorizontalAlign(..),
        HasVerticalAlign(..),   

        RawData(..),
        RawData(..),
        CVOID(..),


        )

where

import HTk
import HtmlTypes
import Char
import Debug(debug)



-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class GUIObject w => HtmlElem w where
        toHtmlElem :: w -> HtmlElement
        fromHtmlElem :: HtmlElement -> w


class HtmlElem w => HasAlt w where
        alt :: String -> Config w       
        alt s e = cset e "ALT" (RawData s)


class HtmlElem w => HasThickness w where
        thickness :: Distance -> Config w
        thickness d e = cset e "SIZE" d


class HtmlElem w => HasHtmlBorder w where
        border :: Distance -> Config w
        border d e = cset e "BORDER" d


class HtmlElem w => HasHtmlValue w where
        htmlvalue :: String -> Config w
        htmlvalue s e = cset e "VALUE" (RawData s)


class HtmlElem w => HasSpace w where
        hspace :: Distance -> Config w
        vspace :: Distance -> Config w
        hspace d e = cset e "HSPACE" d
        vspace d e = cset e "VSPACE" d


class HtmlElem w => HasFontSize w where
        fontsize :: Int -> Config w
        fontsize i w = cset w "SIZE" i


-- --------------------------------------------------------------------------
-- Name
-- --------------------------------------------------------------------------

class GUIObject w => HasName w where
        name     :: String -> Config w
        getName  :: w -> IO String
        name n w  = cset w "name" n
        getName w = cget w "name"

-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class HtmlElem w => HtmlList w where
        compact :: Config w
        compact e = cset e "COMPACT" CVOID


-- --------------------------------------------------------------------------
-- Links
-- --------------------------------------------------------------------------

class HtmlElem w => HtmlHyperLink w where
        url :: String -> Config w 
        url u e = cset e "HREF" (RawData u)

class HtmlHyperLink w => HtmlRelation w where
        rel :: String -> Config w
        rev :: String -> Config w
        rel s e = cset e "REL" (RawData s)
        rev s e = cset e "REL" (RawData s)



-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

data Direction = LTR | RTL

class HtmlElem w => HtmlTableElem w where
        identifier      :: String -> Config w
        classnames      :: [String] -> Config w
        style           :: String -> Config w
        lang            :: String -> Config w
        direction       :: Direction -> Config w
        identifier id e = cset e "ID" (RawData id)
        classnames cl e = 
                cset e "CLASS" (RawData (showCNames cl))
                where           showCNames [] = ""
                                showCNames [x] = x
                                showCNames (x:l) = x ++ "," ++  showCNames l
        style s e = cset e "STYLE" (RawData s)
        lang l e = cset e "LANG" (RawData l)
        direction d e = cset e "DIR" d

class HtmlTableElem w => HasHorizontalAlign w where
        alignchar       :: Char -> Config w
        charoffset      :: Int -> Config w
        alignchar c e   = cset e "CHAR" (RawData [c])
        charoffset i e  = cset e "CHAROFF" i
        
class HtmlElem w => HasVerticalAlign w where
        valign          :: Alignment -> Config w
        valign a e      = cset e "VALIGN" a

class HtmlTableElem w => HtmlTableCell w where
        axis            :: String -> Config w
        axes            :: String -> Config w
        nowrap          :: Config w
        rowspan         :: Int -> Config w
        colspan         :: Int -> Config w
        axis s e        = cset e "AXIS" s
        axes s e        = cset e "AXES" s
        nowrap e        = cset e "NOWRAP" CVOID
        rowspan n e     = cset e "ROWSPAN" n
        colspan n e     = cset e "COLSPAN" n


class (HasSize w,HtmlTableElem w) => HtmlRowElem w where
        cspan           :: Int -> Config w
        cspan n e       = cset e "SPAN" n


-- --------------------------------------------------------------------------
-- Direction Values
-- --------------------------------------------------------------------------


instance GUIValue Direction where
        cdefault = LTR


instance Show Direction where
   showsPrec d p r = 
      (case p of 
        LTR -> "ltr"
        RTL -> "rtl"
      ) ++ r
        
instance Read Direction where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'t':'r':xs -> [(LTR,xs)]
        'r':'t':'l':xs -> [(RTL,xs)]
        _ -> []


-- --------------------------------------------------------------------------
-- Html Void Values
-- --------------------------------------------------------------------------

data CVOID = CVOID

instance GUIValue CVOID where
        cdefault = CVOID

instance Read CVOID where
   readsPrec p b = [(CVOID,b)]

instance Show CVOID where
   showsPrec d CVOID r = "" ++  r
