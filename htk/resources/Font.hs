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

module Font (
        FontDesignator(..),

        Font(..),       
        XFont(..),

        xfont,

        FontFamily(..),
        FontWeight(..),
        FontSlant(..),
        FontWidth(..),
        FontSpacing(..)
        ) where

import GUIValue
import Char
import ExtendedPrelude(split)
import Debug(debug)
import IOExts(unsafePerformIO)


-- -----------------------------------------------------------------------
-- Font
-- -----------------------------------------------------------------------

newtype Font  = Font String

data XFont =
    XFont { foundry   :: String,
            family    :: Maybe FontFamily,
            weight    :: Maybe FontWeight,
            slant     :: Maybe FontSlant,
            fontwidth :: Maybe FontWidth,
            pixels    :: (Maybe Int),
            points    :: (Maybe Int),
            xres      :: (Maybe Int),
            yres      :: (Maybe Int),
            spacing   :: Maybe FontSpacing,
            charwidth :: (Maybe Int),
            charset   :: Maybe String }
  | XFontAlias String


-- -----------------------------------------------------------------------
-- Font
-- -----------------------------------------------------------------------

class FontDesignator fh where
        toFont :: fh -> Font

instance FontDesignator Font where
        toFont = id

instance FontDesignator String where
        toFont = Font

instance FontDesignator XFont where
        toFont = Font . show

instance FontDesignator FontFamily where
        toFont ch = toFont (xfont {family = Just ch})

instance FontDesignator (FontFamily,Int) where
        toFont (ch,s) = toFont (xfont {family = Just ch, points = (Just s)})

instance FontDesignator (FontFamily,FontWeight,Int) where
        toFont (ch,w,po) = toFont (xfont {family = Just ch, weight = Just w, points = (Just po)})

instance FontDesignator (FontFamily,FontSlant,Int) where
        toFont (ch,sl,po) = toFont (xfont {family = Just ch, slant = Just sl, points = (Just po)})


-- -----------------------------------------------------------------------
-- X Font Construction
-- -----------------------------------------------------------------------

xfont :: XFont
xfont = XFont {
                foundry = "Adobe",
                family = Just Helvetica,
                weight = Just NormalWeight,
                slant =  Nothing,
                fontwidth = Just NormalWidth,
                pixels = Nothing,
                points = Just 120,
                xres = Nothing,
                yres = Nothing,
                spacing = Nothing,
                charwidth = Nothing,
                charset = Nothing
                }


-- -----------------------------------------------------------------------
-- Font Instantations
-- -----------------------------------------------------------------------

instance GUIValue Font where
        cdefault = toFont xfont

instance Show Font where
   showsPrec d (Font c) r = c ++ r

instance Read Font where
   readsPrec p str = [(Font str,[])] 


-- -----------------------------------------------------------------------
-- XFont Instantations
-- -----------------------------------------------------------------------

instance GUIValue XFont where
        cdefault = read "-Adobe-Helvetica-Normal-R-Normal-*-*-120-*-*-*-*-*-*"


instance Show XFont where
   showsPrec d c r = cshow c ++ r
     where
        cshow (XFont fo fa we sl sw pi po xr yr sp cw cs) = 
               hy ++ fo ++ hy ++ mshow fa ++ hy ++ mshow we ++ hy ++ 
               mshow sl ++ hy ++ mshow sw ++ hy ++ mshow pi ++ hy ++ 
               mshow po ++ hy ++ mshow xr ++ hy ++ mshow yr ++ hy ++ 
               mshow sp ++ hy ++ mshow cw ++ hy ++ mshow cs ++ hy ++ "*"
               where hy = "-"
        cshow (XFontAlias str) = str

instance Read XFont where
   readsPrec p str = [(cread (dropWhile isSpace str),[])] 
     where
        cread s@('-':str) = toXFont (split (== '-') str)
        cread str = XFontAlias str
        toXFont (fo : fa : we : sl : sw : pi : po : xr : yr : sp : cw : cs : y : _) =
                XFont fo (mread fa) (mread we) (mread sl) (mread sw)
                        (mread pi) (mread po) (mread xr) (mread yr)
                        (mread sp) (mread cw) (mread cs)


mshow :: Show a => Maybe a -> String
mshow Nothing = "*"
mshow (Just a) = show a

mread :: Read a => String -> Maybe a
mread "*" = Nothing
mread str = Just (read str)


-- -----------------------------------------------------------------------
-- FontWeight
-- -----------------------------------------------------------------------

data FontWeight = NormalWeight | Medium | Bold

instance Read FontWeight where
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'n':'o':'r':'m':'a':'l':xs -> [(NormalWeight,xs)]
        'm':'e':'d':'i':'u':'m':xs -> [(Medium,xs)]
        'b':'o':'l':'d':xs -> [(Bold,xs)]
        _ -> []

instance Show FontWeight where
   showsPrec d p r = 
      (case p of 
        NormalWeight -> "Normal" 
        Medium -> "Medium"
        Bold -> "Bold"
        ) ++ r

instance GUIValue FontWeight where
        cdefault = NormalWeight



-- -----------------------------------------------------------------------
--  FontFamily
-- -----------------------------------------------------------------------

data FontFamily =
    Lucida
  | Times
  | Helvetica
  | Courier
  | Symbol
  | Other String

instance Read FontFamily where
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'l':'u':'c':'i':'d':'a':xs -> [(Lucida,xs)]
        't':'i':'m':'e':'s':xs -> [(Times,xs)]
        'h':'e':'l':'v':'e':'t':'i':'c':'a':xs -> [(Helvetica,xs)]
        'c':'o':'u':'r':'i':'e':'r':xs -> [(Courier,xs)]
        's':'y':'m':'b':'o':'l':xs -> [(Symbol,xs)]
        fstr -> [(Other fstr, [])]

instance Show FontFamily where
   showsPrec d p r = 
      (case p of 
        Lucida -> "Lucida" 
        Times -> "Times"
        Helvetica -> "Helvetica"
        Courier -> "Courier"
	Symbol -> "Symbol"
        Other fstr -> fstr
        ) ++ r

instance GUIValue FontFamily where
        cdefault = Courier


-- -----------------------------------------------------------------------
-- FontSlant
-- -----------------------------------------------------------------------

data FontSlant = Roman | Italic | Oblique 

instance Read FontSlant where
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'r':xs -> [(Roman,xs)]
        'i':xs -> [(Italic,xs)]
        'o':xs -> [(Oblique,xs)]
        _ -> []

instance Show FontSlant where
   showsPrec d p r = 
      (case p of 
        Roman -> "R" 
        Italic -> "I"
        Oblique -> "O"
        ) ++ r

instance GUIValue FontSlant where
        cdefault = Roman


-- -----------------------------------------------------------------------
-- FontWidth
-- -----------------------------------------------------------------------

data FontWidth = NormalWidth | Condensed | Narrow

instance Read FontWidth where
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'n':'o':'r':'m':'a':'l':xs -> [(NormalWidth,xs)]
        'c':'o':'n':'d':'e':'n':'s':'e':'d':xs -> [(Condensed,xs)]
        'n':'a':'r':'r':'o':'w':xs -> [(Narrow,xs)]
        _ -> []

instance Show FontWidth where
   showsPrec d p r = 
      (case p of 
        NormalWidth -> "Normal" 
        Condensed -> "Condensed"
        Narrow -> "Narrow"
        ) ++ r

instance GUIValue FontWidth where
        cdefault = NormalWidth


-- --------------------------------------------------------------------------
--  FontSpacing 
-- --------------------------------------------------------------------------

data FontSpacing = MonoSpace | Proportional

instance Read FontSpacing where
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'm':xs -> [(MonoSpace,xs)]
        'p':xs -> [(Proportional,xs)]
        _ -> []

instance Show FontSpacing where
   showsPrec d p r = 
      (case p of 
        MonoSpace -> "M" 
        Proportional -> "P"
        ) ++ r

instance GUIValue FontSpacing where
        cdefault =  MonoSpace
