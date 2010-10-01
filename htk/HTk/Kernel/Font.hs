{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The @module Font@ export basic types and classes concerning
-- font resources.
module HTk.Kernel.Font (

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

import HTk.Kernel.GUIValue
import Data.Char
import Util.ExtendedPrelude(simpleSplit)

-- -----------------------------------------------------------------------
-- Font
-- -----------------------------------------------------------------------

-- | The general @Font@ datatype.
newtype Font = Font String

-- | The @XFont@ datatype - representing the elements of an
-- X font string.
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

-- | Datatypes that describe a font instantiate the
-- @class FontDesignator@.
class FontDesignator fh where
  toFont :: fh -> Font

-- | A @Font@ object itself represents a font.
instance FontDesignator Font where
  -- Internal.
  toFont = id

-- | An X font string represents a font.
instance FontDesignator String where
  -- Internal.
  toFont = Font

-- | An @XFont@ object (see type) represents a font.
instance FontDesignator XFont where
  -- Internal.
  toFont = Font . show

-- | A @FontFamily@ object describes a font (default values
-- set for other parameters).
instance FontDesignator FontFamily where
  -- Internal.
  toFont ch = toFont (xfont {family = Just ch})

-- | A tuple of @(FontFamily,Int)@ describes a font with
-- its font family and points.
instance FontDesignator (FontFamily,Int) where
  -- Internal.
  toFont (ch,s) = toFont (xfont {family = Just ch, points = (Just s)})

-- | A tuple of @(FontFamily,FontWeight,Int)@ describes a font
-- with its font family, font weight and points.
instance FontDesignator (FontFamily,FontWeight,Int) where
  -- Internal.
  toFont (ch, w, po) =
    toFont (xfont {family = Just ch, weight = Just w, points = (Just po)})

-- | A tuple of @(FontFamily,FontSlant,Int)@ describes a font
-- with its font family, font slant and points.
instance FontDesignator (FontFamily,FontSlant,Int) where
  -- Internal.
  toFont (ch, sl, po) =
    toFont (xfont {family = Just ch, slant = Just sl, points = (Just po)})


-- -----------------------------------------------------------------------
-- X Font Construction
-- -----------------------------------------------------------------------

-- | Standard font.
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

-- | Internal.
instance GUIValue Font where
  -- Internal.
  cdefault = toFont xfont

-- | Internal.
instance Show Font where
   -- Internal.
   showsPrec d (Font c) r = c ++ r

-- | Internal.
instance Read Font where
   -- Internal.
   readsPrec p str = [(Font str,[])]


-- -----------------------------------------------------------------------
-- XFont Instantations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIValue XFont where
  -- Internal.
  cdefault = read "-Adobe-Helvetica-Normal-R-Normal-*-*-120-*-*-*-*-*-*"

-- | Internal.
instance Show XFont where
   -- Internal.
   showsPrec d c r = cshow c ++ r
     where
        cshow (XFont fo fa we sl sw pi po xr yr sp cw cs) =
               hy ++ fo ++ hy ++ mshow fa ++ hy ++ mshow we ++ hy ++
               mshow sl ++ hy ++ mshow sw ++ hy ++ mshow pi ++ hy ++
               mshow po ++ hy ++ mshow xr ++ hy ++ mshow yr ++ hy ++
               mshow sp ++ hy ++ mshow cw ++ hy ++ mshow cs ++ hy ++ "*"
               where hy = "-"
        cshow (XFontAlias str) = str

-- | Internal.
instance Read XFont where
   -- Internal.
   readsPrec p str = [(cread (dropWhile isSpace str),[])]
     where
        cread s@('-':str) = toXFont (simpleSplit (== '-') str)
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

-- | The @FontWeight@ datatype.
data FontWeight = NormalWeight | Medium | Bold

-- | Internal.
instance Read FontWeight where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'n':'o':'r':'m':'a':'l':xs -> [(NormalWeight,xs)]
        'm':'e':'d':'i':'u':'m':xs -> [(Medium,xs)]
        'b':'o':'l':'d':xs -> [(Bold,xs)]
        _ -> []

-- | Internal.
instance Show FontWeight where
   -- Internal.
   showsPrec d p r =
      (case p of
        NormalWeight -> "Normal"
        Medium -> "Medium"
        Bold -> "Bold"
        ) ++ r

-- | Internal.
instance GUIValue FontWeight where
  -- Internal.
  cdefault = NormalWeight


-- -----------------------------------------------------------------------
--  FontFamily
-- -----------------------------------------------------------------------

-- | The @FontFamily@ datatype.
data FontFamily =
    Lucida
  | Times
  | Helvetica
  | Courier
  | Symbol
  | Other String

-- | Internal.
instance Read FontFamily where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'l':'u':'c':'i':'d':'a':xs -> [(Lucida,xs)]
        't':'i':'m':'e':'s':xs -> [(Times,xs)]
        'h':'e':'l':'v':'e':'t':'i':'c':'a':xs -> [(Helvetica,xs)]
        'c':'o':'u':'r':'i':'e':'r':xs -> [(Courier,xs)]
        's':'y':'m':'b':'o':'l':xs -> [(Symbol,xs)]
        fstr -> [(Other fstr, [])]

-- | Internal.
instance Show FontFamily where
   -- Internal.
   showsPrec d p r =
      (case p of
        Lucida -> "Lucida"
        Times -> "Times"
        Helvetica -> "Helvetica"
        Courier -> "Courier"
        Symbol -> "Symbol"
        Other fstr -> fstr
        ) ++ r

-- | Internal.
instance GUIValue FontFamily where
  -- Internal.
  cdefault = Courier


-- -----------------------------------------------------------------------
-- FontSlant
-- -----------------------------------------------------------------------

-- | The @FontSlant@ datatype.
data FontSlant = Roman | Italic | Oblique

-- | Internal.
instance Read FontSlant where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'r':xs -> [(Roman,xs)]
        'i':xs -> [(Italic,xs)]
        'o':xs -> [(Oblique,xs)]
        _ -> []

-- | Internal.
instance Show FontSlant where
   -- Internal.
   showsPrec d p r =
      (case p of
        Roman -> "R"
        Italic -> "I"
        Oblique -> "O"
        ) ++ r

-- | Internal.
instance GUIValue FontSlant where
  -- Internal.
  cdefault = Roman


-- -----------------------------------------------------------------------
-- FontWidth
-- -----------------------------------------------------------------------

-- | The @FontWidth@ datatype.
data FontWidth = NormalWidth | Condensed | Narrow

-- | Internal.
instance Read FontWidth where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'n':'o':'r':'m':'a':'l':xs -> [(NormalWidth,xs)]
        'c':'o':'n':'d':'e':'n':'s':'e':'d':xs -> [(Condensed,xs)]
        'n':'a':'r':'r':'o':'w':xs -> [(Narrow,xs)]
        _ -> []

-- | Internal.
instance Show FontWidth where
   -- Internal.
   showsPrec d p r =
      (case p of
        NormalWidth -> "Normal"
        Condensed -> "Condensed"
        Narrow -> "Narrow"
        ) ++ r

-- | Internal.
instance GUIValue FontWidth where
  -- Internal.
  cdefault = NormalWidth


-- -----------------------------------------------------------------------
-- FontSpacing
-- -----------------------------------------------------------------------

-- | The @FontSpacing@ datatype.
data FontSpacing = MonoSpace | Proportional

-- | Internal.
instance Read FontSpacing where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) (map toLower b) of
        'm':xs -> [(MonoSpace,xs)]
        'p':xs -> [(Proportional,xs)]
        _ -> []

-- | Internal.
instance Show FontSpacing where
   -- Internal.
   showsPrec d p r =
      (case p of
        MonoSpace -> "M"
        Proportional -> "P"
        ) ++ r

-- | Internal.
instance GUIValue FontSpacing where
  -- Internal.
  cdefault =  MonoSpace
