{- #########################################################################

MODULE        : HtmlRef
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Link Elements.

   ######################################################################### -}


module HtmlLink (

        Distance,

        GUIObject(..),
        HtmlElem(..),
        HtmlHyperLink(..),
        HtmlRelation(..),

        HtmlImg,
        img,
        ismap,

        HtmlAnc,
        anc,

        HtmlRef,
        href,

        HtmlImageMap,
        imagemap,

        HtmlArea,
        Shape(..),
        area,
        nohref,
        coords,
        shape,

        HtmlBGSound,
        bgsound,
        timesToPlay

        )

where

import HTk
import HtmlKernel
import HtmlTypes
import Char
import Debug(debug)



-- --------------------------------------------------------------------------
-- Image Element
-- --------------------------------------------------------------------------

newtype HtmlImg = HtmlImg HtmlElement

img :: [Config HtmlImg] -> WWW ()
img cnfs = createHtmlElem (AElem Img) cnfs

instance GUIObject HtmlImg where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "IMG"
        cset w cid v = setHtmlAttr w cid v 
        cget = getHtmlAttr

instance HtmlElem HtmlImg where 
        toHtmlElem (HtmlImg e) = e
        fromHtmlElem = HtmlImg

instance HtmlHyperLink HtmlImg where 
        url r e = cset e "SRC" (RawData r)

instance HasAlign HtmlImg 

instance HasAlt HtmlImg 

instance HasSize HtmlImg where
        width w e = cset e "WIDTH" w
        height h e = cset e "HEIGHT" h

instance HasBorder HtmlImg

instance HasSpace HtmlImg

usemap :: String -> Config HtmlImg
usemap u e = cset e "USEMAP" (RawData u)

ismap :: Config HtmlImg
ismap e = cset e "ISMAP" CVOID


-- --------------------------------------------------------------------------
-- Anchor Element
-- --------------------------------------------------------------------------

newtype HtmlAnc = HtmlAnc HtmlElement

anc :: [Config HtmlAnc] -> WWW a -> WWW a       
anc cnfs cmd = createHtmlComp (CElem Anc) cnfs cmd

instance GUIObject HtmlAnc where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "ANC"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlAnc where 
        toHtmlElem (HtmlAnc e) = e
        fromHtmlElem = HtmlAnc

instance HasName HtmlAnc where 
        name str e = cset e "NAME" (RawData str)
        getName e = do {
                (RawData str) <- cget e "NAME";
                return str
                }

instance HasText HtmlAnc String where 
        text str e = cset e "TITLE" (RawData str)
        getText e  = cget e "TITLE"


-- --------------------------------------------------------------------------
-- Reference Element
-- --------------------------------------------------------------------------

data HtmlRef = HtmlRef HtmlElement

href :: [Config HtmlRef] -> WWW a -> WWW a      
href cnfs cmd = createHtmlComp (CElem Ref) cnfs cmd

instance GUIObject HtmlRef where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "A"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlRef where 
        toHtmlElem (HtmlRef e) = e
        fromHtmlElem = HtmlRef

instance HtmlHyperLink HtmlRef

instance HtmlRelation HtmlRef


-- --------------------------------------------------------------------------
-- Map Element
-- --------------------------------------------------------------------------

data HtmlImageMap = HtmlImageMap HtmlElement

imagemap :: [Config HtmlImageMap] -> WWW a -> WWW a             -- map
imagemap cnfs cmd = createHtmlComp  (CElem HtmlMap) cnfs cmd

instance GUIObject HtmlImageMap where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "MAP"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlImageMap where 
        toHtmlElem (HtmlImageMap e) = e
        fromHtmlElem = HtmlImageMap

instance HasName HtmlImageMap where 
        name nm e = cset e "NAME" (RawData nm)
        getName e = do {
                (RawData nm) <- cget e "NAME";
                return nm
                }


-- --------------------------------------------------------------------------
-- Area Element
-- --------------------------------------------------------------------------

data HtmlArea = HtmlArea HtmlElement

area :: [Config HtmlArea] -> WWW ()             -- area
area cnfs = createHtmlElem (AElem Area) cnfs

instance GUIObject HtmlArea where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "AREA"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlArea where 
        toHtmlElem (HtmlArea e) = e
        fromHtmlElem = HtmlArea

instance HtmlHyperLink HtmlArea

instance HasAlt HtmlArea

nohref :: Config HtmlArea
nohref e = cset e "NOHREF" CVOID

coords :: String -> Config HtmlArea
coords cl e = cset e "COORDS" (RawData cl)

shape :: Shape -> Config HtmlArea 
shape s e = cset e "SHAPE" s


-- --------------------------------------------------------------------------
-- Background Audio
-- --------------------------------------------------------------------------

data HtmlBGSound = HtmlBGSound HtmlElement

bgsound :: [Config HtmlBGSound] -> WWW ()
bgsound cnfs = createHtmlElem (AElem BGSound) cnfs

instance GUIObject HtmlBGSound where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "BGSOUND"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlBGSound where 
        toHtmlElem (HtmlBGSound e) = e
        fromHtmlElem = HtmlBGSound

instance HtmlHyperLink HtmlBGSound

timesToPlay :: Maybe Int -> Config HtmlBGSound
timesToPlay Nothing e = cset e "LOOP" "INFINITE"
timesToPlay (Just n) e = cset e "LOOP" n


-- --------------------------------------------------------------------------
-- Shape
-- --------------------------------------------------------------------------


data Shape = RectShape | CircleShape | PolyShape | DefaultShape

instance GUIValue Shape where
        cdefault = DefaultShape


instance Show Shape where
   showsPrec d p r = 
      (case p of 
        RectShape -> "rect"
        CircleShape -> "circle"
        PolyShape -> "poly"
        DefaultShape -> "default"
      ) ++ r
        
instance Read Shape where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'r':'e':'c':'t':xs -> [(RectShape,xs)]
        'c':'i':'r':'c':'l':'e':xs -> [(CircleShape,xs)]
        'p':'o':'l':'y':xs -> [(PolyShape,xs)]
        'd':'e':'f':'a':'u':'l':'t':xs -> [(DefaultShape,xs)]
        _ -> []

