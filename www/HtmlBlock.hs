{- #########################################################################

MODULE        : HtmlBlock
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Block Elements.

   ######################################################################### -}


module HtmlBlock (

        Colour,
        Justify(..),
        Distance,

        HtmlElem(..),
        HasColour(..),
        HasJustify(..),

        HtmlBody,
        body,
        linkcolor, 
        vlinkcolor, 
        alinkcolor,
        backgroundimage, 

        pre,

        address, 

        HtmlDiv,
        division,
     
        blockquote,
        
        HtmlH,
        heading,

        HtmlBr,
        Clear(..),
        br,
        breakline,
        clear,

        HtmlHr,
        ruler,
        hr,
        noshade,

        HtmlPar,
        paragraph,
        par


        )

where

import HTk
import HtmlKernel
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
-- Body
-- --------------------------------------------------------------------------

newtype HtmlBody = HtmlBody HtmlElement

body :: [Config HtmlBody] -> WWW a -> WWW a
body cnfs cmd = createHtmlComp (CElem Body) cnfs cmd

instance GUIObject HtmlBody  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "Body"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlBody where 
        toHtmlElem (HtmlBody e) = e
        fromHtmlElem = HtmlBody

instance HasColour HtmlBody where 
        legalColourID = hasForeGroundColour
        setColour e "foreground" c = cset e "TEXT" (toColour c)
        setColour e "background" c = cset e "BGCOLOR" (toColour c)
        setColour e _ _ = return e

linkcolor :: ColourDesignator c => c -> Config HtmlBody
linkcolor c e = cset e "LINK" (toColour c)

vlinkcolor :: ColourDesignator c => c  -> Config HtmlBody
vlinkcolor c e = cset e "VLINK" (toColour c)

alinkcolor :: ColourDesignator c => c -> Config HtmlBody
alinkcolor c e = cset e "ALINK" (toColour c)

backgroundimage :: String -> Config HtmlBody
backgroundimage url e = cset e "BACKGROUND" (RawData url)


-- --------------------------------------------------------------------------
-- Block Structuring Elements
-- --------------------------------------------------------------------------

pre :: WWW a -> WWW a  
pre cmd = createHtmlComp (Block Pre) ([]:: [Config HtmlDiv]) cmd


-- --------------------------------------------------------------------------
-- Block Structuring Elements
-- --------------------------------------------------------------------------

address :: WWW a -> WWW a
address cmd = createHtmlComp (Block Address) ([]:: [Config HtmlDiv]) cmd

-- --------------------------------------------------------------------------
-- Division Element
-- --------------------------------------------------------------------------

newtype HtmlDiv = HtmlDiv HtmlElement

division :: [Config HtmlDiv] -> WWW a -> WWW a
division cnfs cmd = createHtmlComp (Block Div) cnfs cmd 

instance GUIObject HtmlDiv  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "DIV"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlDiv where 
        toHtmlElem (HtmlDiv e) = e
        fromHtmlElem = HtmlDiv

instance HasJustify HtmlDiv where {justify j e = cset e "ALIGN" j}


-- --------------------------------------------------------------------------
-- Blockquote
-- --------------------------------------------------------------------------

blockquote :: WWW a -> WWW a
blockquote cmd = createHtmlComp (Block Quote) ([]:: [Config HtmlDiv]) cmd


-- --------------------------------------------------------------------------
-- Heading
-- --------------------------------------------------------------------------

newtype HtmlH = HtmlH HtmlElement

heading :: Int -> [Config HtmlH] -> WWW a -> WWW a
heading n cnfs cmd = createHtmlComp (Block (level n)) cnfs cmd
        where level 1 = H1
              level 2 = H2
              level 3 = H3
              level 4 = H4
              level 5 = H5
              level 6 = H6
              level _ = H6


instance GUIObject HtmlH where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "H"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlH where 
        toHtmlElem (HtmlH e) = e
        fromHtmlElem = HtmlH
instance HasJustify HtmlH where {justify j e = cset e "ALIGN" j}


-- --------------------------------------------------------------------------
-- Paragraph Element
-- --------------------------------------------------------------------------

data HtmlPar = HtmlPar HtmlElement

paragraph :: [Config HtmlPar] -> WWW ()
paragraph cnfs = createHtmlElem (AElem Par) cnfs
                  
par :: WWW ()
par = paragraph []

instance GUIObject HtmlPar where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "PAR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr      

instance HtmlElem HtmlPar where 
        toHtmlElem (HtmlPar e) = e
        fromHtmlElem = HtmlPar

instance HasJustify HtmlPar where {justify j e = cset e "ALIGN" j}


-- --------------------------------------------------------------------------
-- Break Line Element
-- --------------------------------------------------------------------------

newtype HtmlBr = HtmlBr HtmlElement

breakline :: [Config HtmlBr] -> WWW ()
breakline cnfs = createHtmlElem (AElem Br) cnfs

br :: WWW ()
br = breakline []

instance GUIObject HtmlBr where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "BR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlBr where 
        toHtmlElem (HtmlBr e) = e
        fromHtmlElem = HtmlBr

clear :: Clear -> Config HtmlBr
clear cl e = cset e "CLEAR" cl


-- --------------------------------------------------------------------------
-- HR Element
-- --------------------------------------------------------------------------

newtype HtmlHr = HtmlHr HtmlElement

ruler :: [Config HtmlHr] -> WWW ()
ruler cnfs =  createHtmlElem (AElem Hr) cnfs

hr :: WWW ()
hr = ruler []

instance GUIObject HtmlHr where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "HR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlHr where 
        toHtmlElem (HtmlHr e) = e
        fromHtmlElem = HtmlHr

instance HasJustify HtmlHr where {justify j e = cset e "ALIGN" j}

instance HasSize HtmlHr where
        width w e = cset e "WIDTH" w
        height h e = return e

instance HasThickness HtmlHr

noshade :: Config HtmlHr
noshade e = cset e "NOSHADE" CVOID



-- --------------------------------------------------------------------------
-- HClear
-- --------------------------------------------------------------------------

data Clear = CLeft | CAll | CRight | CNone

instance GUIValue Clear where
        cdefault = CLeft

instance Show Clear where
   showsPrec d p r = 
      (case p of 
        CLeft -> "left"
        CAll -> "all"
        CRight -> "right"
        CNone -> "none"
      ) ++ r

        
instance Read Clear where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'l':'e':'f':'t':xs -> [(CLeft,xs)]
        'a':'l':'l': xs -> [(CAll,xs)]
        'r':'i':'g':'h':'t':xs -> [(CRight,xs)]
        'n':'o':'n':'e':xs -> [(CNone,xs)]
        _ -> []

