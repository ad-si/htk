{- #########################################################################

MODULE        : HtmlText
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Text Elements.

   ######################################################################### -}


module HtmlText (

        Colour,

        comment ,

        Style(..),
        plain,
        sty,
        format,

        HtmlFont,
        hfont,

        HtmlBaseFont,
        basefont,

        HtmlNoBr,
        nobr,

        HtmlWBR,
        wbr
        )

where

import HTk
import HtmlKernel
import Debug(debug)


-- --------------------------------------------------------------------------
-- Comment Element
-- --------------------------------------------------------------------------

comment :: String -> WWW ()
comment t = addHtmlElem (Comment t)


-- --------------------------------------------------------------------------
-- Html Text Element
-- --------------------------------------------------------------------------

plain :: String -> WWW ()
plain t  = addHtmlElem (Text t)


-- --------------------------------------------------------------------------
-- Phrase Elements
-- --------------------------------------------------------------------------

sty     :: Style -> WWW a -> WWW a
sty s cmd = createHtmlComp (Format s) ([] :: [Config HtmlFont]) cmd


format :: Style -> String -> WWW ()
format p s = sty p (plain s)


-- --------------------------------------------------------------------------
-- Font Element
-- --------------------------------------------------------------------------

newtype HtmlFont = HtmlFont HtmlElement

hfont :: [Config HtmlFont] -> WWW a -> WWW a
hfont cnfs cmd = createHtmlComp (CElem HTMLFont) cnfs cmd

instance GUIObject HtmlFont where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "FONT"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlFont where 
        toHtmlElem (HtmlFont e) = e
        fromHtmlElem = HtmlFont

instance HasColour HtmlFont where
        legalColourID = hasBackGroundColour
        setColour e _ c = cset e "COLOR" (toColour c)

instance HasFontSize HtmlFont


-- --------------------------------------------------------------------------
-- Font Element
-- --------------------------------------------------------------------------

newtype HtmlBaseFont = HtmlBaseFont HtmlElement

basefont :: [Config HtmlBaseFont] -> WWW ()
basefont cnfs = createHtmlElem (AElem BaseFont) cnfs

instance GUIObject HtmlBaseFont where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "BASEFONT"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlBaseFont where 
        toHtmlElem (HtmlBaseFont e) = e
        fromHtmlElem = HtmlBaseFont

instance HasFontSize HtmlBaseFont



-- --------------------------------------------------------------------------
-- No Break Element
-- --------------------------------------------------------------------------

newtype HtmlNoBr = HtmlNoBr HtmlElement

nobr :: [Config HtmlNoBr] -> WWW a -> WWW a
nobr cnfs cmd = createHtmlComp (CElem NoBr) cnfs cmd

instance GUIObject HtmlNoBr where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "NOBR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlNoBr where 
        toHtmlElem (HtmlNoBr e) = e
        fromHtmlElem = HtmlNoBr



-- --------------------------------------------------------------------------
-- Potential Work Break Element
-- --------------------------------------------------------------------------

newtype HtmlWBR = HtmlWBR HtmlElement

wbr :: [Config HtmlWBR] -> WWW ()
wbr cnfs = createHtmlElem (AElem WBr) cnfs

instance GUIObject HtmlWBR where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "WBR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlWBR where 
        toHtmlElem (HtmlWBR e) = e
        fromHtmlElem = HtmlWBR


