{- #########################################################################

MODULE        : HtmlMeta
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Meta Elements.

   ######################################################################### -}


module HtmlMeta (

        Distance,

        prologue,

        header,

        doctitle,

        HtmlBase,
        base,

        isindex,

        HtmlLink,
        link,

        HtmlMeta,
        meta,
        httpequiv,
        htmlcontent,

        nextid

        )

where

import HTk
import HtmlKernel
import HtmlLink
import Debug(debug)


-- --------------------------------------------------------------------------
-- Prologue
-- --------------------------------------------------------------------------

prologue :: String -> WWW ()
prologue s = addHtmlElem (Prologue s)


-- --------------------------------------------------------------------------
-- Header
-- --------------------------------------------------------------------------

header :: WWW a -> WWW a
header cmd = createHtmlComp (CElem Head) ([] :: [Config HtmlBase]) cmd
                                        -- dummy type


-- --------------------------------------------------------------------------
-- Title
-- --------------------------------------------------------------------------

doctitle :: String -> WWW ()
doctitle t = addHtmlElem (Title t)


-- --------------------------------------------------------------------------
-- Base
-- --------------------------------------------------------------------------

newtype HtmlBase = HtmlBase HtmlElement

base :: [Config HtmlBase] -> WWW ()
base cnfs = createHtmlElem (AElem Base) cnfs

instance GUIObject HtmlBase where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "BASE"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlBase where 
         toHtmlElem(HtmlBase e) = e
         fromHtmlElem = HtmlBase

instance HtmlHyperLink HtmlBase


-- --------------------------------------------------------------------------
-- IsIndex Element
-- --------------------------------------------------------------------------

isindex :: WWW ()
isindex = addHtmlElem IsIndex


-- --------------------------------------------------------------------------
-- Link Element
-- --------------------------------------------------------------------------

newtype HtmlLink = HtmlLink HtmlElement

link :: [Config HtmlLink] -> WWW ()
link cnfs =createHtmlElem (AElem Link) cnfs

instance GUIObject HtmlLink where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "LINK"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HasText HtmlLink String where 
        text str e = cset e "TITLE" (RawData str)
        getText e = do {
                (RawData str) <- cget e "TITLE";
                return str
                }

instance HtmlElem HtmlLink where 
        toHtmlElem (HtmlLink e) = e
        fromHtmlElem = HtmlLink

instance HtmlHyperLink HtmlLink

instance HtmlRelation HtmlLink


-- --------------------------------------------------------------------------
-- Meta Element
-- --------------------------------------------------------------------------

newtype HtmlMeta = HtmlMeta HtmlElement

meta :: [Config HtmlMeta] -> WWW ()
meta cnfs = createHtmlElem (AElem MetaElem) cnfs

instance GUIObject HtmlMeta where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "META"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlMeta where 
        toHtmlElem (HtmlMeta e) = e
        fromHtmlElem = HtmlMeta

instance HasName HtmlMeta where 
        name str e = cset e "NAME" (RawData str)
        getName e = do {
                (RawData str) <- cget e "NAME";
                return str
                }

httpequiv :: String -> Config HtmlMeta
httpequiv str e = cset e "HTTPEQUIV" (RawData str)

htmlcontent :: String -> Config HtmlMeta
htmlcontent str e = cset e "CONTENT" (RawData str)


-- --------------------------------------------------------------------------
-- NextId Element
-- --------------------------------------------------------------------------

nextid  :: String -> WWW ()
nextid s = addHtmlElem (NextId s)

