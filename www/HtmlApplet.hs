{- #########################################################################

MODULE        : HtmlApplet
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Applet Elements.

   ######################################################################### -}


module HtmlApplet (
        
        Distance,
        Alignment(..),

        HtmlApplet,
        applet,
        codefile,

        HtmlParam,
        param
        )

where

import HTk
import HtmlKernel
import HtmlLink
import Debug(debug)


-- --------------------------------------------------------------------------
-- Applet Element
-- --------------------------------------------------------------------------

newtype HtmlApplet = HtmlApplet HtmlElement

applet :: [Config HtmlApplet] -> WWW a -> WWW a
applet cf cmd = createHtmlComp (CElem Applet) cf cmd

instance GUIObject HtmlApplet  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "APPLET"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlApplet where 
        toHtmlElem (HtmlApplet e) = e
        fromHtmlElem = HtmlApplet

instance HtmlHyperLink HtmlApplet where 
        url u e = cset e "CODEBASE" (RawData u)

instance HasName HtmlApplet where 
        name v e = cset e "NAME" (RawData v)
        getName e = do {
                (RawData str) <- cget e "NAME";
                return str
                }

instance HasSize HtmlApplet where 
        width w e = cset e "WIDTH" w
        height h e = cset e "HEIGHT" h

instance HasAlign HtmlApplet

instance HasAlt HtmlApplet

instance HasSpace HtmlApplet

codefile :: FilePath -> Config HtmlApplet
codefile fname e= cset e "CODE" (RawData fname)


-- --------------------------------------------------------------------------
-- Param Element
-- --------------------------------------------------------------------------

newtype HtmlParam = HtmlParam HtmlElement

param :: [Config HtmlParam] -> WWW ()
param cf = createHtmlElem (AElem Param) cf

instance GUIObject HtmlParam  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "PARAM"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlParam where 
        toHtmlElem (HtmlParam e) = e
        fromHtmlElem = HtmlParam

instance HasName HtmlParam where 
        name v e = cset e "NAME" (RawData v)
        getName e = do {
                (RawData str) <- cget e "NAME";
                return str
                }


instance HasHtmlValue HtmlParam

