{- #########################################################################

MODULE        : HtmlFrame
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Frame Elements.

   ######################################################################### -}


module HtmlFrame (
        HtmlFrame,

        frame,
        noresize,
        scrolling,

        HtmlFrameSet,
        frameset,
        cols, 
        rows,

        HtmlNoFrames,
        noframes

        )

where

import HTk
import HtmlKernel
import HtmlLink
import Debug(debug)


-- --------------------------------------------------------------------------
-- Frame Element
-- --------------------------------------------------------------------------

newtype HtmlFrame = HtmlFrame HtmlElement

frame :: [Config HtmlFrame] -> WWW a -> WWW a
frame cf cmd = createHtmlComp (CElem Frame) cf cmd

instance GUIObject HtmlFrame  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "FRAME"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlFrame where 
        toHtmlElem (HtmlFrame e) = e
        fromHtmlElem = HtmlFrame

instance HtmlHyperLink HtmlFrame where 
        url u e = cset e "SRC" (RawData u)

instance HasName HtmlFrame where 
        name v e = cset e "NAME" (RawData v)
        getName e = do {
                (RawData str) <- cget e "NAME";
                return str
                }


instance HasSize HtmlFrame where 
        width w e = cset e "MARGINWIDTH" w
        height h e = cset e "MARGINHEIGHT" h

noresize :: Config HtmlFrame
noresize e= cset e "NORESIZE" CVOID

scrolling :: String -> Config HtmlFrame       {- no, yes, always -}
scrolling s e = cset e "SCROLLING" s


-- --------------------------------------------------------------------------
-- Frame Set Element
-- --------------------------------------------------------------------------

newtype HtmlFrameSet = HtmlFrameSet HtmlElement

frameset :: [Config HtmlFrameSet] -> WWW a -> WWW a
frameset cf cmd = createHtmlComp (CElem FrameSet) cf cmd

instance GUIObject HtmlFrameSet  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "FRAMESET"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlFrameSet where 
        toHtmlElem (HtmlFrameSet e) = e
        fromHtmlElem = HtmlFrameSet

cols, rows :: String -> Config HtmlFrameSet
cols v e = cset e "COLS" (RawData v)
rows v e = cset e "ROWS" (RawData v)



-- --------------------------------------------------------------------------
-- No Frames Element
-- --------------------------------------------------------------------------

newtype HtmlNoFrames = HtmlNoFrames HtmlElement

noframes :: [Config HtmlNoFrames] -> WWW a -> WWW a
noframes cf cmd = createHtmlComp (CElem NoFrames) cf cmd

instance GUIObject HtmlNoFrames  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "NOFRAMES"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlNoFrames where 
        toHtmlElem (HtmlNoFrames e) = e
        fromHtmlElem = HtmlNoFrames


