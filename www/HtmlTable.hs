{- #########################################################################

MODULE        : HtmlTable
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Table Elements.

   ######################################################################### -}


module HtmlTable (
        
        Distance,
        Justify(..),

        Direction(..),
        HtmlTableElem(..),
        HtmlTableCell(..),
        HasHorizontalAlign(..),
        HasVerticalAlign(..),

        HtmlTable,
        table,
        cellspacing,
        cellpadding,
        rules,
        frames,
        tableborder,
        TableFrame(..),
        TableRules(..),

        HtmlCaption,
        caption,

        HtmlTh,
        th,

        HtmlTd,
        emptycell,
        td,

        HtmlTr,
        tr
        )

where

import HTk
import HtmlKernel
import HtmlLink
import HtmlText
import Char
import Debug(debug)

-- --------------------------------------------------------------------------
-- Table Element
-- --------------------------------------------------------------------------

newtype HtmlTable = HtmlTable HtmlElement

table :: [Config HtmlTable] -> WWW a -> WWW a
table cf cmd = createHtmlComp (CElem Table) cf cmd

instance GUIObject HtmlTable  where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "TABLE"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlTable where 
        toHtmlElem (HtmlTable e) = e
        fromHtmlElem = HtmlTable

instance HtmlTableElem HtmlTable

instance HasSize HtmlTable where 
        width w e = cset e "WIDTH" w
        height h e = return e

instance HasJustify HtmlTable where {justify j e = cset e "ALIGN" j}

instance HasVerticalAlign HtmlTable

cellspacing :: Distance -> Config HtmlTable
cellspacing d e = cset e "CELLSPACING" d

cellpadding :: Distance -> Config HtmlTable
cellpadding d e = cset e "CELLPADDING" d

rules :: TableRules -> Config HtmlTable
rules r e = cset e "RULES" r

frames :: TableFrame -> Config HtmlTable
frames f e = cset e "FRAME" f

tableborder :: Config HtmlTable
tableborder e = cset e "BORDER" CVOID


-- --------------------------------------------------------------------------
-- Caption Element
-- --------------------------------------------------------------------------

newtype HtmlCaption = HtmlCaption HtmlElement

caption :: [Config HtmlCaption] -> WWW a -> WWW a
caption cf cmd = createHtmlComp (CElem Caption) cf cmd

instance GUIObject HtmlCaption where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "CAPTION"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlCaption where 
        toHtmlElem (HtmlCaption e) = e
        fromHtmlElem = HtmlCaption

instance HasJustify HtmlCaption where {justify j e = cset e "ALIGN" j}

instance HasVerticalAlign HtmlCaption

instance HtmlTableElem HtmlCaption


-- --------------------------------------------------------------------------
-- TH Element
-- --------------------------------------------------------------------------

newtype HtmlTh = HtmlTh HtmlElement

th :: [Config HtmlTh] -> WWW ()
th cf = createHtmlElem (AElem Th) cf

instance GUIObject HtmlTh where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "TH"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlTh where 
        toHtmlElem (HtmlTh e) = e
        fromHtmlElem = HtmlTh

instance HtmlTableElem HtmlTh

instance HtmlTableCell HtmlTh

instance HasJustify HtmlTh where {justify j e = cset e "ALIGN" j}

instance HasHorizontalAlign HtmlTh

instance HasVerticalAlign HtmlTh


-- --------------------------------------------------------------------------
-- TD Element
-- --------------------------------------------------------------------------

newtype HtmlTd = HtmlTd HtmlElement

td :: [Config HtmlTd] -> WWW ()
td cf = createHtmlElem (AElem Td) cf

emptycell :: WWW ()
emptycell = plain "&#160"

instance GUIObject HtmlTd where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "TD"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlTd where 
        toHtmlElem (HtmlTd e) = e
        fromHtmlElem = HtmlTd

instance HtmlTableElem HtmlTd

instance HtmlTableCell HtmlTd

instance HasJustify HtmlTd where {justify j e = cset e "ALIGN" j}

instance HasHorizontalAlign HtmlTd

instance HasVerticalAlign HtmlTd


-- --------------------------------------------------------------------------
-- TR Element
-- --------------------------------------------------------------------------

newtype HtmlTr = HtmlTr HtmlElement

tr :: [Config HtmlTr] -> WWW ()
tr cf = createHtmlElem (AElem Tr) cf

instance GUIObject HtmlTr where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "TR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlTr where 
        toHtmlElem (HtmlTr e) = e
        fromHtmlElem = HtmlTr

instance HtmlTableElem HtmlTr

instance HasJustify HtmlTr where {justify j e = cset e "ALIGN" j}

instance HasHorizontalAlign HtmlTr

instance HasVerticalAlign HtmlTr



-- --------------------------------------------------------------------------
-- Table Rules
-- --------------------------------------------------------------------------

data TableRules = RuleNone | RuleGroups | RuleRows | RuleCols | RuleAll

instance GUIValue TableRules where
        cdefault = RuleNone


instance Show TableRules where
   showsPrec d p r = 
      (case p of 
        RuleNone ->  "none"
        RuleGroups -> "groups"
        RuleRows -> "rows"
        RuleCols -> "cols"
        RuleAll -> "all"
    ) ++ r


instance Read TableRules where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'n':'o':'n':'e':xs -> [(RuleNone,xs)]
        'g':'r':'o':'u':'p':'s':xs -> [(RuleGroups,xs)]
        'r':'o':'w':'s':xs -> [(RuleRows,xs)]
        'c':'o':'l':'s':xs -> [(RuleCols,xs)]
        'a':'l':'l':xs -> [(RuleAll,xs)]
        _ -> []


-- --------------------------------------------------------------------------
-- Table Frame
-- --------------------------------------------------------------------------

data TableFrame = FrameVoid | FrameAbove | FrameBelow | FrameHSides | 
        FrameLhs | FrameRhs | FrameVSides | FrameBox | FrameBorder


instance GUIValue TableFrame where
        cdefault = FrameVoid


instance Show TableFrame where
   showsPrec d p r = 
      (case p of 
        FrameVoid ->  "void"
        FrameAbove -> "above"
        FrameBelow -> "below"
        FrameHSides -> "hsides"
        FrameLhs -> "lhs"
        FrameRhs -> "rhs"
        FrameVSides -> "vsides"
        FrameBox -> "box"
        FrameBorder -> "border"
      ) ++ r


instance Read TableFrame where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'v':'o':'i':'d':xs -> [(FrameVoid,xs)]
        'a':'b':'o':'v':'e':xs -> [(FrameAbove,xs)]
        'b':'e':'l':'o':'w':xs -> [(FrameBelow,xs)]
        'h':'s':'i':'d':'e':'s':xs -> [(FrameHSides,xs)]
        'l':'h':'s':xs -> [(FrameLhs,xs)]
        'r':'h':'s':xs -> [(FrameRhs,xs)]
        'v':'s':'i':'d':'e':'s':xs -> [(FrameVSides,xs)]
        'b':'o':'x':xs -> [(FrameBox,xs)]
        'b':'o':'r':'d':'e':'r':xs -> [(FrameBorder,xs)]
        _ -> []
