{- #########################################################################

MODULE        : HtmlList
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html List Elements.

   ######################################################################### -}


module HtmlList (

        HtmlElem(..),
        HtmlList(..),

        li,
        dt,
        dd,

        HtmlDl,
        deflist,
         
        HtmlDir, 
        dirlist, 

        HtmlMenu,
        menulist,   
           
        HtmlUl,
        BulletStyle(..),
        ulist,     
        bullet,

        HtmlOl,
        NumberingStyle(..),
        olist,
        start,
        numbering

        )

where

import HTk
import HtmlKernel
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
-- LI Element
-- --------------------------------------------------------------------------

li :: WWW ()
li = addHtmlElem Li


-- --------------------------------------------------------------------------
-- DT Elements
-- --------------------------------------------------------------------------

dt :: WWW ()
dt = addHtmlElem Dt


-- --------------------------------------------------------------------------
-- DD Elements
-- --------------------------------------------------------------------------

dd :: WWW ()
dd = addHtmlElem Dd


-- --------------------------------------------------------------------------
-- Definition List Elements
-- --------------------------------------------------------------------------

newtype HtmlDl = HtmlDl HtmlElement

deflist :: [Config HtmlDl] -> WWW a -> WWW a
deflist cnfs cmd = createHtmlComp (List Dl) cnfs cmd

instance GUIObject HtmlDl where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "DL"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlDl where
        toHtmlElem (HtmlDl e) = e
        fromHtmlElem = HtmlDl

instance HtmlList HtmlDl


-- --------------------------------------------------------------------------
-- Directory List Elements
-- --------------------------------------------------------------------------

newtype HtmlDir = HtmlDir HtmlElement

dirlist :: [Config HtmlDir] -> WWW a -> WWW a
dirlist cnfs cmd = createHtmlComp (List Dir) cnfs cmd

instance GUIObject HtmlDir where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "DIR"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlDir where 
        toHtmlElem (HtmlDir e) = e
        fromHtmlElem = HtmlDir

instance HtmlList HtmlDir
  

-- --------------------------------------------------------------------------
-- Menu List Elements
-- --------------------------------------------------------------------------

newtype HtmlMenu = HtmlMenu HtmlElement

menulist :: [Config HtmlMenu] -> WWW a -> WWW a
menulist cnfs cmd = createHtmlComp (List Menu) cnfs cmd
        
instance GUIObject HtmlMenu where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "MENU"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlMenu where 
        toHtmlElem (HtmlMenu e) = e
        fromHtmlElem = HtmlMenu

instance HtmlList HtmlMenu
  

-- --------------------------------------------------------------------------
-- Ordered List Elements
-- --------------------------------------------------------------------------

newtype HtmlOl = HtmlOl HtmlElement

olist :: [Config HtmlOl] -> WWW a -> WWW a
olist cnfs cmd = createHtmlComp (List Ol) cnfs cmd

instance GUIObject HtmlOl where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "OL"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlOl where 
        toHtmlElem (HtmlOl e) = e
        fromHtmlElem = HtmlOl

instance HtmlList HtmlOl

start :: Int -> Config HtmlOl
start n e = cset e "START" n

numbering :: NumberingStyle -> Config HtmlOl
numbering s e = cset e "TYPE" s 



-- --------------------------------------------------------------------------
-- List Elements
-- --------------------------------------------------------------------------

newtype HtmlUl = HtmlUl HtmlElement

ulist :: [Config HtmlUl] -> WWW a -> WWW a
ulist cnfs cmd = createHtmlComp (List Ul) cnfs cmd

instance GUIObject HtmlUl where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "UL"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlUl where 
        toHtmlElem (HtmlUl e) = e
        fromHtmlElem = HtmlUl

instance HtmlList HtmlUl

bullet ::BulletStyle -> Config HtmlUl
bullet s e = cset e "TYPE" s 


-- --------------------------------------------------------------------------
-- Bullet Style
-- --------------------------------------------------------------------------


data BulletStyle = Disc | Square | Circle

instance GUIValue BulletStyle where
        cdefault = Disc

instance Show BulletStyle where
   showsPrec d p r = 
      (case p of 
        Disc ->  "disc"
        Square -> "square"
        Circle -> "circle"
      ) ++ r


instance Read BulletStyle where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'd':'i':'s':'c':xs -> [(Disc,xs)]
        's':'q':'u':'a':'r':'e':xs -> [(Square,xs)]
        'c':'i':'r':'c':'l':'e':xs -> [(Circle,xs)]
        _ -> []


-- --------------------------------------------------------------------------
-- Numbering Style
-- --------------------------------------------------------------------------


data NumberingStyle = ArabicNumbers | LowerAlpha | UpperAlpha | LowerRoman |
        UpperRoman

instance GUIValue NumberingStyle where
        cdefault = ArabicNumbers


instance Show NumberingStyle where
   showsPrec d p r = 
      (case p of 
        ArabicNumbers -> "1"
        LowerAlpha -> "a"
        UpperAlpha -> "A"
        LowerRoman -> "i"
        UpperRoman -> "I"
      ) ++ r
        
instance Read NumberingStyle where
   readsPrec p b =
     case dropWhile (isSpace) b of
        '1':xs -> [(ArabicNumbers,xs)]
        'a':xs -> [(LowerAlpha,xs)]
        'A':xs -> [(UpperAlpha,xs)]
        'i':xs -> [(LowerRoman,xs)]
        'I':xs -> [(UpperRoman,xs)]
        _ -> []



