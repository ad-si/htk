{- #########################################################################

MODULE        : HtmlForm
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Form Elements.

   ######################################################################### -}

module HtmlForm (

        Distance,
        Font(..),

        HtmlForm,
        HTTPMethod(..),
        form,
        method,
        enctype,

        HtmlSelect,
        hselect,
        multiple,

        HtmlOption,
        option,
        selected,

        HtmlTextArea,
        textarea,
           
        HtmlInput,
        InputType(..),
        input,
        checked,
        kind,
        maxlength

        )

where

import HTk
import HtmlKernel
import HtmlLink
import Char
import Font
import Debug(debug)

-- --------------------------------------------------------------------------
-- Form Elements
-- --------------------------------------------------------------------------

data HtmlForm = HtmlForm HtmlElement

form :: [Config HtmlForm] -> WWW a -> WWW a
form cf cmd = createHtmlComp (CElem Form) cf cmd

instance GUIObject HtmlForm where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "FORM"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlForm where 
        toHtmlElem (HtmlForm e) = e
        fromHtmlElem = HtmlForm

instance HtmlHyperLink HtmlForm where
        url u e = cset e "ACTION" (RawData u)


method :: HTTPMethod -> Config HtmlForm
method m e = cset e "METHOD" m

type MimeContent = String

enctype :: MimeContent -> Config HtmlForm
enctype mc e = cset e "ENCTYPE" (RawData mc)


-- --------------------------------------------------------------------------
-- Select Element
-- --------------------------------------------------------------------------

data HtmlSelect = HtmlSelect HtmlElement

hselect :: [Config HtmlSelect] -> WWW a -> WWW a
hselect cf cmd = createHtmlComp (CElem Select) cf cmd

instance GUIObject HtmlSelect where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "SELECT"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlSelect where 
        toHtmlElem (HtmlSelect e) = e
        fromHtmlElem = HtmlSelect

instance HasName HtmlSelect where 
        name nm e = cset e "NAME" (RawData nm)
        getName e = do {
                (RawData v) <- cget e "NAME"; 
                return v
                }


instance HasSize HtmlSelect where
        height _ e = return e
        width _ e = return e
        size n e = cset e "SIZE" n

multiple :: Config HtmlSelect
multiple e = cset e "MULTIPLE" CVOID


-- --------------------------------------------------------------------------
-- Option Element
-- --------------------------------------------------------------------------

data HtmlOption = HtmlOption HtmlElement

option :: [Config HtmlOption] -> WWW a -> WWW a
option cf cmd = createHtmlComp (CElem Option) cf cmd

instance GUIObject HtmlOption where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "OPTION"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlOption where 
        toHtmlElem (HtmlOption e) = e
        fromHtmlElem = HtmlOption

instance HasHtmlValue HtmlOption where 
        htmlvalue v e = cset e "VALUE" (RawData v) 

selected :: Config HtmlOption
selected e = cset e "SELECTED" CVOID


-- --------------------------------------------------------------------------
-- TextArea Element
-- --------------------------------------------------------------------------

data HtmlTextArea = HtmlTextArea HtmlElement

textarea :: [Config HtmlTextArea] -> WWW a -> WWW a
textarea cf cmd = createHtmlComp (CElem TextArea) cf cmd

instance GUIObject HtmlTextArea where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "TEXTAREA"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlTextArea where 
        toHtmlElem (HtmlTextArea e) = e
        fromHtmlElem = HtmlTextArea

instance HasName HtmlTextArea where 
        name v e = cset e "NAME" (RawData v)
        getName e = do {
                (RawData v) <- cget e "NAME"; 
                return v
                }

instance HasSize HtmlTextArea where 
        width w e = cset e "COLS" w
        height h e = cset e "ROWS" h

-- --------------------------------------------------------------------------
-- TextArea Element
-- --------------------------------------------------------------------------

data HtmlInput = HtmlInput HtmlElement

input :: [Config HtmlInput] -> WWW ()
input cf = createHtmlElem (AElem Input) cf

instance GUIObject HtmlInput where
        toGUIObject _ = getHtmlGUIObject
        cname _ = "INPUT"
        cset w cid v = setHtmlAttr w cid v
        cget = getHtmlAttr

instance HtmlElem HtmlInput where 
        toHtmlElem (HtmlInput e) = e
        fromHtmlElem = HtmlInput

instance HtmlHyperLink HtmlInput where 
        url u e = cset e "SRC" (RawData u)

instance HasSize HtmlInput where
        width v e = cset e "SIZE" v
        height h e = return e

instance HasName HtmlInput where 
        name v e = cset e "NAME" (RawData v)
        getName e = do {
                (RawData v) <- cget e "NAME"; 
                return v
                }


instance HasHtmlValue HtmlInput where 
        htmlvalue v e = cset e "VALUE" (RawData v)

instance HasAlign HtmlInput

checked :: Config HtmlInput
checked e = cset e "CHECKED" CVOID

kind :: InputType -> Config HtmlInput
kind t e = cset e "TYPE" t

maxlength :: Int -> Config HtmlInput
maxlength l e = cset e "MAXLENGTH" l


-- --------------------------------------------------------------------------
-- HTTP Method
-- --------------------------------------------------------------------------

data HTTPMethod = Get | Post

instance GUIValue HTTPMethod where
        cdefault = Get

instance Show HTTPMethod where
   showsPrec d p r = 
      (case p of 
        Get -> "get"
        Post -> "post"
      ) ++ r

        
instance Read HTTPMethod where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'g':'e':'t':xs -> [(Get,xs)]
        'p':'o':'s':'t':xs -> [(Post,xs)]
        _ -> []



-- --------------------------------------------------------------------------
-- InputType
-- --------------------------------------------------------------------------


data InputType = TextField | ImageField | PassWord | CheckBox | Radio 
        | Submit | Reset | File | Hidden

instance GUIValue InputType where
        cdefault = TextField


instance Show InputType where
   showsPrec d p r = 
      (case p of 
        TextField -> "text"
        ImageField -> "image"
        PassWord -> "password"
        CheckBox -> "checkbox"
        Radio -> "radio"
        Submit -> "submit"
        Reset -> "reset"
        File -> "file"
        Hidden -> "hidden"
     ) ++ r

instance Read InputType where
   readsPrec p b =
     case dropWhile (isSpace) b of
        't':'e':'x':'t':xs -> [(TextField,xs)]
        'i':'m':'a':'g':'e':xs -> [(ImageField,xs)]
        'p':'a':'s':'s':'w':'o':'r':'d':xs -> [(PassWord,xs)]
        'c':'h':'e':'c':'k':'b':'o':'x':xs -> [(CheckBox,xs)]
        'r':'a':'d':'i':'o':xs -> [(Radio,xs)]
        's':'u':'b':'m':'i':'t':xs -> [(Submit,xs)]
        'r':'e':'s':'e':'t':xs -> [(Reset,xs)]
        'f':'i':'l':'e':xs -> [(File,xs)]
        'h':'i':'d':'d':'e':'n':xs -> [(Hidden,xs)]
        _ -> []


