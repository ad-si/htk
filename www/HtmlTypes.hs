{- #########################################################################

MODULE        : HtmlTypes
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : The Html Types.

   ######################################################################### -}


module HtmlTypes (
        HtmlDoc(..),
        HtmlElement(..),
        HtmlAttribute(..),
        HtmlKind(..),
        AKind(..),
        CKind(..),
        Style(..),
        BKind(..),
        LKind(..)
        )

where

import Debug(debug)

-- --------------------------------------------------------------------------
-- Type Html
-- --------------------------------------------------------------------------
        
data HtmlDoc = HtmlDoc HtmlElements

type HtmlElements = [HtmlElement]

data HtmlElement = HtmlElement HtmlKind [HtmlAttribute] [HtmlElement]

data HtmlAttributes = HtmlAttributes [HtmlAttribute]

data HtmlAttribute = HtmlAttribute String String

data HtmlKind   = 
          Text String 
        | Comment String 
        | Title String 
        | Prologue String
        | IsIndex 
        | NextId String
        | Dt 
        | Dd 
        | Li
        | Format Style                  -- CKind  
        | Block BKind                   -- CKind
        | List LKind                    -- CKind
        | AElem AKind
        | CElem CKind


data AKind      = Base | MetaElem | Link | Img | Input | Br | Hr | Par
                 | Area | Param | ColGroup | Col | THead | TFoot | TBody
                 | Tr | Th | Td | BGSound | BaseFont | WBr deriving Show

data CKind      = Head | Body | Anc | Ref | Form | Select | Option | TextArea 
                | HTMLFont | HtmlMap | Applet | Table | Caption | NoBr 
                | Frame | FrameSet | NoFrames

data Style      = Em | Cite | Code | Kbd | Samp | Strong | Var | B | I | TT |
                  U | Strike | Big | Small | Sub | Sup | Blink | Dfn
                deriving (Show)

data BKind      = Pre | Address | Quote | Div | H1 | H2 | H3 | H4 | H5 | H6 
                  deriving Show

data LKind      = Dl | Dir | Menu | Ol | Ul deriving (Show)
                

-- -------------------------------------------------------------------------
-- Unparsing
-- -------------------------------------------------------------------------    

instance Show HtmlDoc where
   showsPrec d p r = 
      (case p of 
         (HtmlDoc []) -> sTagElem "HTML" []
         (HtmlDoc ((HtmlElement (Prologue s) _ _) :el)) -> 
                (showElem (HtmlElement (Prologue s) [] []) 0 True False) ++ 
                sTagElem "HTML" (showElems el 0 False)
         (HtmlDoc el) ->  showElems el 0 True
       ) ++ r


instance Show HtmlAttributes where
   showsPrec d p r = 
      (case p of 
        (HtmlAttributes []) -> ""
        (HtmlAttributes [a]) -> show a 
        (HtmlAttributes (a : al)) -> show a ++ " " ++ show (HtmlAttributes al)  
      ) ++ r


instance Show HtmlAttribute where
   showsPrec d p r = 
      (case p of 
         (HtmlAttribute n "") -> n 
         (HtmlAttribute n v) -> n ++ "=" ++ "\"" ++ v ++ "\""
      ) ++ r
        

instance Show CKind where
   showsPrec d p r = 
      (case p of 
          Head -> "HEAD" 
          Body -> "BODY" 
          Anc -> "A" 
          Ref -> "A" 
          Form -> "FORM" 
          Select -> "SELECT" 
          Option -> "OPTION" 
          TextArea -> "TEXTAREA" 
          HTMLFont -> "FONT" 
          HtmlMap -> "MAP" 
          Applet -> "APPLET" 
          Table -> "TABLE" 
          Caption -> "CAPTION" 
      ) ++ r
        
  
-- -------------------------------------------------------------------------
-- Auxiliary Functions
-- -------------------------------------------------------------------------    


showElems :: HtmlElements -> Int -> Bool -> String
showElems [] _ _        = ""
showElems (e :[]) il first = showElem e il first True
showElems (e :el) il first = (showElem e il first False) ++ (showElems el il False)


showElem :: HtmlElement -> Int -> Bool -> Bool -> String

showElem (HtmlElement (Text t) _ _) il f l = 
        t 

showElem (HtmlElement (Comment c) _ _) il f l = 
        bem f ++ indent il ++ "<!-- " ++ c ++ "-->" ++ eem l

showElem (HtmlElement (Prologue p) _ _) il f l = 
        "<! " ++ p ++ ">" ++ eem l

showElem (HtmlElement (NextId s) _ _) il f l = 
        "<NEXTID N = " ++ s ++ ">"

showElem (HtmlElement (Title t) _ _) il f l = 
        bem f ++ sTagElem "TITLE" t ++ eem l

showElem (HtmlElement Dt _ _)  il f l = 
        "\n" ++ indent il ++ "<DT>"

showElem (HtmlElement Dd _ _) il f l = 
        "<DD>"

showElem (HtmlElement Li _ _) il f l =  
        "\n" ++ indent il ++ "<LI>"

showElem (HtmlElement IsIndex _ _) il f l = 
        bem f ++ "<ISINDEX>"

showElem (HtmlElement (AElem k) a _) il f l = 
        nl ++ sAElem (show k) (show (HtmlAttributes a))
        where nl = case k of {Img -> ""; Input -> ""; _ -> bem f} 

showElem (HtmlElement (CElem k) a e) il f l = 
        bem f ++ (indent il) ++ 
        sCElem  k il (show (HtmlAttributes a)) (showElems e il True) ++ eem l
        where   sCElem  :: CKind -> Int -> String -> String -> String
                sCElem k il at el =
                 (sAElem tag at) ++ bnl k ++ el ++ enl k ++ "</" ++ tag ++ ">"
                tag = show k
                enl Head = "\n" 
                enl Body = "\n"
                enl Form = "\n"
                enl Applet = "\n"
                enl Table = "\n"
                enl _ = ""
                bnl Head = "\n"
                bnl Body = "\n"
                bnl Applet = "\n"
                bnl Table = "\n"
                bnl _ = ""

showElem (HtmlElement (Format p) a e) il f l = 
        (sAElem tag (show (HtmlAttributes a))) ++(showElems e il True) ++ "</" ++ tag ++ ">" 
        where tag = show p

showElem (HtmlElement (Block k) a e) il f l = 
        bem f ++ (indent il) ++  sBElem  k il (show (HtmlAttributes a)) (showElems e il True) ++ eem l
        where 
              sBElem k il at el =
                (sAElem tag at) ++ bnl k ++ el ++ enl k ++ "</" ++ tag ++ ">" 
              tag = show k
              enl Pre = "\n"
              enl Address = "\n"
              enl _ = ""
              bnl Address = "\n"
              bnl _ = ""

showElem (HtmlElement (List k) a e) il f l = 
        bem f ++ (indent il) ++ 
        sLElem k il (show (HtmlAttributes a)) (showElems e (il + 1) True) ++ 
        eem l
        where 
                sLElem k il at el =
                  (sAElem tag at) ++ el ++ enl k ++ "</" ++ tag ++ ">" 
                tag = show k
                enl _ = "\n" ++ indent il


indent 0  = ""
indent n  = "  " ++ indent (n - 1)

sTagElem :: String -> String -> String          
sTagElem tag cnt = "<" ++ tag ++ ">" ++ cnt ++ "</" ++ tag ++ ">"

sAElem :: String -> String -> String            
sAElem tag []   = "<" ++ tag ++ ">"
sAElem tag at   = "<" ++ tag ++ " " ++ at ++ ">"

eem l   =  if l then "" else "\n"

bem f   =  if f then "" else "\n"
        

