{- #########################################################################

MODULE        : GUIObjectKind
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines the possible set of GUIObjects (widgets etc).


   ######################################################################### -}


module GUIObjectKind (
        ObjectKind(..), 
        CanvasItemKind(..)
        ) where


import Resources
import Debug(debug)

  
-- --------------------------------------------------------------------------
--  OBJECT Kind
-- --------------------------------------------------------------------------

data ObjectKind =
          FRAME 
        | LABEL 
        | MESSAGE 
        | CLICKBUTTON 
        | CHECKBUTTON Toggle
        | RADIOBUTTON Toggle
        | MENUBUTTON
        | MENU 
        | OPTIONMENU [GUIVALUE]                 -- unpacked elements 
        | LISTBOX [GUIVALUE]                    -- unpacked elements
        | SEPARATOR  
        | ENTRY 
        | TEXT GUIVALUE                         -- unpacked lines of text
        | CANVAS 
        | SCALE
        | SCROLLBAR     
        | TOPLEVEL
        | TEXTTAG [GUIVALUE]                    -- initial position
        | EMBEDDEDTEXTWIN GUIVALUE              -- initial position
        | CANVASITEM CanvasItemKind Coord
        | POSTSCRIPT
        | SESSION
        | GRAPH
        | ABSTRACT
        | WIDGET String


data CanvasItemKind = 
          ARC 
        | LINE 
        | POLYGON
        | RECTANGLE
        | OVAL
        | BITMAPITEM 
        | IMAGEITEM
        | TEXTITEM
        | CANVASTAG String                      -- searchspec
        | EMBEDDEDCANVASWIN


-- --------------------------------------------------------------------------
--  Unparsing of Widget Kind
-- --------------------------------------------------------------------------

instance Show ObjectKind where
   showsPrec d p r = 
      (case p of 
                FRAME -> "frame" 
                LABEL -> "label"
                MESSAGE -> "message"
                (CHECKBUTTON _) -> "checkbutton"
                CLICKBUTTON -> "button"
                (RADIOBUTTON _) -> "radiobutton"
                MENUBUTTON -> "menubutton"
                MENU -> "menu"
                (OPTIONMENU _) -> "tk_optionMenu" 
                (LISTBOX _) -> "listbox"
                SEPARATOR -> "separator"  
                ENTRY -> "entry"
                (TEXT _) -> "text"
                CANVAS -> "canvas"
                SCALE -> "scale"
                SCROLLBAR -> "scrollbar"        
                TOPLEVEL -> "toplevel"
                (CANVASITEM ARC _) -> "arc"
                (CANVASITEM LINE _) -> "line"
                (CANVASITEM POLYGON _) -> "polygon"
                (CANVASITEM RECTANGLE _) -> "rectangle"
                (CANVASITEM OVAL _) -> "oval"
                (CANVASITEM BITMAPITEM _) -> "bitmap"
                (CANVASITEM IMAGEITEM _) -> "image"
                (CANVASITEM TEXTITEM _) -> "text"
                (CANVASITEM (CANVASTAG _) _) -> "tag"
                (CANVASITEM EMBEDDEDCANVASWIN _) -> "window"
                (EMBEDDEDTEXTWIN _) -> "window"
                (TEXTTAG _) -> "tag"
                (WIDGET kind) -> kind
        ) ++ r

