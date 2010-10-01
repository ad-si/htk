module HTk.Kernel.GUIObjectKind (

  ObjectKind(..),
  CanvasItemKind(..),
  MenuItemKind(..)

) where

import HTk.Kernel.GUIValue
import HTk.Kernel.Geometry
import HTk.Kernel.Resources
import HTk.Kernel.GUIObjectName


-- -----------------------------------------------------------------------
--  OBJECT Kind
-- -----------------------------------------------------------------------

data ObjectKind =
    FRAME
  | LABEL
  | MESSAGE
  | BUTTON
  | CHECKBUTTON
  | RADIOBUTTON
  | MENUBUTTON
  | MENU
  | MENUITEM MenuItemKind Int             -- Tcl ID
  | OPTIONMENU [GUIVALUE]                 -- unpacked elements
  | LISTBOX [GUIVALUE]                    -- unpacked elements
  | SEPARATOR
  | ENTRY
  | TEXT GUIVALUE                         -- unpacked lines of text
  | CANVAS
  | SCALE
  | SCROLLBAR
  | TOPLEVEL
  | TEXTTAG [GUIVALUE]
  | EMBEDDEDTEXTWIN GUIVALUE ObjectName
  | CANVASITEM CanvasItemKind Coord
  | POSTSCRIPT
  | SESSION
  | GRAPH
  | ABSTRACT
  | WIDGET String
  | NOTEBOOK
  | NOTEBOOKPAGE String                   -- title
  | LABELFRAME
  | PANEDWINDOW Orientation               -- orientation of panes
  | WINDOWPANE
  | COMBOBOX Bool                         -- editable
  | BOX Orientation Flexibility
  | SUBWIDGET ObjectKind String

data CanvasItemKind =
    ARC
  | LINE
  | POLYGON
  | RECTANGLE
  | OVAL
  | BITMAPITEM
  | IMAGEITEM
  | TEXTITEM
  | CANVASTAG
  | EMBEDDEDCANVASWIN

data MenuItemKind =
    MENUCASCADE
  | MENUCOMMAND
  | MENUCHECKBUTTON
  | MENURADIOBUTTON
  | MENUSEPARATOR


-- -----------------------------------------------------------------------
--  Unparsing of Widget Kind
-- -----------------------------------------------------------------------

instance Show ObjectKind where
  showsPrec d p r =
    (case p of
       FRAME -> "frame"
       LABEL -> "label"
       MESSAGE -> "message"
       CHECKBUTTON -> "checkbutton"
       BUTTON -> "button"
       RADIOBUTTON -> "radiobutton"
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
       (CANVASITEM CANVASTAG _) -> "tag"
       (CANVASITEM EMBEDDEDCANVASWIN _) -> "window"
       (MENUITEM MENUCASCADE _) -> "cascade"
       (MENUITEM MENUCOMMAND _) -> "command"
       (MENUITEM MENUCHECKBUTTON _) -> "checkbutton"
       (MENUITEM MENURADIOBUTTON _) -> "radiobutton"
       (MENUITEM MENUSEPARATOR _) -> "separator"
       (EMBEDDEDTEXTWIN _ _) -> "window"
       (TEXTTAG _) -> "tag"
       (WIDGET kind) -> kind
       NOTEBOOK -> "tixNoteBook"
       NOTEBOOKPAGE _ -> ""
       LABELFRAME -> "tixLabelFrame"
       PANEDWINDOW _ -> "tixPanedWindow"
       COMBOBOX _ -> "tixComboBox"
       WINDOWPANE -> ""
       BOX _ _ -> "frame"
       SUBWIDGET subKind megaName -> megaName ++ " subwidget " ++ show subKind)
    ++ r
