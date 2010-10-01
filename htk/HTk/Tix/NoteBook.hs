{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | HTk\'s /notebook/ and
-- /notebook pages/.
-- This widget is from the Tix library and therefore only available if
-- you are using tixwish.
module HTk.Tix.NoteBook (

  NoteBook,
  NoteBookPage,

  newNoteBook,
  createNoteBookPage

) where

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import Util.Computation
import Events.Synchronized
import Events.Destructible
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip

-- -----------------------------------------------------------------------
-- type NoteBook
-- -----------------------------------------------------------------------

-- | The @NoteBook@ datatype.
newtype NoteBook = NoteBook GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- notebook creation
-- -----------------------------------------------------------------------

-- | Constructs a new notebook widget and returns it as a value.
newNoteBook :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config NoteBook]
   -- ^ the list of configuration options for this notebook.
   -> IO NoteBook
   -- ^ A notebook widget.
newNoteBook par cnf =
  do
    w <- createWidget (toGUIObject par) NOTEBOOK
    configure (NoteBook w) cnf


-- -----------------------------------------------------------------------
-- notebook instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject NoteBook where
  toGUIObject (NoteBook w) = w
  cname _ = "NoteBook"

-- | You can specify the size of a notebook widget.
instance HasSize NoteBook

-- | A notebook widget has standard widget properties (focus, cursor, ...).
instance Widget NoteBook

-- | A notebook widget can be destroyed.
instance Destroyable NoteBook where
  -- Destroys a notebook widget.
  destroy = destroy . toGUIObject

-- | You can synchronize on a notebook object (in JAVA style).
instance Synchronized NoteBook where
  -- Synchronizes on a notebook object.
  synchronize = synchronize . toGUIObject



-- -----------------------------------------------------------------------
-- type NoteBookPage
-- -----------------------------------------------------------------------

-- | The @NoteBookPage@ datatype - a single page of a notebook.
newtype NoteBookPage = NoteBookPage GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- notebook page creation
-- -----------------------------------------------------------------------

-- | Constructs a new page inside a notebook widget and returns it as a
-- value.
createNoteBookPage :: NoteBook
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> String
   -- ^ the list of configuration options for this notebook
   -- page.
   -> [Config NoteBookPage]
   ->
   IO NoteBookPage
   -- ^ A notebook page.
createNoteBookPage nb title cnf =
  do
    w <- createGUIObject (toGUIObject nb) (NOTEBOOKPAGE title) pageMethods
    configure (NoteBookPage w) cnf


-- -----------------------------------------------------------------------
-- notebook page methods
-- -----------------------------------------------------------------------

pageMethods = Methods tkGetNoteBookPageConfig
                      tkSetNoteBookPageConfigs
                      tkCreateNoteBookPage
                      (packCmd voidMethods)
                      (gridCmd voidMethods)
                      (destroyCmd defMethods)
                      (bindCmd defMethods)
                      (unbindCmd defMethods)
                      (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- unparsing of notebook page commands
-- -----------------------------------------------------------------------

tkGetNoteBookPageConfig :: ObjectName -> ConfigID -> TclScript
tkGetNoteBookPageConfig (NoteBookPageName oid) cid =
  ["global v" ++ show oid,
   "$v" ++ show oid ++ " cget -" ++ cid]
{-# INLINE tkGetNoteBookPageConfig #-}

tkSetNoteBookPageConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetNoteBookPageConfigs (NoteBookPageName oid) args =
  ["global v" ++ show oid,
   "$v" ++ show oid ++ " configure " ++ showConfigs args]
tkSetNoteBookPageConfigs _ _ = []
{-# INLINE tkSetNoteBookPageConfigs #-}

tkCreateNoteBookPage :: ObjectName -> ObjectKind -> ObjectName ->
                        ObjectID -> [ConfigOption] -> TclScript
tkCreateNoteBookPage parnm (NOTEBOOKPAGE title) _ oid args =
  [show parnm ++ " add " ++ show oid ++ " -label \"" ++ title ++ "\" " ++
   showConfigs args,
   "global v" ++ show oid,
   "set v" ++ show oid ++ " [" ++ show parnm ++ " subwidget " ++
   show oid ++ "]"]
{-# INLINE tkCreateNoteBookPage #-}


-- -----------------------------------------------------------------------
-- notebook page instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject NoteBookPage where
  toGUIObject (NoteBookPage w) = w
  cname _ = "NoteBookPage"

-- | A notebook page can be destroyed.
instance Destroyable NoteBookPage where
  -- Destroys a notebook page.
  destroy   = destroy . toGUIObject

-- | A notebook page has standard widget properties
-- (concerning focus, cursor).
instance Widget NoteBookPage

-- | A notebook page is a container for widgets. You can pack widgets to
-- a notebook page via pack or grid command in the
-- @module HTk.Kernel.Packer@.
instance Container NoteBookPage

-- | A notebook page has a text label.
instance GUIValue a => HasText NoteBookPage a where
  text s w  = cset w  "label" s
  getText w = cget w "label"

-- | A notebook page has a configureable border.
instance HasBorder NoteBookPage

-- | A notebook page can have a tooltip.
instance HasTooltip NoteBookPage

-- | A notebook page has a background colour.
instance HasColour NoteBookPage where
  legalColourID = hasBackGroundColour

-- | You can synchronize on a notebook page (in JAVA style).
instance Synchronized NoteBookPage where
  -- Synchronizes on a notebook page object.
  synchronize = synchronize . toGUIObject
