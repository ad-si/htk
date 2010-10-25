{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTk's <strong>entry field</strong>.<br>
-- A simple widget that displays an editable line of text.
module HTk.Widgets.Entry (
  Entry,

  newEntry,

  XCoord(..),

  showText,
  getShowText

) where

import Control.Exception

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Geometry
import HTk.Widgets.ScrollBar
import HTk.Components.Selection
import HTk.Devices.XSelection
import HTk.Components.Index
import HTk.Components.ICursor
import Util.Computation
import Events.Destructible
import Events.Synchronized
import HTk.Kernel.Packer
import HTk.Kernel.TkVariables
import HTk.Kernel.Tooltip
import HTk.Tix.Subwidget


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

-- | The @Entry@ datatype.
newtype Entry a = Entry GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new entry field and returns a handler.
newEntry :: (Container par, GUIValue a) =>
   par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config (Entry a)]
   -- ^ the list of configuration options for this entry
   -- field.
   -> IO (Entry a)
   -- ^ An entry field.
newEntry par cnf =
  do
    wid <- createGUIObject (toGUIObject par) ENTRY entryMethods
    configure (Entry wid) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject (Entry a) where
  toGUIObject (Entry  w) = w
  cname _ = "Entry"

-- | An entry field has a configureable width (height config is ignored).
instance HasSize (Entry a) where
  height _ w = return w
  getHeight w = return 1

-- | An entry field can be destroyed.
instance Destroyable (Entry a) where
  destroy   = destroy . toGUIObject

-- | An entry field has standard widget properties
-- (concerning focus, cursor).
instance Widget (Entry a)

-- | An entry field has a configureable border.
instance HasBorder (Entry a)

-- | An entry field has a foreground and background colour.
instance HasColour (Entry a) where
  legalColourID = hasForeGroundColour

-- | You can specify the font of an entry field.
instance HasFont (Entry a)

-- | The value of an entry field is associated with a polymorphic variable.
instance HasVariable (Entry a) where
  variable var@(TkVariable oid) w =
    cset w "textvariable" ("v" ++ show oid) >> return w

-- | An entry field has a value that is associated with a polymorphic
-- variable.
instance GUIValue a => HasValue (Entry a) a where
  value val w = execMethod w (\nm-> tkSetText nm (toGUIValue val)) >>
                return w
  -- Selector for the value of an entry field.
  --    w         - the concerned entry field.
  --    result    - The concerned entry field.
  getValue w  = evalMethod w (\nm-> tkGetText nm)

-- | An entry field has a configureable text justification.
instance HasJustify (Entry a)

-- | An entry field is a stateful widget - it can be enabled of disabled.
instance HasEnable (Entry a)

-- | An entry field is scrollable in horizontal direction.
instance HasScroller (Entry a) where
  isWfOrientation _ Horizontal = True
  isWfOrientation _ Vertical   = False

-- | You can synchronize on an entry field (in JAVA-style)
instance Synchronized (Entry a) where
  synchronize w = synchronize (toGUIObject w)

-- | An entry can have a tooltip (only displayed if you are using tixwish).
instance HasTooltip (Entry a)


-- -----------------------------------------------------------------------
-- index
-- -----------------------------------------------------------------------

-- | The @XCoord@ datatype.
data XCoord = XCoord Distance

-- | Internal.
instance Show XCoord where
   showsPrec d (XCoord x) r = "@"++show x ++ r


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

-- | An integer value is a valid index position for an entry widget.
instance HasIndex (Entry a) Int BaseIndex where
  getBaseIndex w i = return (IndexNo i)

-- | A base index is a valid index position for an entry widget.
instance HasIndex (Entry a) BaseIndex BaseIndex where
  getBaseIndex w i = return i

-- | The @EndOfText@ index is a valid index position for an
-- entry widget.
instance HasIndex (Entry a) EndOfText BaseIndex where
  getBaseIndex w _ = return (IndexText "end")

-- | An @XCoord@ is a valid index for an entry widget.
instance HasIndex (Entry a) XCoord BaseIndex where
  getBaseIndex ent i = return (IndexText (show i))

-- | The entries insertion cursor is a valid index for an entry widget.
instance HasIndex (Entry a) (ICursor (Entry a)) BaseIndex where
  getBaseIndex ent i = return (IndexText "insert")

-- | The selection start is a valid index position for an entry widget.
instance HasIndex (Entry a) (Selection (Entry a),First) BaseIndex where
  getBaseIndex ent i = return (IndexText "sel.first")

-- | The selection end is a valid index position for an entry widget.
instance HasIndex (Entry a) (Selection (Entry a),Last) BaseIndex where
  getBaseIndex ent i = return (IndexText "sel.last")

-- | Internal.
instance HasIndex (Entry a) i BaseIndex => HasIndex (Entry a) i Int where
  getBaseIndex w i =
    do
      bi <- getBaseIndex w i
      evalMethod w (\nm -> tkGetIndexNumber nm bi)
    where tkGetIndexNumber :: ObjectName -> BaseIndex -> TclScript
          tkGetIndexNumber nm bi = [show nm ++ " index " ++ show bi]


-- -----------------------------------------------------------------------
-- selection
-- -----------------------------------------------------------------------

-- | You can select text inside an entry widget.
instance HasSelection (Entry a) where
  -- Clears the entry\'s selection.
  clearSelection ent =
    execMethod ent (\nm -> [show nm ++ " selection clear"])

-- | An entry widget\'s characters are selectable.
instance HasIndex (Entry a) i BaseIndex =>
           HasSelectionIndex (Entry a) i where
  -- Selects the character at the specified index.
  selection inx ent =
    synchronize ent
      (do
         binx <- getBaseIndex ent inx
         execMethod ent (\nm -> [tkSelection nm binx])
         return ent)
  -- Queries if the character at the specified index is selected.
  isSelected ent inx =
    synchronize ent
      (do
         binx <- getBaseIndex ent inx
         start <- getSelectionStart ent
         end <- getSelectionEnd ent
         case (start,end,binx) of
           (Just start, Just end, IndexNo i) ->
             return (start <= i && i < end)
           _ ->    return False)

-- | You can select a text range inside an entry widget.
instance HasSelectionBaseIndex (Entry a) (Int,Int) where
  getSelection = getSelectionRange

-- | You can select a text range inside an entry widget.
instance (HasIndex (Entry a) i1 BaseIndex,
          HasIndex (Entry a) i2 BaseIndex) =>
         HasSelectionIndexRange (Entry a) i1 i2 where
  -- Sets the selection range inside the entry widget.
  selectionRange start end ent =
    synchronize ent
      (do
         start' <- getBaseIndex ent start
         end' <- getBaseIndex ent end
         execMethod ent (\nm -> [tkSelectionRange nm start' end'])
         return ent)

-- | You can select a text range inside an entry widget.
instance HasSelectionBaseIndexRange (Entry a) Int where
  getSelectionStart ent =
    do
      mstart <-
        try (evalMethod ent (\nm -> [show nm ++ " index sel.first "]))
      case mstart of
        Left (e :: SomeException) -> return Nothing     -- actually a tk error
        Right v -> return (Just v)
  -- Gets the end index of the entry\'s selection.
  getSelectionEnd ent =
    do
      mend <-
        try (evalMethod ent (\nm -> [show nm ++ " index sel.last "]))
      case mend of
        Left (e :: SomeException) -> return Nothing    -- actually a tk error
        Right v -> return (Just v)

-- | An editor widget has an X selection.
instance HasXSelection (Entry a)


-- -----------------------------------------------------------------------
-- insertion cursor
-- -----------------------------------------------------------------------

-- | An entry widget has an insertion cursor.
instance HasInsertionCursor (Entry a)

-- |
instance HasIndex (Entry a) i BaseIndex =>
           HasInsertionCursorIndexSet (Entry a) i where
  -- Sets the position of the insertion cursor.
  insertionCursor inx ent =
    synchronize ent
      (do
         binx <- getBaseIndex ent inx
         execMethod ent (\nm -> [tkSetInsert nm binx])
         return ent)

-- | You can get the position of the insertion cursor of an entry widget.
instance HasInsertionCursorIndexGet (Entry a) Int where

  -- Gets the position of the insertion cursor.
  getInsertionCursor ent = evalMethod ent (\nm -> [tkGetInsert nm])


-- | An entry widget can be a subwidget, e.g. in a combo box
instance GUIValue a => CanBeSubwidget (Entry a) where
  createAsSubwidget megaWidget
     = do e <- createSubwidget ENTRY entryMethods megaWidget
          return (Entry e)

-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

-- | Sets a character to display instead of contents (e.g. for password
-- fields).
showText :: GUIValue a => Char
   -- ^ the character to display.
   -> Entry a
   -- ^ the concerned entry field.
   -> IO (Entry a)
   -- ^ The concerned entry field.
showText ch ent = cset ent "show" [ch]

-- | Gets the character to show instead of contents.
getShowText :: GUIValue a => Entry a -> IO Char
getShowText w = do {l <- cget w "show"; return (head (l ++ " "))}


-- -----------------------------------------------------------------------
-- entry methods
-- -----------------------------------------------------------------------

entryMethods = defMethods { cleanupCmd = tkCleanupEntry,
                            createCmd = tkCreateEntry }


-- -----------------------------------------------------------------------
-- Unparsing of Tk Commands
-- -----------------------------------------------------------------------

tkSetInsert :: ObjectName -> BaseIndex -> TclCmd
tkSetInsert wn i = show wn ++ " icursor " ++ show i
{-# INLINE tkSetInsert #-}

tkGetInsert :: ObjectName -> TclCmd
tkGetInsert wn = show wn ++ " index insert"
{-# INLINE tkGetInsert #-}

tkSelection :: ObjectName -> BaseIndex -> TclCmd
tkSelection wn (IndexNo i) =
  show wn ++ " selection range " ++ show i ++ " " ++ show (i + 1)
tkSelection wn _ = show wn ++ " selection range end end"
{-# INLINE tkSelection #-}

tkSelectionRange :: ObjectName -> BaseIndex ->  BaseIndex -> TclCmd
tkSelectionRange wn start end = show wn ++ " selection range " ++
  show start ++ " " ++ show end
{-# INLINE tkSelectionRange #-}

tkCreateEntry :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                 [ConfigOption] -> TclScript
tkCreateEntry pnm kind name oid confs =
  tkDeclVar ("sv" ++ show oid) (show name) ++
  (createCmd defMethods) pnm kind name oid confs

tkCleanupEntry :: ObjectID -> ObjectName -> TclScript
tkCleanupEntry oid _ = []
{-# INLINE tkCleanupEntry #-}

tkSetText :: ObjectName -> GUIVALUE -> TclScript
tkSetText nm val = [show nm ++ " delete 0 end",
                    show nm ++ " insert 0 " ++ show val]
{-# INLINE tkSetText #-}

tkGetText :: ObjectName -> TclScript
tkGetText nm = [show nm ++ " get"]
{-# INLINE tkGetText #-}
