-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

---
-- HTk's <strong>entry field</strong>.<br>
-- A simple widget that displays an editable line of text.
module Entry (

  module Selection,
  module Index,
  module ICursor,

  Entry,

  newEntry,

  XCoord(..),

  showText,
  getShowText

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Geometry
import GUIValue
import ScrollBar
import Index
import Selection
import XSelection
import ICursor
import Computation
import Destructible
import Synchronized
import Packer
import TkVariables
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

---
-- The <code>Entry</code> datatype.
newtype Entry a = Entry GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new entry field and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this entry
--                  field.
-- @return result - An entry field.
newEntry :: (Container par, GUIValue a) =>
            par -> [Config (Entry a)] -> IO (Entry a)
newEntry par cnf =
  do
    wid <- createGUIObject (toGUIObject par) ENTRY entryMethods
    configure (Entry wid) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (Entry a) where 
---
-- Internal.
  toGUIObject (Entry  w) = w
---
-- Internal.
  cname _ = "Entry"

---
-- An entry field has a configureable width (height config is ignored).
instance HasSize (Entry a) where
  height _ w = return w
  getHeight w = return 1

---
-- An entry field can be destroyed.
instance Destroyable (Entry a) where
---
-- Destroys an entry field.
  destroy   = destroy . toGUIObject

---
-- An entry field has standard widget properties
-- (concerning focus, cursor).
instance Widget (Entry a)

---
-- An entry field has a configureable border.
instance HasBorder (Entry a)

---
-- An entry field has a foreground and background colour.
instance HasColour (Entry a) where
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- You can specify the font of an entry field.
instance HasFont (Entry a)

---
-- The value of an entry field is associated with a polymorphic variable.
instance HasVariable (Entry String) where
  variable var@(TkVariable oid) w =
    cset w "textvariable" ("v" ++ show oid) >> return w

---
-- An entry field has a value that is associated with a polymorphic
-- variable.
instance GUIValue a => HasValue (Entry a) a where
  value val w = execMethod w (\nm-> tkSetText nm (toGUIValue val)) >>
                return w
---
-- Selector for the value of an entry field.
-- @param w	  - the concerned entry field.
-- @return result - The concerned entry field.
  getValue w  = evalMethod w (\nm-> tkGetText nm)

---
-- An entry field has a configureable text justification.
instance HasJustify (Entry a)

---
-- An entry field is a stateful widget - it can be enabled of disabled.
instance HasEnable (Entry a)

---
-- An entry field is scrollable in horizontal direction.
instance HasScroller (Entry a) where
---
-- Internal.
  isWfOrientation _ Horizontal = True
  isWfOrientation _ Vertical   = False

---
-- You can synchronize on an entry field (in JAVA-style)
instance Synchronized (Entry a) where
---
-- Synchronizes on an entry field.
  synchronize w = synchronize (toGUIObject w)

---
-- An entry can have a tooltip (only displayed if you are using tixwish).
instance HasTooltip (Entry a)


-- -----------------------------------------------------------------------
-- index
-- -----------------------------------------------------------------------

data XCoord = XCoord Distance

instance Show XCoord where
   showsPrec d (XCoord x) r = "@"++show x ++ r


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

instance HasIndex (Entry a) Int BaseIndex where
  getBaseIndex w i = return (IndexNo i)

instance HasIndex (Entry a) BaseIndex BaseIndex where
  getBaseIndex w i = return i

instance HasIndex (Entry a) EndOfText BaseIndex where
  getBaseIndex w _ = return (IndexText "end")

instance HasIndex (Entry a) XCoord BaseIndex where
  getBaseIndex ent i = return (IndexText (show i))
        
instance HasIndex (Entry a) (ICursor (Entry a)) BaseIndex where
  getBaseIndex ent i = return (IndexText "insert")

instance HasIndex (Entry a) (Selection (Entry a),First) BaseIndex where
  getBaseIndex ent i = return (IndexText "sel.first")

instance HasIndex (Entry a) (Selection (Entry a),Last) BaseIndex where
  getBaseIndex ent i = return (IndexText "sel.last")

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

instance HasSelection (Entry a) where
  clearSelection ent =
    execMethod ent (\nm -> [show nm ++ " selection clear"])

instance HasIndex (Entry a) i BaseIndex =>
           HasSelectionIndex (Entry a) i where
  selection inx ent =
    synchronize ent
      (do
         binx <- getBaseIndex ent inx
         execMethod ent (\nm -> [tkSelection nm binx])
         return ent)
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

instance HasSelectionBaseIndex (Entry a) (Int,Int) where
  getSelection = getSelectionRange

instance (HasIndex (Entry a) i1 BaseIndex,
          HasIndex (Entry a) i2 BaseIndex) =>
         HasSelectionIndexRange (Entry a) i1 i2 where
  selectionRange start end ent =
    synchronize ent
      (do
         start' <- getBaseIndex ent start
         end' <- getBaseIndex ent end
         execMethod ent (\nm -> [tkSelectionRange nm start' end'])
         return ent)

instance HasSelectionBaseIndexRange (Entry a) Int where
  getSelectionStart ent =
    do
      mstart <-
        try (evalMethod ent (\nm -> [show nm ++ " index sel.first "]))
      case mstart of
        Left e -> return Nothing     -- actually a tk error
        Right v -> return (Just v)
  getSelectionEnd ent =
    do
      mend <-
        try (evalMethod ent (\nm -> [show nm ++ " index sel.last "]))
      case mend of
        Left e -> return Nothing    -- actually a tk error
        Right v -> return (Just v)

instance HasXSelection (Entry a)


-- -----------------------------------------------------------------------
-- insertion cursor
-- -----------------------------------------------------------------------

instance HasInsertionCursor (Entry a)

instance HasIndex (Entry a) i BaseIndex =>
           HasInsertionCursorIndexSet (Entry a) i where
  insertionCursor inx ent =
    synchronize ent
      (do
         binx <- getBaseIndex ent inx
         execMethod ent (\nm -> [tkSetInsert nm binx])
         return ent)

instance HasInsertionCursorIndexGet (Entry a) Int where
  getInsertionCursor ent = evalMethod ent (\nm -> [tkGetInsert nm])


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

---
-- Sets a character to display instead of contents (e.g. for password
-- fields).
-- @param ch	  - the character to display.
-- @param ent	  - the concerned entry field.
-- @return result - The concerned entry field.
showText :: GUIValue a => Char -> Entry a -> IO (Entry a)
showText ch ent = cset ent "show" [ch]

---
-- Gets the character to show instead of contents.
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
tkSetText nm val = [show nm ++ " insert 0 " ++ show val]
{-# INLINE tkSetText #-}

tkGetText :: ObjectName -> TclScript
tkGetText nm = [show nm ++ " get"]
{-# INLINE tkGetText #-}
