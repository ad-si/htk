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
-- HTk's <strong>listbox widget</strong> .<br>
-- A scrollable widget that displays a set of text lines with selection
-- functionality.
module ListBox (

  module Selection,
  module Index,

  ListBox,
  newListBox,
        
  SelectMode(..),
  selectMode,
  getSelectMode,

  activateElem,
  selectionAnchor,

  ListBoxElem(..),
  elemNotFound

) where

import List
import Core
import BaseClasses(Widget)
import Configuration
import ScrollBar
import Index
import Selection
import Char(isSpace)
import Focus
import XSelection
import Synchronized
import Computation
import Destructible
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

---
-- The <code>ListBox</code> datatype - parametrised over the type of
-- the list elements.
newtype ListBox a = ListBox GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new listbox widget and returns a handler.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this listbox
--                  widget.
-- @return result - A listbox widget.
newListBox :: (Container par, GUIValue a) => par ->
                                             [Config (ListBox a)] ->
                                             IO (ListBox a)
newListBox par cnf =
  do
    w <- createGUIObject (toGUIObject par) (LISTBOX []) lboxMethods 
    configure (ListBox w) cnf


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (ListBox a) where 
---
-- Internal.
   toGUIObject (ListBox w) = w
---
-- Internal.
   cname _ = "ListBox"

---
-- A listbox widget can be destroyed.
instance Destroyable (ListBox a) where
---
-- Destroys a listbox widget
   destroy = destroy . toGUIObject

---
-- A listbox widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (ListBox a)

---
-- You can synchronize on a listbox object (in JAVA style).
instance Synchronized (ListBox a) where
---
-- Synchronizes on a listbox object.
  synchronize = synchronize . toGUIObject

---
-- A listbox widget has a configureable border.
instance HasBorder (ListBox a)

---
-- A listbox widget has a foreground and background colour.
instance HasColour (ListBox a) where 
  legalColourID = hasForeGroundColour

---
-- A listbox is a stateful widget - it can be enabled or disabled.
instance HasEnable (ListBox a)

---
-- You can specify the font of a listbox.
instance HasFont (ListBox a)

instance HasGrid (ListBox a)

---
-- A listbox is a scrollable widget.
instance HasScroller (ListBox a)

---
-- You can specify the size of a listbox.
instance HasSize (ListBox a)

---
-- The value of a listbox is the list of the displayed objects (these
-- are instances of class <code>GUIValue</code> and therefore instances
-- of class <code>Show</code>)
instance (GUIValue a, GUIValue [a]) => HasValue (ListBox a) [a] where
  value vals w =
    execMethod w (\nm -> tkInsert nm 0 (map toGUIValue vals)) >> return w
---
-- Gets the list of displayed objects.
  getValue w = evalMethod w (\nm -> tkGet nm)

---
-- A listbox can have a tooltip (only displayed if you are using tixwish).
instance HasTooltip (ListBox a)


-- -----------------------------------------------------------------------
-- ListBox configurations
-- -----------------------------------------------------------------------

---
-- Sets the select mode of a listbox.
-- @param sm	  - the select mode to set.
-- @param lbox	  - the concerned listbox.
-- @return result - The concerned listbox.
selectMode :: GUIValue a => SelectMode -> ListBox a -> IO (ListBox a)
selectMode sm lbox = cset lbox "selectmode" sm

---
-- Gets the set select mode from a listbox.
-- @param lbox	  - the concerned listbox.
-- @return result - The current select mode.
getSelectMode :: GUIValue a => (ListBox a) -> IO SelectMode 
getSelectMode lbox = cget lbox "selectmode"


-- -----------------------------------------------------------------------
-- BBox
-- -----------------------------------------------------------------------

instance HasIndex (ListBox a) i Int => HasBBox (ListBox a) i  where
  bbox w i =
    do
      binx <- getBaseIndex w i
      ans <- try (evalMethod w (\nm -> [tkBBox nm (binx::Int)]))
      case ans of Left e  -> return Nothing
                  Right v -> return (Just v)
    where tkBBox nm i = show nm ++ " bbox " ++ show i


-- -----------------------------------------------------------------------
-- Index
-- -----------------------------------------------------------------------

data Eq a => ListBoxElem a = ListBoxElem a deriving Eq


-- -----------------------------------------------------------------------
-- Has Index
-- -----------------------------------------------------------------------

instance HasIndex (ListBox a) Int Int where
  getBaseIndex lb i = return i

instance HasIndex (ListBox a) EndOfText Int where
  getBaseIndex lb _ = getIndexNumber lb "end"

instance HasIndex (ListBox a) Pixels Int where
  getBaseIndex lb p = getIndexNumber lb (show p)

instance (Eq a,GUIValue a) => HasIndex (ListBox [a])
                                        (ListBoxElem a) Int where 
  getBaseIndex lb (ListBoxElem val) =
    do
      kind <- getObjectKind (toGUIObject lb)
      case kind of
        LISTBOX elems -> 
          case findIndex (\e -> show e == val') elems of
            Nothing  -> raise elemNotFound
            Just i -> return i
    where val' = show (toGUIValue val)

instance (Eq a, GUIValue a, GUIValue [a]) =>
         HasIndex (ListBox a) Int (ListBoxElem a) where 
  getBaseIndex lb i =
    synchronize lb
      (do
         elems <- getValue lb
         (if (i >= 0) && (i <= (length elems - 1)) then
            return (ListBoxElem (elems !! i))
          else
            raise elemNotFound))

getIndexNumber :: ListBox a -> String -> IO Int
getIndexNumber lb i =
  evalMethod lb (\lnm -> [show lnm ++ " index "  ++ i])


-- -----------------------------------------------------------------------
-- ListBox selection
-- -----------------------------------------------------------------------

instance HasSelection (ListBox a) where
  clearSelection lb = execMethod lb (\nm -> tkSelectionClearAll nm)

instance (HasIndex (ListBox a) i Int) =>
         HasSelectionIndex (ListBox a) i where
  selection i lb =
    synchronize lb
      (do
         binx <- getBaseIndex lb i
         execMethod lb (\ nm -> tkSelectionSetItem nm binx)
         return lb)
  isSelected lb i =
    synchronize lb
      (do
         binx <- getBaseIndex lb i
         evalMethod lb (\nm -> tkSelectionIncludes nm binx))

instance HasSelectionBaseIndex (ListBox a) [Int] where
  getSelection lb =
    do
      sel <- evalMethod lb (\ nm -> tkCurSelection nm)
      case (((map read) .words) sel) of
        [] -> return Nothing
        l -> return (Just l)

instance (HasIndex (ListBox a) i1 Int, HasIndex (ListBox a) i2 Int) =>
         HasSelectionIndexRange (ListBox a) i1 i2  where
  selectionRange start end lb =
    synchronize lb
      (do
         start' <- getBaseIndex lb start
         end' <- getBaseIndex lb end
         execMethod lb (\ nm -> tkSelectionSet nm start' end')
         return lb)
        
instance HasSelectionBaseIndexRange (ListBox a) Int where
  getSelectionStart lb =
    do
      sel <- getSelection lb
      case sel of
        Nothing -> return Nothing 
        Just (v:_) -> return (Just v)
  getSelectionEnd lb =
    do
      sel <- getSelection lb
      case sel of
        Nothing  -> return Nothing
        Just l -> (return . Just . head . reverse) l


-- -----------------------------------------------------------------------
-- Other ListBox operations
-- -----------------------------------------------------------------------

---
-- Activates the specified line.
-- @param lb	  - the concerned listbox.
-- @param i	  - the index of the line to activate.
-- @return result - Nothing.
activateElem :: HasIndex (ListBox a) i Int => ListBox a -> i -> IO ()
activateElem lb i  =
  synchronize lb
    (do
       binx <- getBaseIndex lb i
       execMethod lb (\ nm -> tkActivate nm binx))

---
-- Anchors the selection at the specified line.
-- @param lb	  - the concerned listbox.
-- @param i	  - the index of the line to anchor the selection at.
-- @return result - Nothing.
selectionAnchor :: HasIndex (ListBox a) i Int => ListBox a -> i -> IO ()
selectionAnchor lb i =
  synchronize lb
    (do
       binx <- getBaseIndex lb i
       execMethod lb (\nm -> tkSelectionAnchor nm binx)
       done)


-- -----------------------------------------------------------------------
-- SelectMode
-- -----------------------------------------------------------------------

data SelectMode =
  Single | Browse | Multiple | Extended deriving (Eq,Ord,Enum)

instance GUIValue SelectMode where
  cdefault = Single
        
instance Read SelectMode where
   readsPrec p b =
     case dropWhile (isSpace) b of
        's':'i':'n':'g':'l':'e':xs -> [(Single,xs)]
        'b':'r':'o':'w':'s':'e':xs -> [(Browse,xs)]
        'm':'u':'l':'t':'i':'p':'l':'e':xs -> [(Multiple,xs)]
        'e':'x':'t':'e':'n':'d':'e':'d':xs -> [(Extended,xs)]
        _ -> []

instance Show SelectMode where
   showsPrec d p r = 
      (case p of 
         Single -> "single"
         Browse -> "browse"
         Multiple -> "multiple"
         Extended -> "extended"
        ) ++ r


-- -----------------------------------------------------------------------
-- exceptions
-- -----------------------------------------------------------------------

elemNotFound :: IOError
elemNotFound = userError "listbox element not found"


-- -----------------------------------------------------------------------
-- ListBox methods
-- -----------------------------------------------------------------------

lboxMethods :: Methods
lboxMethods =
  defMethods{ cleanupCmd = tkCleanupListBox,
              createCmd = tkCreateListBox }

-- -----------------------------------------------------------------------
-- Tk commands
-- -----------------------------------------------------------------------

tkCreateListBox :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                   [ConfigOption] -> TclScript
tkCreateListBox parnm kind@(LISTBOX el) name oid confs =
  tkDeclVar ("sv" ++ show oid) (show name) ++
  (createCmd defMethods) parnm kind name oid confs ++
  tkCreateListBoxElems name el
{-# INLINE tkCreateListBox #-}

tkCleanupListBox :: ObjectID -> ObjectName -> TclScript
tkCleanupListBox oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupListBox #-}

tkCreateListBoxElems ::  ObjectName -> [GUIVALUE] -> TclScript
tkCreateListBoxElems name elems = 
        [show name ++ " insert 0 " ++ showElements elems]
{-# INLINE tkCreateListBoxElems #-}

showElements :: [GUIVALUE] -> String
showElements = concatMap (++ " ") . (map show) 
{-# INLINE showElements #-}

tkActivate :: ObjectName -> Int -> TclScript
tkActivate name inx = [show name ++ " activate " ++ show inx]
{-# INLINE tkActivate #-}

tkCurSelection :: ObjectName -> TclScript
tkCurSelection name = [show name ++ " curselection "]
{-# INLINE tkCurSelection #-}

tkDelete :: ObjectName -> String -> String -> TclCmd
tkDelete name first last = show name ++ " delete " ++ first ++ " " ++ last
{-# INLINE tkDelete #-}

tkInsert ::  ObjectName -> Int -> [GUIVALUE] -> TclScript
tkInsert name inx elems = 
  [tkDelete name "0" "end",
   show name ++ " insert " ++ show inx ++ " " ++ showElements elems]
{-# INLINE tkInsert #-}

tkGet :: ObjectName -> TclScript
tkGet name = [show name ++ " get"]
{-# INLINE tkGet #-}

tkSelectionAnchor :: ObjectName -> Int -> TclScript
tkSelectionAnchor name inx =
  [show name ++ " selection anchor " ++ show inx]
{-# INLINE tkSelectionAnchor #-}

tkSelectionIncludes :: ObjectName -> Int -> TclScript
tkSelectionIncludes name inx = 
  [show name ++ " selection includes " ++ show inx]  
{-# INLINE tkSelectionIncludes #-}

tkSelectionClear :: ObjectName -> Int -> Int -> TclScript
tkSelectionClear name first last = 
  [show name ++ " selection clear " ++ show first ++ " " ++ show last]
{-# INLINE tkSelectionClear #-}

tkSelectionClearAll :: ObjectName -> TclScript
tkSelectionClearAll name = [show name ++ " selection clear 0 end"]
{-# INLINE tkSelectionClearAll #-}

tkSelectionSet :: ObjectName -> Int -> Int -> TclScript
tkSelectionSet name first last = 
  [show name ++ " selection set " ++ show first ++ " " ++ show last]
{-# INLINE tkSelectionSet #-}

tkSelectionSetItem :: ObjectName -> Int -> TclScript
tkSelectionSetItem name first = 
  [show name ++ " selection set " ++ show first]
{-# INLINE tkSelectionSetItem #-}
