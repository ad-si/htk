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
-- HTk's <strong>editor widget</strong>.<br>
-- A text container for editing purposes. An editor widget can contain
-- text tags, to which you can make bindings, and also embedded windows.
module Editor (

  module Selection,
  module ICursor,
  module Index,

  ScrollBar,
  ScrollUnit,
  HasScroller(..),

  Editor,
  newEditor,

  deleteText,
  deleteTextRange,
  getTextRange,
  insertText,
  insertNewline,
  getTextLine,
  appendText,

  getIndexPosition,
  compareIndices,

  writeTextToFile,
  readTextFromFile,

  HasTabulators(..),
  HasLineSpacing(..),

  adjustViewTo,

  scanMark,
  scanDragTo,

  SearchDirection(..),
  SearchMode(..),
  SearchSwitch(..),
  search, 

  IndexModifiers(..),
  IndexModifier(..),

  WrapMode(..),
  wrap,
  getWrapMode

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Geometry
import BitMap
import ScrollBar
import Index
import Selection
import XSelection
import ICursor
import Char(isSpace)
import Keyboard
import Computation
import Destructible
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- Editor
-- -----------------------------------------------------------------------

---
-- The <code>Editor</code> datatpe.
newtype Editor a = Editor GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

---
-- Constructs a new editor widget and returns it as a value.
-- @param par     - the parent widget, which has to be a container widget
--                  (an instance of <code>class Container</code>).
-- @param cnf     - the list of configuration options for this editor.
-- @return result - An editor widget.
newEditor :: (Container par, GUIValue a) => par -> [Config (Editor a)] ->
                                            IO (Editor a)
newEditor par ol =
  do
    w <- createGUIObject (toGUIObject par) (TEXT cdefault) textMethods
    tp <- return (Editor w)
    configure tp ol
  where defvalue :: GUIValue a => Editor a -> a -> a
        defvalue tp a = a


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (Editor a) where 
---
-- Internal.
  toGUIObject (Editor w) = w
---
-- Internal.
  cname _                  = "Text"

---
-- An editor widget can be destroyed.
instance Destroyable (Editor a) where
---
-- Destroys a check button widget.
  destroy = destroy . toGUIObject

---
-- An editor widget has standard widget properties
-- (concerning focus, cursor).
instance Widget (Editor a)

---
-- A editor widget has a configureable border.
instance HasBorder (Editor a)

---
-- An editor widget has a foreground and background colour.
instance HasColour (Editor a) where 
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- You can specify the size of an editor widget.
instance HasSize (Editor a)

---
-- You can specify the font of an editor widget.
instance HasFont (Editor a)

---
-- A editor widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable (Editor a)

---
-- You can adjust the line spacing of an editor widget.
instance HasLineSpacing (Editor a)

instance HasTabulators (Editor a)

instance HasScroller (Editor a)

instance Synchronized (Editor a) where
  synchronize = synchronize . toGUIObject

instance GUIValue a => HasValue (Editor a) a where
  value val w = setTextLines w val >> return w
  getValue w = getTextLines w

instance HasTooltip (Editor a)


-- -----------------------------------------------------------------------
-- commands for getting and setting the text content
-- -----------------------------------------------------------------------

getTextLines :: GUIValue a => Editor a -> IO a
getTextLines tp =
  do
    start' <- getBaseIndex tp ((1,0) :: Position)
    end' <- getBaseIndex tp (EndOfText,BackwardChars 1)
    evalMethod tp (\nm -> tkGetText nm start' (Just end'))
  where wid = toGUIObject tp

setTextLines :: GUIValue a => Editor a -> a -> IO ()
setTextLines tp lns =
  do
    deleteTextRange tp ((1,0) :: Position) EndOfText
    start' <- getBaseIndex tp ((1,0) :: Position)
    execMethod tp (\nm -> tkInsertText nm start' val)
  where wid = toGUIObject tp
        val = toGUIValue lns


-- -----------------------------------------------------------------------
-- commands for reading and writing texts
-- -----------------------------------------------------------------------

deleteText :: HasIndex (Editor a) i BaseIndex => Editor a -> i -> IO ()
deleteText tp i =
  do
    pos <- getBaseIndex tp i
    execMethod tp (\nm -> tkDeleteText nm pos Nothing)

deleteTextRange :: (HasIndex (Editor a) i1 BaseIndex, 
                    HasIndex (Editor a) i2 BaseIndex) =>
                   Editor a -> i1 -> i2 -> IO ()
deleteTextRange tp start end =
  do
    start' <- getBaseIndex tp start
    end' <- getBaseIndex tp end
    execMethod tp (\nm -> tkDeleteText nm start' (Just end'))

getTextRange :: (HasIndex (Editor String) i1 BaseIndex, 
                 HasIndex (Editor String) i2 BaseIndex) =>
                Editor String -> i1 -> i2 -> IO String
getTextRange tp start end = 
  do
    start' <- getBaseIndex tp start
    end' <- getBaseIndex tp end
    evalMethod tp (\nm -> tkGetText nm start' (Just end'))

insertText :: (HasIndex (Editor String) i BaseIndex,GUIValue a) =>
              Editor String -> i -> a -> IO ()
insertText tp i txt = 
  do
    pos <- getBaseIndex tp i
    execMethod tp (\nm -> tkInsertText nm pos val)
  where val = toGUIValue txt

insertNewline   :: Editor String -> IO ()
insertNewline tp = execMethod tp (\nm -> tkInsertNewLine nm)

getTextLine     :: HasIndex (Editor String) i BaseIndex =>
                   Editor String -> i -> IO String
getTextLine tp i = 
  do
    (l,c) <- getIndexPosition tp i
    getTextRange tp (start l) (end l)
  where start l = (l,0::Distance)
        end l = ((l+1,0::Distance ),BackwardChars 1)

appendText :: Editor String -> String -> IO ()
appendText tp str =
  do
    try (insertText tp EndOfText str)
    moveto Vertical tp 1.0
    done


-- -----------------------------------------------------------------------
-- Editor to/from files
-- -----------------------------------------------------------------------

writeTextToFile :: Editor String -> FilePath -> IO ()
writeTextToFile tp fnm =
  do
    str <- getValue tp
    writeFile fnm str

readTextFromFile :: Editor String -> FilePath -> IO ()
readTextFromFile tp fnm =
  do
    str <- readFile fnm
    configure tp [value str]
    done


-- -----------------------------------------------------------------------
-- BBox
-- -----------------------------------------------------------------------

instance (HasIndex (Editor a) i BaseIndex) => HasBBox (Editor a) i  where
  bbox w i =
    do
      binx <- getBaseIndex w i
      ans <- try (evalMethod w (\nm -> [tkBBox nm (binx::BaseIndex)]))
      case ans of (Left e)  -> return Nothing
                  (Right v) -> return (Just v)
    where tkBBox nm i = show nm ++ " bbox " ++ show i


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

instance HasIndex (Editor a) BaseIndex BaseIndex where
  getBaseIndex w i = return i

instance HasIndex (Editor a) EndOfText BaseIndex where
  getBaseIndex w _ = return (IndexText "end")

instance HasIndex (Editor a) Pixels BaseIndex where
  getBaseIndex w p = return (IndexText (show p))

instance HasIndex (Editor a) (Distance, Distance) BaseIndex where
  getBaseIndex w pos = return (IndexPos pos)

instance HasIndex (Editor a) i BaseIndex => 
         HasIndex (Editor a) (i,[IndexModifier]) BaseIndex where
  getBaseIndex tp (i,ml) =
    do
      bi <- getBaseIndex tp i
      return
        (IndexText (show (bi::BaseIndex) ++ show (IndexModifiers ml)))

instance HasIndex (Editor a) i BaseIndex => 
         HasIndex (Editor a) (i,IndexModifier) BaseIndex where
  getBaseIndex tp (i,m) =
    do
      bi <- getBaseIndex tp i
      return (IndexText (show (bi::BaseIndex) ++ show m))

instance HasIndex (Editor a) i BaseIndex =>
         HasIndex (Editor a) i (Distance,Distance) where
  getBaseIndex = getIndexPosition 


-- -----------------------------------------------------------------------
-- Index modifiers
-- -----------------------------------------------------------------------

newtype IndexModifiers = IndexModifiers [IndexModifier]

data IndexModifier = 
          ForwardChars Int
        | BackwardChars Int
        | ForwardLines Int
        | BackwardLines Int
        | LineStart 
        | LineEnd 
        | WordStart 
        | WordEnd

instance Show IndexModifier where
   showsPrec d (ForwardChars counts) r = "+" ++ show counts ++ "chars " ++ r
   showsPrec d (BackwardChars counts) r = "-" ++ show counts ++ "chars " ++ r
   showsPrec d (ForwardLines counts) r = "+" ++ show counts ++ "lines " ++ r
   showsPrec d (BackwardLines counts) r = "-" ++ show counts ++ "lines " ++ r
   showsPrec d LineStart r = " linestart " ++ r
   showsPrec d LineEnd r = " lineend " ++ r
   showsPrec d WordStart r = " wordstart " ++ r
   showsPrec d WordEnd r = " wordend " ++ r

instance Show IndexModifiers where
   showsPrec d (IndexModifiers []) r = r
   showsPrec d (IndexModifiers (m:ml)) r = show m ++ " " ++ show (IndexModifiers ml) ++ r


-- -----------------------------------------------------------------------
-- Index operations
-- -----------------------------------------------------------------------

getIndexPosition :: HasIndex (Editor a) i BaseIndex 
                 => (Editor a) -> i -> IO Position
getIndexPosition tp i = do {
        inx <- getBaseIndex tp i;
        pos <- evalMethod tp (\nm -> tkPosition nm inx);
        case pos of
                (IndexPos pos) -> return pos
}

compareIndices :: (
        HasIndex (Editor a) i1 BaseIndex,
        HasIndex (Editor a) i2 BaseIndex
        ) => (Editor a) -> String -> i1 -> i2 -> IO Bool
compareIndices tp op i1 i2 = do
        bi1 <- getBaseIndex tp i1
        bi2 <- getBaseIndex tp i2
        evalMethod tp (\nm -> tkCompare nm op bi1 bi2)
 where  tkCompare :: ObjectName -> String -> BaseIndex -> BaseIndex -> TclScript
        tkCompare nm op i1 i2 = 
                [show nm ++ " compare " ++ show i1 ++ op ++ " " ++ " " ++ show i2] 

 
-- -----------------------------------------------------------------------
-- selection
-- -----------------------------------------------------------------------

instance HasSelection (Editor a) where
        clearSelection tp = synchronize tp (do {
                start <- getSelectionStart tp;
                end <- getSelectionEnd tp;
                case (start,end) of
                        (Just start,Just end) -> do {
                            start' <- getBaseIndex tp (start::Position);
                            end' <- getBaseIndex tp (end::Position);    
                            execMethod tp (\nm -> tkClearSelection nm start' end')
                            }
                        _ -> done
                })

instance (HasIndex (Editor a) i BaseIndex) => HasSelectionIndex (Editor a) i 
  where
        selection inx tp = synchronize tp (do {
                binx <- getBaseIndex tp inx;
                execMethod tp (\nm -> tkSelection nm binx);
                return tp
                })
        isSelected tp inx = synchronize tp (do {
                binx <- getBaseIndex tp inx;
                start <- getSelectionStart tp;
                end <- getSelectionEnd tp;
                case (start,end,binx) of 
                        (Just s,Just e,IndexPos i) -> return ((s <= i) && (i < e)) 
                        _                          -> return False
                })

instance HasSelectionBaseIndexRange (Editor a) (Distance,Distance) where
        getSelectionStart tp = do
                mstart <- try (evalMethod tp (\nm -> tkSelFirst nm))
                case mstart of
                        (Left e)  -> return Nothing -- actually a tk error
                        (Right v) -> (return . Just) v
        getSelectionEnd tp = do
                mstart <- try (evalMethod tp (\nm -> tkSelEnd nm))
                case mstart of
                        (Left e)  -> return Nothing -- actually a tk error
                        (Right v) -> (return . Just) v

instance (
        HasIndex (Editor a) i1 BaseIndex, 
        HasIndex (Editor a) i2 BaseIndex
        ) => HasSelectionIndexRange (Editor a) i1 i2 
  where
        selectionRange start end tp = synchronize tp (do {
                start' <- getBaseIndex tp start;
                end' <- getBaseIndex tp end;
                execMethod tp (\nm -> tkSelectionRange nm start' end');
                return tp
                })

instance HasSelectionBaseIndex (Editor a) ((Distance,Distance),(Distance,Distance)) where
        getSelection = getSelectionRange

instance HasXSelection (Editor a)


-- -----------------------------------------------------------------------
-- Insertion Cursor
-- -----------------------------------------------------------------------

instance HasInsertionCursor (Editor a)

instance ( HasIndex (Editor a) i BaseIndex
        ) => HasInsertionCursorIndexSet (Editor a) i 
  where
        insertionCursor inx tp =  synchronize tp (do {
                binx <- getBaseIndex tp inx;
                execMethod tp (\nm -> tkSetInsertMark nm binx);
                return tp
                })

instance HasInsertionCursorIndexGet (Editor a) (Distance,Distance) where
        getInsertionCursor tp =  evalMethod tp (\nm -> tkGetInsertMark nm)


-- -----------------------------------------------------------------------
-- View
-- -----------------------------------------------------------------------

adjustViewTo :: HasIndex (Editor a) i BaseIndex => (Editor a) -> i -> IO ()
adjustViewTo  tp i = 
        synchronize tp (do {
                inx <- getBaseIndex tp i;
                execMethod tp (\nm -> tkSee nm inx)
                })


-- -----------------------------------------------------------------------
-- Scan
-- -----------------------------------------------------------------------

scanMark :: HasIndex (Editor a) i BaseIndex => (Editor a) -> i -> IO ()
scanMark tp i = do {
        pos <- getIndexPosition tp i;
        execMethod tp (\nm -> tkScanMark nm pos)
}

scanDragTo :: HasIndex (Editor a) i BaseIndex => (Editor a) -> i -> IO ()
scanDragTo tp i = 
        synchronize tp (do {
                pos <- getIndexPosition tp i;
                execMethod tp (\nm -> tkScanDragTo nm pos)
                })


-- -----------------------------------------------------------------------
-- Wrap Mode
-- -----------------------------------------------------------------------

wrap :: WrapMode -> Config (Editor a) 
wrap d tp = cset tp "wrap" d

getWrapMode :: Editor a -> IO WrapMode
getWrapMode tp = cget tp "wrap"


-- -----------------------------------------------------------------------
--  WrapMode
-- -----------------------------------------------------------------------

data WrapMode = NoWrap | CharWrap | WordWrap deriving (Eq,Ord,Enum)

instance GUIValue WrapMode where
        cdefault = NoWrap

instance Read WrapMode where
   readsPrec p b =
     case dropWhile (isSpace) b of
        'n':'o':'n':'e':xs -> [(NoWrap,xs)]
        'c':'h':'a':'r':xs -> [(CharWrap,xs)]
        'w':'o':'r':'d':xs -> [(WordWrap,xs)]
        _ -> []

instance Show WrapMode where
   showsPrec d p r = 
      (case p of 
         NoWrap -> "none"  
         CharWrap -> "char"  
         WordWrap -> "word"  
        ) ++ r


-- -----------------------------------------------------------------------
-- tabulators
-- -----------------------------------------------------------------------

class GUIObject w => HasTabulators w where
        tabs            :: String -> Config w
        getTabs         :: w -> IO String
        tabs s w        = cset w "tabs" s
        getTabs w       = cget w "tabs"



-- -----------------------------------------------------------------------
-- Line Spacings
-- -----------------------------------------------------------------------

class GUIObject w => HasLineSpacing w where
        spaceAbove      :: Distance -> Config w
        getSpaceAbove   :: w -> IO Distance
        spaceWrap       :: Distance -> Config w
        getSpaceWrap    :: w -> IO Distance
        spaceBelow      :: Distance -> Config w
        getSpaceBelow   :: w -> IO Distance
        getSpaceAbove w = cget w "spacing1"
        spaceAbove d w  = cset w "spacing1" d
        getSpaceBelow w = cget w "spacing3" 
        spaceBelow d w  = cset w "spacing3" d
        spaceWrap d w   = cset w "spacing2" d
        getSpaceWrap w  = cget w "spacing2" 


-- -----------------------------------------------------------------------
-- Search Swithc
-- -----------------------------------------------------------------------

data SearchDirection = Forward | Backward deriving (Eq,Ord,Enum)
 
instance Show SearchDirection where 
  showsPrec d p r = 
      (case p of 
         Forward -> " -forward"  
         Backward -> " -backward"  
        ) ++ r

data SearchMode = Exact | Nocase deriving (Eq,Ord,Enum)

instance Show SearchMode where 
  showsPrec d p r = 
      (case p of 
         Exact -> " -exact"  
         Nocase -> " -nocase"  
        ) ++ r

data SearchSwitch = SearchSwitch {
                searchdirection :: SearchDirection,
                searchmode :: SearchMode,
                rexexp :: Bool
                }

instance Show SearchSwitch where
  showsPrec _ (SearchSwitch d m False) r = 
        show d ++ show m ++ r
  showsPrec _ (SearchSwitch d m True) r = 
        show d ++ show m ++ " -regexp " ++ r


-- -----------------------------------------------------------------------
-- Text Methods 
-- -----------------------------------------------------------------------

textMethods = defMethods {
                cleanupCmd = tkCleanupText,
                createCmd = tkCreateText
                }


-- -----------------------------------------------------------------------
-- Search
-- -----------------------------------------------------------------------

search :: HasIndex (Editor a) i BaseIndex => 
        (Editor a) -> SearchSwitch -> String -> i -> IO (Maybe BaseIndex) 
search tp switch ptn inx = do {
        binx <- getBaseIndex tp inx;
        (RawData mb) <- evalMethod tp (\nm -> tkSearch nm switch ptn binx);
        case dropWhile isSpace mb of
                ""  -> return Nothing
                s   -> creadTk s >>= return . Just
        }

tkSearch :: ObjectName -> SearchSwitch -> String -> BaseIndex -> TclScript
tkSearch nm switch ptn inx = 
        [show nm ++ " search " ++ show switch ++ " " ++ ptn ++ " " ++ show inx]


-- -----------------------------------------------------------------------
-- Unparsing of Text Pane
-- -----------------------------------------------------------------------

tkCreateText :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                [ConfigOption] -> TclScript
tkCreateText pnm kind@(TEXT lns) name oid confs = 
  tkDeclVar ("sv" ++ show oid) (show name) ++ 
  (createCmd defMethods) pnm kind name oid confs
{-# INLINE tkCreateText #-}

tkCleanupText :: ObjectID -> ObjectName -> TclScript
tkCleanupText oid _ = tkUndeclVar ("sv" ++ show oid)
{-# INLINE tkCleanupText #-}

tkDeleteText :: ObjectName -> BaseIndex -> Maybe BaseIndex -> TclScript
tkDeleteText name pl Nothing = 
        [show name ++ " delete " ++ ishow pl]
tkDeleteText name pl1 (Just pl2) = 
        [show name ++ " delete " ++ ishow pl1 ++ " " ++ ishow pl2]
{-# INLINE tkDeleteText #-}

tkGetText :: ObjectName -> BaseIndex -> Maybe BaseIndex -> TclScript
tkGetText name pl Nothing = 
        [show name ++ " get " ++ ishow pl]
tkGetText name pl1 (Just pl2) = 
        [show name ++ " get " ++ ishow pl1 ++ " " ++ ishow pl2]
{-# INLINE tkGetText #-}

tkInsertText :: ObjectName -> BaseIndex -> GUIVALUE -> TclScript
tkInsertText name pl val = 
        [show name ++ " insert " ++ ishow pl ++ " " ++ show val ++ " "]
{-# INLINE tkInsertText #-}

tkInsertNewLine :: ObjectName -> TclScript
tkInsertNewLine name = [show name ++ " insert end \\n"]
{-# INLINE tkInsertNewLine #-}

tkPosition :: ObjectName -> BaseIndex -> TclScript
tkPosition name pl = [show name ++ " index " ++ ishow pl]
{-# INLINE tkPosition #-}

tkSee :: ObjectName -> BaseIndex -> TclScript
tkSee name pl = [show name ++ " see " ++ ishow pl]
{-# INLINE tkSee #-}

tkScanMark :: ObjectName -> Position -> TclScript
tkScanMark name pos = [show name ++ " scan mark " ++ show pos]
{-# INLINE tkScanMark #-}

tkScanDragTo :: ObjectName -> Position -> TclScript
tkScanDragTo name pos = [show name ++ " scan dragto " ++ show pos]
{-# INLINE tkScanDragTo #-}

tkSetInsertMark :: ObjectName -> BaseIndex -> TclScript
tkSetInsertMark wn p = [show wn ++ " mark set insert " ++ ishow p]
{-# INLINE tkSetInsertMark #-}

tkGetInsertMark :: ObjectName -> TclScript
tkGetInsertMark wn = [show wn ++ "  index insert"]
{-# INLINE tkGetInsertMark #-}

tkSelection :: ObjectName -> BaseIndex -> TclScript
tkSelection wn i @ (IndexPos (x,y)) = [show wn ++ " tag add sel " ++ 
        ishow i ++ " " ++ show (IndexPos(x,(y + 1)))]
tkSelection wn _ = [show wn ++ " tag add sel end end"]
{-# INLINE tkSelection #-}

tkSelectionRange :: ObjectName -> BaseIndex ->  BaseIndex -> TclScript
tkSelectionRange wn start end = [show wn ++ " tag add sel " ++ 
        ishow start ++ " " ++ ishow end]
{-# INLINE tkSelectionRange #-}

tkSelFirst :: ObjectName -> TclScript
tkSelFirst wn = [show wn ++ " index sel.first "]
{-# INLINE tkSelFirst #-}

tkSelEnd :: ObjectName -> TclScript
tkSelEnd wn = [show wn ++ " index sel.last "]
{-# INLINE tkSelEnd #-}

tkClearSelection :: ObjectName -> BaseIndex ->  BaseIndex -> TclScript
tkClearSelection wn start end = [show wn ++ " tag remove sel " ++
        ishow start ++ " " ++ ishow end]
{-# INLINE tkClearSelection #-}

tkMarkCreate :: ObjectName -> String -> BaseIndex -> TclScript
tkMarkCreate tname mname ix =
        [show tname ++ " mark set " ++ show mname ++ " " ++ ishow ix]
{-# INLINE tkMarkCreate #-}

ishow :: BaseIndex -> String
ishow i = "{" ++ show i ++ "}"
