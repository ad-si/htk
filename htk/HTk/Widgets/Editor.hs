{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTk\'s /editor widget/.
-- A text container for editing purposes. An editor widget can contain
-- text tags, to which you can bind events, and also embedded windows.
module HTk.Widgets.Editor (

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

import Control.Exception
import Data.Char(isSpace)

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import HTk.Kernel.Geometry
import HTk.Widgets.ScrollBar
import HTk.Components.Selection
import HTk.Devices.XSelection
import HTk.Components.ICursor
import HTk.Components.Index

import Util.Computation
import Events.Destructible
import Events.Synchronized
import HTk.Kernel.Packer
import HTk.Kernel.Tooltip


-- -----------------------------------------------------------------------
-- Editor
-- -----------------------------------------------------------------------

-- | The @Editor@ datatpe.
newtype Editor = Editor GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new editor widget and returns it as a value.
newEditor :: Container par => par
   -- ^ the parent widget, which has to be a container widget
   -- (an instance of @class Container@).
   -> [Config Editor]
   -- ^ the list of configuration options for this editor.
   -> IO Editor
   -- ^ An editor widget.
newEditor par cnf =
  do
    w <- createGUIObject (toGUIObject par) (TEXT cdefault) textMethods
    tp <- return (Editor w)
    configure tp cnf
  where defvalue :: GUIValue a => Editor -> a -> a
        defvalue tp a = a


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject Editor where
  -- Internal.
  toGUIObject (Editor w) = w
  -- Internal.
  cname _                  = "Text"

-- | An editor widget can be destroyed.
instance Destroyable Editor where
  -- Destroys a check button widget.
  destroy = destroy . toGUIObject

-- | An editor widget has standard widget properties
-- (concerning focus, cursor).
instance Widget Editor

-- | An editor is also a container for widgets, because it can contain
-- widgets in embedded windows.
instance Container Editor

-- | A editor widget has a configureable border.
instance HasBorder Editor

-- | An editor widget has a foreground and background colour.
instance HasColour Editor where
  -- Internal.
  legalColourID = hasForeGroundColour

-- | You can specify the size of an editor widget.
instance HasSize Editor

-- | You can specify the font of an editor widget.
instance HasFont Editor

-- | A editor widget is a stateful widget, it can be enabled or
-- disabled.
instance HasEnable Editor

-- | You can adjust the line spacing of an editor widget.
instance HasLineSpacing Editor

-- | An editor widget has adjustable tab stops.
instance HasTabulators Editor

-- | An editor is a scrollable widget.
instance HasScroller Editor

-- | You can synchronize on an editor widget.
instance Synchronized Editor where
  -- Synchronizes on an editor widget.
  synchronize = synchronize . toGUIObject

-- | An editor widget has a value (its textual content).
instance GUIValue a => HasValue Editor a where
  -- Sets the editor\'s value.
  value val w = setTextLines w val >> return w
  -- Gets the editor\'s value.
  getValue w = getTextLines w

-- | An editor widget can have a tooltip.
instance HasTooltip Editor


-- -----------------------------------------------------------------------
-- commands for getting and setting the text content
-- -----------------------------------------------------------------------

getTextLines :: GUIValue a => Editor -> IO a
getTextLines tp =
  do
    start' <- getBaseIndex tp ((1,0) :: Position)
    end' <- getBaseIndex tp (EndOfText,BackwardChars 1)
    evalMethod tp (\nm -> tkGetText nm start' (Just end'))
  where wid = toGUIObject tp

setTextLines :: GUIValue a => Editor -> a -> IO ()
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

-- | Deletes the character at the specified index.
deleteText :: HasIndex Editor i BaseIndex => Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the concerned index.
   -> IO ()
   -- ^ None.
deleteText ed i =
  do
    pos <- getBaseIndex ed i
    execMethod ed (\nm -> tkDeleteText nm pos Nothing)

-- | Deletes the text in the specified range.
deleteTextRange :: (HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex) =>
   Editor
   -- ^ the start index.
   -> i1
   -- ^ the end index.
   -> i2
   -> IO ()
   -- ^ None.
deleteTextRange tp start end =
  do
    start' <- getBaseIndex tp start
    end' <- getBaseIndex tp end
    execMethod tp (\nm -> tkDeleteText nm start' (Just end'))

-- | Gets the text in the specified range.
getTextRange :: (HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex) =>
   Editor
   -- ^ the concerned editor widget.
   -> i1
   -- ^ the start index.
   -> i2
   -- ^ the end index.
   -> IO String
   -- ^ The editor\'s text in the specified range.
getTextRange ed start end =
  do
    start' <- getBaseIndex ed start
    end' <- getBaseIndex ed end
    evalMethod ed (\nm -> tkGetText nm start' (Just end'))

-- | Inserts the given text at the specified index.
insertText :: (HasIndex Editor i BaseIndex,GUIValue a) =>
   Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the index to insert the text.
   -> a
   -- ^ the text to insert.
   -> IO ()
   -- ^ None.
insertText ed i txt =
  do
    pos <- getBaseIndex ed i
    execMethod ed (\nm -> tkInsertText nm pos val)
  where val = toGUIValue txt

-- | Inserts a newline character at the end of the editor widget.
insertNewline   :: Editor
   -- ^ the concerned editor widget.
   -> IO ()
   -- ^ None.
insertNewline ed = execMethod ed (\nm -> tkInsertNewLine nm)

-- | Gets a text line from an editor widget.
getTextLine     :: HasIndex Editor i BaseIndex =>
   Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ an index in the requested text line.
   -> IO String
   -- ^ The requested line of text.
getTextLine tp i =
  do
    (l,c) <- getIndexPosition tp i
    getTextRange tp (start l) (end l)
  where start l = (l,0::Distance)
        end l = ((l+1,0::Distance ),BackwardChars 1)

-- | Appends text at the end of the editor widget.
appendText :: Editor
   -- ^ the concerned editor widget.
   -> String
   -- ^ the text to append.
   -> IO ()
   -- ^ None.
appendText ed str =
  do
    try (insertText ed EndOfText str)
      :: IO (Either SomeException ())
    moveto Vertical ed 1.0
    done


-- -----------------------------------------------------------------------
-- Editor to/from files
-- -----------------------------------------------------------------------

-- | Writes the contained text to a file.
writeTextToFile :: Editor
   -- ^ the concerned editor widget.
   -> FilePath
   -- ^ the name of the file.
   -> IO ()
   -- ^ None.
writeTextToFile ed fnm =
  do
    str <- getValue ed
    writeFile fnm str

-- | Reads a text from a file and inserts it into the editor pane.
readTextFromFile :: Editor
   -- ^ the concerned editor widget.
   -> FilePath
   -- ^ the name of the file.
   -> IO ()
   -- ^ None.
readTextFromFile ed fnm =
  do
    str <- readFile fnm
    configure ed [value str]
    done


-- -----------------------------------------------------------------------
-- BBox
-- -----------------------------------------------------------------------

-- | You can find out the bounding box of characters inside an editor
-- widget.
instance (HasIndex Editor i BaseIndex) => HasBBox Editor i  where
  -- Returns the bounding box of the character at the specified index.
  bbox w i =
    do
      binx <- getBaseIndex w i
      ans <- try (evalMethod w (\nm -> [tkBBox nm (binx::BaseIndex)]))
      case ans of
        Left (e :: SomeException)  -> return Nothing
        Right v -> return (Just v)
    where tkBBox nm i = show nm ++ " bbox " ++ show i


-- -----------------------------------------------------------------------
-- HasIndex
-- -----------------------------------------------------------------------

-- | A base index is a valid index for an editor widget.
instance HasIndex Editor BaseIndex BaseIndex where
  -- Internal.
  getBaseIndex w i = return i

-- | The @EndOfText@ index is a valid index for an editor widget.
instance HasIndex Editor EndOfText BaseIndex where
  -- Internal.
  getBaseIndex w _ = return (IndexText "end")

-- | A position in pixels is a valid index for an editor widget.
instance HasIndex Editor Pixels BaseIndex where
  -- Internal.
  getBaseIndex w p = return (IndexText (show p))

-- | A pair of line and character is a valid index for an editor widget.
instance HasIndex Editor (Distance, Distance) BaseIndex where
  -- Internal.
  getBaseIndex w pos = return (IndexPos pos)

-- | A pair of a valid index and a list of index modifiers is a valid index
-- for an editor widget.
instance HasIndex Editor i BaseIndex =>
         HasIndex Editor (i,[IndexModifier]) BaseIndex where
  -- Internal.
  getBaseIndex tp (i,ml) =
    do
      bi <- getBaseIndex tp i
      return
        (IndexText (show (bi::BaseIndex) ++ show (IndexModifiers ml)))

-- | A pair of a valid index and an index modifier is a valid index for an
-- editor widget.
instance HasIndex Editor i BaseIndex =>
         HasIndex Editor (i,IndexModifier) BaseIndex where
  -- Internal.
  getBaseIndex tp (i,m) =
    do
      bi <- getBaseIndex tp i
      return (IndexText (show (bi::BaseIndex) ++ show m))

-- | Internal.
instance HasIndex Editor i BaseIndex =>
         HasIndex Editor i (Distance,Distance) where
  -- Internal.
  getBaseIndex = getIndexPosition


-- -----------------------------------------------------------------------
-- Index modifiers
-- -----------------------------------------------------------------------

-- | The @IndexModifiers@ datatype.
newtype IndexModifiers = IndexModifiers [IndexModifier]

-- | The @IndexModifier@ datatype.
data IndexModifier =
          ForwardChars Int
        | BackwardChars Int
        | ForwardLines Int
        | BackwardLines Int
        | LineStart
        | LineEnd
        | WordStart
        | WordEnd

-- | Internal.
instance Show IndexModifier where
   -- Internal.
   showsPrec d (ForwardChars counts) r = "+" ++ show counts ++ "chars " ++ r
   showsPrec d (BackwardChars counts) r = "-" ++ show counts ++ "chars " ++ r
   showsPrec d (ForwardLines counts) r = "+" ++ show counts ++ "lines " ++ r
   showsPrec d (BackwardLines counts) r = "-" ++ show counts ++ "lines " ++ r
   showsPrec d LineStart r = " linestart " ++ r
   showsPrec d LineEnd r = " lineend " ++ r
   showsPrec d WordStart r = " wordstart " ++ r
   showsPrec d WordEnd r = " wordend " ++ r

-- | Internal.
instance Show IndexModifiers where
   -- Internal.
   showsPrec d (IndexModifiers []) r = r
   showsPrec d (IndexModifiers (m:ml)) r = show m ++ " " ++ show (IndexModifiers ml) ++ r


-- -----------------------------------------------------------------------
-- Index operations
-- -----------------------------------------------------------------------

-- | Returns the position on the text widget for a given index.
getIndexPosition :: HasIndex Editor i BaseIndex
   => Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the concerned index.
   -> IO Position
   -- ^ The requested position.
getIndexPosition ed i = do {
        inx <- getBaseIndex ed i;
        pos <- evalMethod ed (\nm -> tkPosition nm inx);
        case pos of
                (IndexPos pos) -> return pos
}

-- | Compares two indizes.
compareIndices :: (
   HasIndex Editor i1 BaseIndex,
   HasIndex Editor i2 BaseIndex
   ) => Editor
   -- ^ the concerned editor widget.
   -> String
   -- ^ an operation given as a string
   -> i1
   -- ^ the first index.
   -> i2
   -- ^ the second index.
   -> IO Bool
   -- ^ @True@ or @False@, depending on
   -- the given operation.
compareIndices ed op i1 i2 = do
        bi1 <- getBaseIndex ed i1
        bi2 <- getBaseIndex ed i2
        evalMethod ed (\nm -> tkCompare nm op bi1 bi2)
 where  tkCompare :: ObjectName -> String -> BaseIndex -> BaseIndex -> TclScript
        tkCompare nm op i1 i2 =
                [show nm ++ " compare " ++ show i1 ++ op ++ " " ++ " " ++ show i2]


-- -----------------------------------------------------------------------
-- selection
-- -----------------------------------------------------------------------

-- | You can select text inside an editor widget.
instance HasSelection Editor where
        -- Clears the editors selection.
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

-- | An editor widget\'s characters are selectable.
instance (HasIndex Editor i BaseIndex) => HasSelectionIndex Editor i
  where
        -- Selects the character at the specified index.
        selection inx tp = synchronize tp (do {
                binx <- getBaseIndex tp inx;
                execMethod tp (\nm -> tkSelection nm binx);
                return tp
                })
        -- Queries if the character at the specified index is selected.
        isSelected tp inx = synchronize tp (do {
                binx <- getBaseIndex tp inx;
                start <- getSelectionStart tp;
                end <- getSelectionEnd tp;
                case (start,end,binx) of
                        (Just s,Just e,IndexPos i) -> return ((s <= i) && (i < e))
                        _                          -> return False
                })

-- | You can select a text range inside an editor widget.
instance HasSelectionBaseIndexRange Editor (Distance,Distance) where
        -- Gets the start index of the editor\'s selection.
        getSelectionStart tp = do
                mstart <- try (evalMethod tp (\nm -> tkSelFirst nm))
                case mstart of
                        Left (e :: SomeException)  -> return Nothing -- actually a tk error
                        Right v -> return $ Just v
        -- Gets the end index of the editor\'s selection.
        getSelectionEnd tp = do
                mstart <- try (evalMethod tp (\nm -> tkSelEnd nm))
                case mstart of
                        Left (e :: SomeException)  -> return Nothing -- actually a tk error
                        Right v -> return $ Just v

-- | You can select a text range inside an editor widget.
instance (
        HasIndex Editor i1 BaseIndex,
        HasIndex Editor i2 BaseIndex
        ) => HasSelectionIndexRange Editor i1 i2
  where
        -- Sets the selection range inside the editor widget.
        selectionRange start end tp = synchronize tp (do {
                start' <- getBaseIndex tp start;
                end' <- getBaseIndex tp end;
                execMethod tp (\nm -> tkSelectionRange nm start' end');
                return tp
                })

-- | You can select a text range inside an editor widget.
instance HasSelectionBaseIndex Editor ((Distance,Distance),(Distance,Distance)) where
        -- Gets the selection range inside the editor widget.
        getSelection = getSelectionRange

-- | An editor widget has an X selection.
instance HasXSelection Editor


-- -----------------------------------------------------------------------
-- Insertion Cursor
-- -----------------------------------------------------------------------

-- | An editor widget has an insertion cursor.
instance HasInsertionCursor Editor

-- | The insertion cursor of an editor widget can be set by a base index.
instance ( HasIndex Editor i BaseIndex
        ) => HasInsertionCursorIndexSet Editor i
  where
        -- Sets the position of the insertion cursor.
        insertionCursor inx tp =  synchronize tp (do {
                binx <- getBaseIndex tp inx;
                execMethod tp (\nm -> tkSetInsertMark nm binx);
                return tp
                })

-- | You can get the position of the insertion cursor of an editor widget.
instance HasInsertionCursorIndexGet Editor (Distance,Distance) where
        -- Gets the position of the insertion cursor.
        getInsertionCursor tp =  evalMethod tp (\nm -> tkGetInsertMark nm)


-- -----------------------------------------------------------------------
-- View
-- -----------------------------------------------------------------------

-- | Adjusts the view so that the character at the specified position is
-- visible.
adjustViewTo :: HasIndex Editor i BaseIndex => Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the index to adjust the view to.
   -> IO ()
   -- ^ None.
adjustViewTo ed i =
        synchronize ed (do {
                inx <- getBaseIndex ed i;
                execMethod ed (\nm -> tkSee nm inx)
                })


-- -----------------------------------------------------------------------
-- Scan
-- -----------------------------------------------------------------------

-- | Anchor a scrolling operation.
scanMark :: HasIndex Editor i BaseIndex => Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the concerned index.
   -> IO ()
   -- ^ None.
scanMark ed i = do {
        pos <- getIndexPosition ed i;
        execMethod ed (\nm -> tkScanMark nm pos)
}

-- | Scroll based on a new position.
scanDragTo :: HasIndex Editor i BaseIndex => Editor
   -- ^ the concerned editor widget.
   -> i
   -- ^ the concerned index.
   -> IO ()
   -- ^ None.
scanDragTo ed i =
        synchronize ed (do {
                pos <- getIndexPosition ed i;
                execMethod ed (\nm -> tkScanDragTo nm pos)
                })


-- -----------------------------------------------------------------------
-- Wrap Mode
-- -----------------------------------------------------------------------

-- | Sets the editor\'s wrap mode.
wrap :: WrapMode -> Config Editor
wrap d tp = cset tp "wrap" d

-- | Gets the editor\'s wrap mode.
getWrapMode :: Editor -> IO WrapMode
getWrapMode tp = cget tp "wrap"


-- -----------------------------------------------------------------------
--  WrapMode
-- -----------------------------------------------------------------------

-- | The @WrapMode@ datatype.
data WrapMode = NoWrap | CharWrap | WordWrap deriving (Eq,Ord,Enum)

-- | Internal.
instance GUIValue WrapMode where
        -- Internal.
        cdefault = NoWrap

-- | Internal.
instance Read WrapMode where
   -- Internal.
   readsPrec p b =
     case dropWhile (isSpace) b of
        'n':'o':'n':'e':xs -> [(NoWrap,xs)]
        'c':'h':'a':'r':xs -> [(CharWrap,xs)]
        'w':'o':'r':'d':xs -> [(WordWrap,xs)]
        _ -> []

-- | Internal.
instance Show WrapMode where
   -- Internal.
   showsPrec d p r =
      (case p of
         NoWrap -> "none"
         CharWrap -> "char"
         WordWrap -> "word"
        ) ++ r


-- -----------------------------------------------------------------------
-- tabulators
-- -----------------------------------------------------------------------

-- | Widgets with adjustable tab stops instantiate the
-- @class HasTabulators@.
class GUIObject w => HasTabulators w where
        -- Sets the tab stops.
        tabs            :: String -> Config w
        -- Gets the tab stops.
        getTabs         :: w -> IO String
        tabs s w        = cset w "tabs" s
        getTabs w       = cget w "tabs"



-- -----------------------------------------------------------------------
-- Line Spacings
-- -----------------------------------------------------------------------

-- | Widgets with an adjustable line spacing instantiate the
-- @class HasLineSpacing@.
class GUIObject w => HasLineSpacing w where
        -- Sets the space above an unwrapped line.
        spaceAbove      :: Distance -> Config w
        -- Gets the space above an unwrapped line.
        getSpaceAbove   :: w -> IO Distance
        -- Sets the space above a wrapped line.
        spaceWrap       :: Distance -> Config w
        -- Gets the space above a wrapped line.
        getSpaceWrap    :: w -> IO Distance
        -- Sets the space below an unwrapped line.
        spaceBelow      :: Distance -> Config w
        -- Sets the space below an unwrapped line.
        getSpaceBelow   :: w -> IO Distance
        getSpaceAbove w = cget w "spacing1"
        spaceAbove d w  = cset w "spacing1" d
        getSpaceBelow w = cget w "spacing3"
        spaceBelow d w  = cset w "spacing3" d
        spaceWrap d w   = cset w "spacing2" d
        getSpaceWrap w  = cget w "spacing2"


-- -----------------------------------------------------------------------
-- Search Switch
-- -----------------------------------------------------------------------

-- | The @SearchDirection@ datatype.
data SearchDirection = Forward | Backward deriving (Eq,Ord,Enum)

-- | Internal.
instance Show SearchDirection where
  -- Internal.
  showsPrec d p r =
      (case p of
         Forward -> " -forward"
         Backward -> " -backward"
        ) ++ r

-- | The @SearchMode@ datatype.
data SearchMode = Exact | Nocase deriving (Eq,Ord,Enum)

-- | Internal.
instance Show SearchMode where
  -- Internal.
  showsPrec d p r =
      (case p of
         Exact -> " -exact"
         Nocase -> " -nocase"
        ) ++ r

-- | The @SearchSwitch@ datatype.
data SearchSwitch = SearchSwitch {
                searchdirection :: SearchDirection,
                searchmode :: SearchMode,
                rexexp :: Bool
                }

-- | Internal.
instance Show SearchSwitch where
  -- Internal.
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

-- | Searches for text inside an editor widget.
search :: HasIndex Editor i BaseIndex =>
   Editor
   -- ^ the concerned editor widget.
   -> SearchSwitch
   -- ^ the search switch.
   -> String
   -- ^ the searched text or regular expression.
   -> i
   -- ^ the start index.
   -> IO (Maybe BaseIndex)
   -- ^ The index of the first match (if successful).
search ed switch ptn inx = do {
        binx <- getBaseIndex ed inx;
        (RawData mb) <- evalMethod ed (\nm -> tkSearch nm switch ptn binx);
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
