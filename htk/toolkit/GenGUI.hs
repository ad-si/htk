{- --------------------------------------------------------------------
 -
 - Module: GenGUI
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


module GenGUI (

  newGenGUI,
  GenGUI,

  NewItem(..),
  Item,

  Name(..),
  ItemIcon,

  CItem,
  HasProp(..),
  HasProperty,

  root,
--  status,

  addItem,
  children,
  content,
  contentD,

  addedItem,
  selectedItemInTreeList,
  focusedItemInTreeList,
  selectedItemInNotepad,
  droppedOnItemInNotepad

) where

import Property
import HTk
import Image
import Editor
import ScrollBox
import TreeList
import DragAndDrop
import RVar
import Channels
import Name
import Dynamic


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

{- - heterogeneous n-ary trees
   - can change: - items can be destroyed (for simplicity only if no
                                           subitems)
                 - can add new subitems
   - has external representation -}

type ItemIcon = IO Image

class (HasProperty i ItemIcon, HasProperty i Name) => CItem i


-----------------------------
-- external representation --
-----------------------------

data NewItem =
    forall i . (CItem i, Typeable i) => FolderItem i [NewItem]
  | forall i . (CItem i, Typeable i) => LeafItem i


-----------------------------------
-- internal type & functionality --
-----------------------------------

type Id = String

data Item = IntFolderItem GenGUI                             -- parent gui
                          Id                                -- internal id
                          NewItem               -- external representation
                          (RVar [Item])                        -- subitems

          | IntLeafItem GenGUI                               -- parent gui
                        Id                                  -- internal id
                        NewItem                 -- external representation

          | Root Id (RVar (Maybe (GenGUI)))

isItemFolder :: Item -> Bool
isItemFolder (IntFolderItem _ _ _ _) = True
isItemFolder _ = False

isItemLeaf :: Item -> Bool
isItemLeaf = not . isItemFolder

isRoot :: Item -> Bool
isRoot (Root _ _) = True
isRoot _ = False

getGenGUI :: Item -> IO GenGUI
getGenGUI (IntFolderItem gui _ _ _) = return gui
getGenGUI (IntLeafItem gui _ _) = return gui
getGenGUI (Root _ mguiref) =
  do
    mgui <- getVar mguiref
    case mgui of Just gui -> return gui
                 _ -> error "GenGUI (getGenGUI) : root empty"

getItemImage :: TreeListObject Item -> ItemIcon
getItemImage tlobj =
  case (getObjectValue tlobj) of
    IntFolderItem _ _ (FolderItem ext _) _ -> do
                                                ioimg <- get ext
                                                ioimg
    IntLeafItem _ _ (LeafItem ext) -> do
                                        ioimg <- get ext
                                        ioimg
    Root _ _ -> folderImg

newID :: (RVar Int) -> IO String
newID idref =
  do
    n <- getVar idref
    setVar idref (n + 1)
    return ("item" ++ show n)

toItem :: GenGUI -> NewItem -> IO Item
toItem  gui@(GenGUI _ _ _ _ _ idref _ _ _ _ _ _ _ _)
        it@(FolderItem _ ch) =
  do
    id <- newID idref
    intch <- mapM (toItem gui) ch
    intchref <- newRVar intch
    return (IntFolderItem gui id it intchref)
toItem gui@(GenGUI _ _ _ _ _ idref _ _ _ _ _ _ _ _) it@(LeafItem _) =
  do
    id <- newID idref
    return (IntLeafItem gui id it)
toItem _ _ = error "GenGUI (toItem) : called for root"

instance Eq Item where
  (IntFolderItem gui1 id1 _ _) == (IntFolderItem gui2 id2 _ _) =
    id1 == id2 && gui1 == gui2
  (IntLeafItem gui1 id1 _) == (IntLeafItem gui2 id2 _) =
    id1 == id2 && gui1 == gui2
  (Root id1 _) == (Root id2 _) = id1 == id2
  _ == _ = False

--------------------
-- internal state --
--------------------

type InternalState = RVar [Item]

root :: GenGUI -> IO Item
root gui@(GenGUI id _ _ _ _ _ _ _ _ _ _ _ _ _) =
  do
    mguiref <- newRVar (Just gui)
    return (Root id mguiref)

--status :: Item -> IO NewItem


--------------------------
-- type and constructor --
--------------------------

data GenGUI =
  GenGUI Id                                                      -- gui id
         (TreeList Item)                                   -- folders tree
         (Notepad Item)                                       -- dnd field
         (Editor String)                         -- textual representation
         Window                                             -- main window
         (RVar Int)                                     -- counter for ids
         (RVar (Maybe Item))                -- displayed item (in notepad)
         (RVar Position)
         InternalState                                        -- root item
         (MsgQueue Item)                      -- item addition event queue
         (MsgQueue (Maybe Item))             -- treelist focus event queue
         (MsgQueue (Maybe Item))         -- treelist selection event queue
         (MsgQueue (Item, Bool))          -- notepad selection event queue
         (MsgQueue (Item, [Item]))             -- notepad drop event queue

getNotepad :: GenGUI -> Notepad Item
getNotepad (GenGUI _ _ np _ _ _ _ _ _ _ _ _ _ _) = np

newGenGUI :: IO GenGUI
newGenGUI =
  do
    main <- newHFBox [pad Horizontal 22, pad Vertical 12]
    win <- window main [text "GenGUI"]
    objects <- newVFBox [parent main]
    intstate <- newRVar []
    idref <- newRVar 0
    id <- newID idref
    guiref <- newRVar Nothing
    addmsgQ <- newMsgQueue
    tlfocusmsgQ <- newMsgQueue
    tlselmsgQ <- newMsgQueue
    npselmsgQ <- newMsgQueue
    npdropmsgQ <- newMsgQueue
    tl <- newTreeList Pretty cfun getItemImage
                      (newTreeListObject (Root id guiref)
                                         "object root" Node)
                      [background "white", size (500, 250),
                       pad Vertical 5, pad Horizontal 5, parent objects]
    np <- newNotepad Scrolled (12, 12)
                     [size (500, 280), background "white",
                      pad Vertical 5, pad Horizontal 5, parent objects]
    ed <- newEditor [width 60]
    edscr <- newScrollBox ed [pad Horizontal 5, pad Vertical 5,
                              parent main]
    appendText ed "textual representation of objects"
    ed # state Disabled
    posref <- newRVar (0, 0)
    initItemPosition posref
    interactor (\i -> (TreeList.selectionEvent tl >>>=
                         tlObjectSelected tlselmsgQ posref) +>
                      (DragAndDrop.selectionEvent np >>>=
                         npItemSelected npselmsgQ) +>
                      (focusEvent tl >>>= tlObjectFocused tlfocusmsgQ) +>
                      (dropEvent np >>>= npDropEvent npdropmsgQ))
    displayref <- newRVar Nothing
    let gui = (GenGUI id tl np ed win idref displayref posref intstate
                      addmsgQ tlfocusmsgQ tlselmsgQ npselmsgQ npdropmsgQ)
    setVar guiref (Just gui)
    return gui

tlObjectSelected :: MsgQueue (Maybe Item) -> RVar Position ->
                    Maybe (TreeListObject Item) -> IO ()
tlObjectSelected tlselmsgQ posref mobj =
  let addNotepadItem :: Item -> IO ()
      addNotepadItem item@(IntLeafItem (GenGUI _ _ np _ _ _ _ _ _ _ _ _ _ 
                                               _)
                                       _ (LeafItem ext)) =
        do
          nm <- get ext
          ioimg <- get ext
          img <- ioimg
          pos <- getNewItemPosition posref
          newNotepadItem item np [position pos, photo img, name nm]
          done
  in case mobj of
       Nothing -> sendIO tlselmsgQ Nothing
       Just obj ->
         let item = getObjectValue obj
         in do
              gui@(GenGUI _ _ _ _ _ _ displayref _ _ _ _ _ _ _) <-
                getGenGUI item
              (if isRoot item then done
               else sendIO tlselmsgQ (Just item))
              synchronize gui
                (do
                   ch <- children item
                   clearNotepad (getNotepad gui)
                   initItemPosition posref
                   mapM addNotepadItem (filter isItemLeaf ch)
                   setVar displayref (Just item)
                   done)

tlObjectFocused :: MsgQueue (Maybe Item) -> Maybe (TreeListObject Item) ->
                   IO ()
tlObjectFocused tlfocusmsgQ mobj =
  case mobj of
    Just obj -> sendIO tlfocusmsgQ (Just (getObjectValue obj))
    _ -> sendIO tlfocusmsgQ Nothing

npItemSelected :: MsgQueue (Item, Bool) -> (NotepadItem Item, Bool) ->
                  IO ()
npItemSelected npselmsgQ (npitem, b) =
  do
    item <- getItemValue npitem
    sendIO npselmsgQ (item, b)

npDropEvent :: MsgQueue (Item, [Item]) ->
               (NotepadItem Item, [NotepadItem Item]) -> IO ()
npDropEvent npdropmsgQ (npitem, npitems) =
  do
    item <- getItemValue npitem
    items <- mapM getItemValue npitems
    sendIO npdropmsgQ (item, items)


------------------------------------
-- getting notepaditems positions --
------------------------------------

notepaddx :: Int
notepaddx = 90

notepaddy :: Int
notepaddy = 40

num_cols :: Int
num_cols = 5

initItemPosition :: RVar Position -> IO ()
initItemPosition posref = setVar posref (Distance (10 + div notepaddx 2),
                                         Distance (10 + div notepaddy 2))

getNewItemPosition :: RVar Position -> IO Position
getNewItemPosition posref =
  do
    (x, y) <- getVar posref
    (if x < Distance (10 + (num_cols - 1) * notepaddx) then
       setVar posref (x + Distance notepaddx, y)
     else setVar posref (Distance (10 + div notepaddx 2),
                         y + Distance notepaddy))
    return (x, y)


----------------------------
-- exported functionality --
----------------------------

children :: Item -> IO [Item]
children (IntFolderItem _ _ _ chref) = getVar chref
children (Root _ mguiref) =
  do
    mgui <- getVar mguiref
    case mgui of
      (Just (GenGUI _ _ _ _ _ _ _ _ intstate _ _ _ _ _)) ->
        do
          items <- getVar intstate
          return items
      _ -> return []
children _ = error "GenGUI (children) : called for a leaf"

addItem :: Item -> NewItem -> IO Item
addItem par@(IntFolderItem (gui@(GenGUI _ tl np _ _ _ displayref posref _
                                        addmsgQ _ _ _ _))
                           _ _ chref) newitem =
  synchronize gui
    (do
       mditem <- getVar displayref
       ch <- getVar chref
       item <- toItem gui newitem
       setVar chref (ch ++ [item])
       sendIO addmsgQ item
       (if isItemFolder item then
          do
            mkNode tl par
            nuch <- children item
            let nod = if (any isItemFolder nuch) then Node else Leaf
            case newitem of
              FolderItem ext _ ->
                do
                  nm <- get ext
                  addTreeListObject tl par (newTreeListObject item
                                              (full nm) nod)
              LeafItem ext ->
                do
                  nm <- get ext
                  addTreeListObject tl par (newTreeListObject item
                                              (full nm) nod)
        else done)
       case mditem of
         Just ditem -> if ditem == par then
                         case newitem of
                           LeafItem ext ->
                             do
                               pos <- getNewItemPosition posref
                               nm <- get ext
                               ioimg <- get ext
                               img <- ioimg
                               newNotepadItem item np [position pos,
                                                       name nm, photo img]
                               done
                           _ -> done
                       else done
         _ -> done
       return item)
addItem par@(Root _ mguiref) newitem =
  do
    mgui <- getVar mguiref
    case mgui of
      Just gui@(GenGUI _ tl _ _ _ _ _ _ intstate addmsgQ _ _ _ _) ->
        synchronize gui
          (do
             items <- getVar intstate
             item <- toItem gui newitem
             setVar intstate (items ++ [item])
             chs <- children item
             sendIO addmsgQ item
             (if isItemFolder item then
                do
                  ch <- children item
                  let nod = if (any isItemFolder ch) then Node else Leaf
                  case newitem of
                    FolderItem ext _ ->
                      do
                        nm <- get ext
                        addTreeListObject tl par (newTreeListObject item
                                                    (full nm) nod)
                    LeafItem ext ->
                      do
                        nm <- get ext
                        addTreeListObject tl par (newTreeListObject item
                                                    (full nm) nod)
              else done)
             return item)
      _ -> error "GenGUI (addItem) : root empty"
addItem _ _ = error "GenGUI (addItem) : called for a leaf"

{-
content :: forall i . CItem i => Item -> i
content (IntFolderItem _ _ (FolderItem i _) _) = i
content (IntFolderItem _ _ (LeafItem i) _) = i
content (IntLeafItem _ _ (FolderItem i _)) = i
content (IntLeafItem _ _ (LeafItem i)) = i
content _ = error "GenGUI (content) : called for root"
-}

content :: Item -> NewItem
content (IntFolderItem _ _ newitem _) = newitem
content (IntLeafItem _ _ newitem) = newitem
content _ = error "GenGUI (content) : called for root"

contentD :: Item -> Dynamic
contentD (IntFolderItem _ _ (FolderItem i _) _) = toDyn i
contentD (IntFolderItem _ _ (LeafItem i) _) = toDyn i
contentD (IntLeafItem _ _ (FolderItem i _)) = toDyn i
contentD (IntLeafItem _ _ (LeafItem i)) = toDyn i
contentD _ = error "GenGUI (content) : called for root"


------------
-- events --
------------

addedItem :: GenGUI -> IA Item 
addedItem (GenGUI _ _ _ _ _ _ _ _ _ addmsgQ _ _ _ _) =
  lift (receive addmsgQ)

focusedItemInTreeList :: GenGUI -> IA (Maybe Item)
focusedItemInTreeList (GenGUI _ _ _ _ _ _ _ _ _ _ tlfocusmsgQ _ _ _) =
  lift (receive tlfocusmsgQ)

selectedItemInTreeList :: GenGUI -> IA (Maybe Item)
selectedItemInTreeList (GenGUI _ _ _ _ _ _ _ _ _ _ _ tlselmsgQ _ _) = 
  lift (receive tlselmsgQ)

selectedItemInNotepad :: GenGUI -> IA (Item, Bool)
selectedItemInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ npselmsgQ _) =
  lift (receive npselmsgQ)

droppedOnItemInNotepad :: GenGUI -> IA (Item, [Item])
droppedOnItemInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ _ npdropmsgQ) =
  lift (receive npdropmsgQ)


--------------------------------
-- treelist children function --
--------------------------------

toTreeListObjects :: [Item] -> IO [TreeListObject Item]
toTreeListObjects (it@(IntFolderItem gui id (FolderItem ext _) chref) :
                   items) =
  do
    rest <- toTreeListObjects items
    nm <- get ext
    ch <- children it
    let nod = if (any isItemFolder ch) then Node else Leaf
    return (newTreeListObject it (full nm) nod : rest)
toTreeListObjects _ = return []

cfun :: ChildrenFun Item
cfun obj =
  do
    ch <- children (getObjectValue obj)
    toTreeListObjects (filter isItemFolder ch)


---------------
-- instances --
---------------

instance Eq GenGUI where
  (GenGUI _ _ _ _ win1 _ _ _ _ _ _ _ _ _) ==
    (GenGUI _ _ _ _ win2 _ _ _ _ _ _ _ _ _) = win1 == win2

instance GUIObject GenGUI where
  toGUIObject (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = toGUIObject win
  cname _ = "GenGUI"

instance Destructible GenGUI where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Interactive GenGUI

instance ToplevelWindow GenGUI where
  iconify (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = iconify win
  deiconify (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = deiconify win
  withdraw (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = withdraw win
  putWinOnTop (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = putWinOnTop win
  putWinAtBottom (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) =
    putWinAtBottom win

instance Synchronized GenGUI where
  synchronize (GenGUI _ _ _ _ win _ _ _ _ _ _ _ _ _) = synchronize win


-- temp --

folderImg = newImage [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAAP///////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAEACwAAAAADAAMAEADLUi6vCAihDjBIA9qaBWYAtBgkESFF6Cu7OWpI3myXlSW
3QPueTmZHcJgSCwmAAA7"]
