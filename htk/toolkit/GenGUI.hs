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

module GenGUI (

  newGenGUI,
  GenGUI,

  NewItem(..),
  Item,

  Name(..),

  CItem(..),

  root,
--  status,

  addItem,
  children,
  content,

  addedItem,
  selectedItemInTreeList,
  focusedItemInTreeList,
  selectedItemInNotepad,
  droppedOnItemInNotepad,
  doubleClickInNotepad,
  rightClickInNotepad

) where

import HTk
import ScrollBox
import TreeList
import DragAndDrop
import ReferenceVariables
import Name
import Core


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

class CItem c where
  getName :: c -> IO Name
  getIcon :: c -> IO Image


-----------------------------
-- external representation --
-----------------------------

data CItem c => NewItem c = LeafItem c (Maybe Position)
                          | FolderItem c [NewItem c]
                                       (Maybe (Position, Bool))


-----------------------------------
-- internal type & functionality --
-----------------------------------

type Id = String

data CItem c => Item c =
    IntFolderItem (GenGUI c)                                 -- parent gui
                  Id                                        -- internal id
                  (NewItem c)                   -- external representation
                  (Ref [Item c])                               -- subitems
  | IntLeafItem (GenGUI c)                                   -- parent gui
                Id                                          -- internal id
                (NewItem c)                     -- external representation
  | Root Id (Ref (Maybe (GenGUI c)))

isItemFolder :: Item c -> Bool
isItemFolder (IntFolderItem _ _ _ _) = True
isItemFolder _ = False

isItemLeaf :: Item c -> Bool
isItemLeaf = not . isItemFolder

isRoot :: Item c -> Bool
isRoot (Root _ _) = True
isRoot _ = False

getGenGUI :: Item c -> IO (GenGUI c)
getGenGUI (IntFolderItem gui _ _ _) = return gui
getGenGUI (IntLeafItem gui _ _) = return gui
getGenGUI (Root _ mguiref) =
  do
    mgui <- getRef mguiref
    case mgui of Just gui -> return gui
                 _ -> error "GenGUI (getGenGUI) : root empty"

getItemImage :: CItem c => TreeListObject (Item c) -> IO Image
getItemImage tlobj =
  case (getObjectValue tlobj) of
    IntFolderItem _ _ (FolderItem c _ _) _ -> getIcon c {-do
                                                ioimg <- get ext
                                                ioimg-}
    IntLeafItem _ _ (LeafItem c _) -> getIcon c {-do
                                        ioimg <- get ext
                                        ioimg-}
    Root _ _ -> folderImg

newID :: (Ref Int) -> IO String
newID idref =
  do
    n <- getRef idref
    setRef idref (n + 1)
    return ("item" ++ show n)

toItem :: CItem c => GenGUI c -> NewItem c -> IO (Item c)
toItem  gui@(GenGUI _ _ _ _ _ idref _ _ _ _ _ _ _ _ _ _)
        it@(FolderItem _ ch _) =
  do
    id <- newID idref
    intch <- mapM (toItem gui) ch
    intchref <- newRef intch
    return (IntFolderItem gui id it intchref)
toItem gui@(GenGUI _ _ _ _ _ idref _ _ _ _ _ _ _ _ _ _)
       it@(LeafItem _ _) =
  do
    id <- newID idref
    return (IntLeafItem gui id it)
--toItem _ _ = error "GenGUI (toItem) : called for root"

instance CItem c => Eq (Item c) where
  (IntFolderItem gui1 id1 _ _) == (IntFolderItem gui2 id2 _ _) =
    id1 == id2 && gui1 == gui2
  (IntLeafItem gui1 id1 _) == (IntLeafItem gui2 id2 _) =
    id1 == id2 && gui1 == gui2
  (Root id1 _) == (Root id2 _) = id1 == id2
  _ == _ = False

--------------------
-- internal state --
--------------------

root :: CItem c => GenGUI c -> IO (Item c)
root gui@(GenGUI id _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
  do
    mguiref <- newRef (Just gui)
    return (Root id mguiref)

--status :: Item -> IO NewItem


--------------------------
-- type and constructor --
--------------------------

data CItem c => GenGUI c =
  GenGUI Id                                                      -- gui id
         (TreeList (Item c))                               -- folders tree
         (Notepad (Item c))                                   -- dnd field
         (Editor String)                         -- textual representation
         Toplevel                                           -- main window
         (Ref Int)                                      -- counter for ids
         (Ref (Maybe (Item c)))             -- displayed item (in notepad)
         (Ref Position)
         (Ref [Item c])                                       -- root item
         (Channel (Item c))                   -- item addition event queue
         (Channel (Maybe (Item c)))          -- treelist focus event queue
         (Channel (Maybe (Item c)))      -- treelist selection event queue
         (Channel (Item c, Bool))         -- notepad selection event queue
         (Channel (Item c, [Item c]))          -- notepad drop event queue
         (Channel (Item c))            -- notepad double click event queue
         (Channel [Item c])             -- notepad right click event queue


getNotepad :: CItem c => GenGUI c -> Notepad (Item c)
getNotepad (GenGUI _ _ np _ _ _ _ _ _ _ _ _ _ _ _ _) = np

newGenGUI :: CItem c => IO (GenGUI c)
newGenGUI =
  do
    main <- createToplevel [text "GenGUI"]
    intstate <- newRef []
    idref <- newRef 0
    id <- newID idref
    guiref <- newRef Nothing
    addmsgQ <- newChannel
    tlfocusmsgQ <- newChannel
    tlselmsgQ <- newChannel
    npselmsgQ <- newChannel
    npdropmsgQ <- newChannel
    npdoubleclmsgQ <- newChannel
    nprightclmsgQ <- newChannel
    (tl, np) <-
      (if tixAvailable then
         do
           objects <- newPanedWindow main Vertical []
           pane1 <- createPane objects [initsize 200] []
           pane2 <- createPane objects [initsize 300] []
           pack objects [Side AtLeft, Fill X, Expand On]
           tl <- newTreeList pane1 Pretty cfun getItemImage
                             (newTreeListObject (Root id guiref)
                                                "object root" Node)
                             [background "white"{-, size (500, 250)-}]
           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad pane2 Scrolled (12, 12)
                            [{-size (500, 280),-} background "white"]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           return (tl, np)
       else
         do
           objects <- newFrame main []
           pack objects [Side AtLeft, Fill X, Expand On]
           tl <- newTreeList objects Pretty cfun getItemImage
                             (newTreeListObject (Root id guiref)
                                                "object root" Node)
                             [background "white", size (500, 250)]
           pack tl [PadX 5, PadY 5]
           np <- newNotepad objects Scrolled (12, 12)
                            [size (500, 280), background "white"]
           pack np [PadX 5, PadY 5]
           return (tl, np))
    (edscr, ed) <- newScrollBox main (\par -> newEditor par [width 60]) []
    pack edscr [PadX 5, PadY 5, Fill Y, Expand On]
    appendText ed "textual representation of objects"
    ed # state Disabled
    posref <- newRef (0, 0)
    initItemPosition posref
    spawnEvent (forever ((receive (TreeList.selectionEvent tl) >>>=
                            tlObjectSelected tlselmsgQ posref) +>
                         (receive (DragAndDrop.selectionEvent np) >>>=
                            npItemSelected npselmsgQ) +>
                         (receive (focusEvent tl) >>>=
                            tlObjectFocused tlfocusmsgQ) +>
                         (receive (dropEvent np) >>>=
                            npDropEvent npdropmsgQ) +>
                         (receive (doubleClickEvent np) >>>=
                            npDoubleClick npdoubleclmsgQ) +>
                         (receive (rightClickEvent np) >>>=
                            npRightClick nprightclmsgQ)))
    displayref <- newRef Nothing
    let gui = (GenGUI id tl np ed main idref displayref posref intstate
                      addmsgQ tlfocusmsgQ tlselmsgQ npselmsgQ npdropmsgQ
                      npdoubleclmsgQ nprightclmsgQ)
    setRef guiref (Just gui)
    return gui

tlObjectSelected :: CItem c => Channel (Maybe (Item c)) -> Ref Position ->
                               Maybe (TreeListObject (Item c)) -> IO ()
tlObjectSelected tlselmsgQ posref mobj =
  let addNotepadItem :: CItem c => (Item c) -> IO ()
      addNotepadItem item@(IntLeafItem (GenGUI _ _ np _ _ _ _ _ _ _ _ _ _ 
                                               _ _ _)
                                       _ (LeafItem c _)) =
        do
          nm <- GenGUI.getName c
          img <- getIcon c
          pos <- getNewItemPosition posref
          createNotepadItem item np [position pos, photo img, name nm]
          done
  in case mobj of
       Nothing -> syncNoWait (send tlselmsgQ Nothing)
       Just obj ->
         let item = getObjectValue obj
         in do
              gui@(GenGUI _ _ _ _ _ _ displayref _ _ _ _ _ _ _ _ _) <-
                getGenGUI item
              (if isRoot item then done
               else syncNoWait (send tlselmsgQ (Just item)))
              synchronize gui
                (do
                   ch <- children item
                   clearNotepad (getNotepad gui)
                   initItemPosition posref
                   mapM addNotepadItem (filter isItemLeaf ch)
                   setRef displayref (Just item)
                   done)

tlObjectFocused :: Channel (Maybe (Item c)) ->
                   Maybe (TreeListObject (Item c)) ->
                   IO ()
tlObjectFocused tlfocusmsgQ mobj =
  case mobj of
    Just obj -> syncNoWait (send tlfocusmsgQ (Just (getObjectValue obj)))
    _ -> syncNoWait (send tlfocusmsgQ Nothing)

npItemSelected :: CItem c => Channel (Item c, Bool) ->
                             (NotepadItem (Item c), Bool) -> IO ()
npItemSelected npselmsgQ (npitem, b) =
  do
    item <- getItemValue npitem
    syncNoWait (send npselmsgQ (item, b))

npDropEvent :: CItem c => Channel (Item c, [Item c]) ->
                          (NotepadItem (Item c),
                           [NotepadItem (Item c)]) ->
                          IO ()
npDropEvent npdropmsgQ (npitem, npitems) =
  do
    item <- getItemValue npitem
    items <- mapM getItemValue npitems
    syncNoWait (send npdropmsgQ (item, items))

npDoubleClick :: CItem c => Channel (Item c) -> NotepadItem (Item c) ->
                            IO ()
npDoubleClick npdoubleclmsgQ npitem =
  do
    item <- getItemValue npitem
    syncNoWait (send npdoubleclmsgQ item)

npRightClick :: CItem c => Channel [Item c] -> [NotepadItem (Item c)] ->
                           IO ()
npRightClick npdoubleclmsgQ npitems =
  do
    items <- mapM getItemValue npitems
    syncNoWait (send npdoubleclmsgQ items)


------------------------------------
-- getting notepaditems positions --
------------------------------------

notepaddx :: Int
notepaddx = 90

notepaddy :: Int
notepaddy = 40

num_cols :: Int
num_cols = 5

initItemPosition :: Ref Position -> IO ()
initItemPosition posref = setRef posref (Distance (10 + div notepaddx 2),
                                         Distance (10 + div notepaddy 2))

getNewItemPosition :: Ref Position -> IO Position
getNewItemPosition posref =
  do
    (x, y) <- getRef posref
    (if x < Distance (10 + (num_cols - 1) * notepaddx) then
       setRef posref (x + Distance notepaddx, y)
     else setRef posref (Distance (10 + div notepaddx 2),
                         y + Distance notepaddy))
    return (x, y)


----------------------------
-- exported functionality --
----------------------------

children :: CItem c => Item c -> IO [Item c]
children (IntFolderItem _ _ _ chref) = getRef chref
children (Root _ mguiref) =
  do
    mgui <- getRef mguiref
    case mgui of
      (Just (GenGUI _ _ _ _ _ _ _ _ intstate _ _ _ _ _ _ _)) ->
        do
          items <- getRef intstate
          return items
      _ -> return []
children _ = error "GenGUI (children) : called for a leaf"

addItem :: CItem c => Item c -> NewItem c -> IO (Item c)
addItem par@(IntFolderItem (gui@(GenGUI _ tl np _ _ _ displayref posref _
                                        addmsgQ _ _ _ _ _ _))
                           _ _ chref) newitem =
  synchronize gui
    (do
       mditem <- getRef displayref
       ch <- getRef chref
       item <- toItem gui newitem
       setRef chref (ch ++ [item])
       syncNoWait (send addmsgQ item)
       (if isItemFolder item then
          do
            mkNode tl par
            nuch <- children item
            let nod = if (any isItemFolder nuch) then Node else Leaf
            case newitem of
              FolderItem c _ _ ->
                do
                  nm <- GenGUI.getName c
                  addTreeListObject tl par (newTreeListObject item
                                              (full nm) nod)
              LeafItem c _ ->
                do
                  nm <- GenGUI.getName c
                  addTreeListObject tl par (newTreeListObject item
                                              (full nm) nod)
        else done)
       case mditem of
         Just ditem -> if ditem == par then
                         case newitem of
                           LeafItem c _ ->
                             do
                               pos <- getNewItemPosition posref
                               nm <- GenGUI.getName c
                               img <- getIcon c
                               createNotepadItem item np
                                 [position pos, name nm, photo img]
                               done
                           _ -> done
                       else done
         _ -> done
       return item)
addItem par@(Root _ mguiref) newitem =
  do
    mgui <- getRef mguiref
    case mgui of
      Just gui@(GenGUI _ tl _ _ _ _ _ _ intstate addmsgQ _ _ _ _ _ _) ->
        synchronize gui
          (do
             items <- getRef intstate
             item <- toItem gui newitem
             setRef intstate (items ++ [item])
             chs <- children item
             syncNoWait (send addmsgQ item)
             (if isItemFolder item then
                do
                  ch <- children item
                  let nod = if (any isItemFolder ch) then Node else Leaf
                  case newitem of
                    FolderItem c _ _ ->
                      do
                        nm <- GenGUI.getName c
                        addTreeListObject tl par (newTreeListObject item
                                                    (full nm) nod)
                    LeafItem c _ ->
                      do
                        nm <- GenGUI.getName c
                        addTreeListObject tl par (newTreeListObject item
                                                    (full nm) nod)
              else done)
             return item)
      _ -> error "GenGUI (addItem) : root empty"
addItem _ _ = error "GenGUI (addItem) : called for a leaf"


content :: CItem c => (Item c) -> c
content (IntFolderItem _ _ (FolderItem c _ _) _) = c
content (IntLeafItem _ _ (LeafItem c _)) = c
content _ = error "GenGUI (content) : called for root"


------------
-- events --
------------

addedItem :: GenGUI c -> Event (Item c)
addedItem (GenGUI _ _ _ _ _ _ _ _ _ addmsgQ _ _ _ _ _ _) = receive addmsgQ

focusedItemInTreeList :: GenGUI c -> Event (Maybe (Item c))
focusedItemInTreeList (GenGUI _ _ _ _ _ _ _ _ _ _ tlfocusmsgQ _ _ _ _ _) =
  receive tlfocusmsgQ

selectedItemInTreeList :: GenGUI c -> Event (Maybe (Item c))
selectedItemInTreeList (GenGUI _ _ _ _ _ _ _ _ _ _ _ tlselmsgQ _ _ _ _) = 
  receive tlselmsgQ

selectedItemInNotepad :: GenGUI c -> Event (Item c, Bool)
selectedItemInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ npselmsgQ _ _ _) =
  receive npselmsgQ

droppedOnItemInNotepad :: GenGUI c -> Event (Item c, [Item c])
droppedOnItemInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ _ npdropmsgQ _ _) =
  receive npdropmsgQ

doubleClickInNotepad :: GenGUI c -> Event (Item c)
doubleClickInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ _ _
                             npdoubleclmsgQ _) = receive npdoubleclmsgQ

rightClickInNotepad :: GenGUI c -> Event [Item c]
rightClickInNotepad (GenGUI _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                            nprightclmsgQ) = receive nprightclmsgQ

--------------------------------
-- treelist children function --
--------------------------------

toTreeListObjects :: CItem c => [Item c] -> IO [TreeListObject (Item c)]
toTreeListObjects (it@(IntFolderItem gui id (FolderItem c _ _) chref) :
                   items) =
  do
    rest <- toTreeListObjects items
    nm <- GenGUI.getName c
    ch <- children it
    let nod = if (any isItemFolder ch) then Node else Leaf
    return (newTreeListObject it (full nm) nod : rest)
toTreeListObjects _ = return []

cfun :: CItem c => ChildrenFun (Item c)
cfun obj =
  do
    ch <- children (getObjectValue obj)
    toTreeListObjects (filter isItemFolder ch)


---------------
-- instances --
---------------

instance CItem c => Eq (GenGUI c) where
  (GenGUI _ _ _ _ main1 _ _ _ _ _ _ _ _ _ _ _) ==
    (GenGUI _ _ _ _ main2 _ _ _ _ _ _ _ _ _ _ _) = main1 == main2

instance GUIObject (GenGUI c) where
  toGUIObject (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) =
    toGUIObject main
  cname _ = "GenGUI"

instance CItem c => Destroyable (GenGUI c) where
  destroy = destroy . toGUIObject

instance CItem c => Window (GenGUI c) where
  iconify (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) = iconify main
  deiconify (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) = deiconify main
  withdraw (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) = withdraw main
  putWinOnTop (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) =
    putWinOnTop main
  putWinAtBottom (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) =
    putWinAtBottom main

instance CItem c => Synchronized (GenGUI c) where
  synchronize (GenGUI _ _ _ _ main _ _ _ _ _ _ _ _ _ _ _) =
    synchronize main


-- temp --

folderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAAP///////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAEACwAAAAADAAMAEADLUi6vCAihDjBIA9qaBWYAtBgkESFF6Cu7OWpI3myXlSW
3QPueTmZHcJgSCwmAAA7"]
