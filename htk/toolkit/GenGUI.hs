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

  root,                  {- :: GenGUI -> IO Item -}
--  status,

  addItem,               {- :: Item -> NewItem -> IO Item -}
  children               {- :: Item -> IO [Item] -}
--  content,

-- addedItem

) where

import Property
import HTk
import Image
import Editor
import ScrollBox
import TreeList
import DragAndDrop
import RVar


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

{- - heterogeneous n-ary trees
   - can change: - items can be destroyed (for simplicity only if no
                                           subitems)
                 - can add new subitems
   - has external representation -}

data Name = Name { shortname :: Int -> String,
	           fullname  :: String }

type ItemIcon = IO Image

class (HasProperty i ItemIcon, HasProperty i Name) => CItem i


-----------------------------
-- external representation --
-----------------------------

data NewItem =
    forall i . CItem i => FolderItem i [NewItem]
  | forall i . CItem i => LeafItem i


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

          | Root (RVar (Maybe (GenGUI)))

isFolder :: Item -> Bool
isFolder (IntFolderItem _ _ _ _) = True
isFolder _ = False

newID :: (RVar Int) -> IO String
newID idref =
  do
    n <- getVar idref
    setVar idref (n + 1)
    return ("item" ++ show n)

toItem :: GenGUI -> NewItem -> IO Item
toItem  gui@(GenGUI _ _ _ _ idref _) it@(FolderItem _ ch) =
  do
    id <- newID idref
    intch <- mapM (toItem gui) ch
    intchref <- newRVar intch
    return (IntFolderItem gui id it intchref)
toItem gui@(GenGUI _ _ _ _ idref _) it@(LeafItem _) =
  do
    id <- newID idref
    return (IntLeafItem gui id it)
toItem _ _ = error "GenGUI: toItem called for root"

instance Eq Item where
  (IntFolderItem gui1 id1 _ _) == (IntFolderItem gui2 id2 _ _) =
    id1 == id2 && gui1 == gui2
  (IntLeafItem gui1 id1 _) == (IntLeafItem gui2 id2 _) =
    id1 == id2 && gui1 == gui2
  (Root mguiref1) == (Root mguiref2) = mguiref1 == mguiref2


--------------------
-- internal state --
--------------------

type InternalState = RVar [Item]

root :: GenGUI -> IO Item
root gui =
  do
    mguiref <- newRVar (Just gui)
    return (Root mguiref)

--status :: Item -> IO NewItem


--------------------------
-- type and constructor --
--------------------------

data GenGUI = GenGUI (TreeList Item)                       -- folders tree
                     (Notepad Item)                           -- dnd field
                     (Editor String)
                     Window                                 -- main window
                     (RVar Int)                         -- counter for ids
                     InternalState                            -- root item

newGenGUI :: IO GenGUI
newGenGUI =
  do
    main <- newHFBox []
    win <- window main [text "GenGUI"]
    objects <- newVFBox [parent main]
    intstate <- newRVar []
    idref <- newRVar 0
    guiref <- newRVar Nothing
    tl <- newTreeList Pretty cfun ifun
                      (newTreeListObject (Root guiref)
                                         "object root" Node)
                      [background "white", size (400, 220),
                       parent objects]
    np <- newNotepad True [size (400, 280), background "white",
                           parent objects]
    ed <- newEditor [state Disabled]
    edscr <- newScrollBox ed [parent main]
    let gui = (GenGUI tl np ed win idref intstate)
    setVar guiref (Just gui)
    return gui

----------------------------
-- exported functionality --
----------------------------

children :: Item -> IO [Item]
children (IntFolderItem _ _ _ chref) = getVar chref
children (Root mguiref) =
  do
    mgui <- getVar mguiref
    case mgui of
      (Just (GenGUI _ _ _ _ _ intstate)) ->
        do
          items <- getVar intstate
          return items
      _ -> return []
children _ = error "GenGUI: children called for a leaf"

addItem :: Item -> NewItem -> IO Item
addItem (IntFolderItem (gui@(GenGUI tl _ _ _ _ _)) id ext chref) newitem =
  do
    ch <- getVar chref
    item <- toItem gui newitem
    setVar chref (ch ++ [item])
    updateTreeList tl
    return item
addItem (Root mguiref) newitem =
  do
    mgui <- getVar mguiref
    case mgui of
      Just gui@(GenGUI tl _ _ _ _ intstate) ->
        do
          items <- getVar intstate
          item <- toItem gui newitem
          setVar intstate (items ++ [item])
          updateTreeList tl
          return item
      _ -> error "Root empty"

--content :: forall i . CItem i => Item -> i


------------
-- events --
------------

--addedItem :: Item -> IA NewItem


--------------------------------
-- treelist children function --
--------------------------------

toTreeListObjects :: [Item] -> IO [TreeListObject Item]
toTreeListObjects (it@(IntFolderItem gui id (FolderItem ext _) chref) :
                   items) =
  do
    rest <- toTreeListObjects items
    nm <- get ext
    return (newTreeListObject it (fullname nm) Node : rest)
toTreeListObjects _ = return []

cfun :: ChildrenFun Item
cfun obj =
  do
    ch <- children (getObjectValue obj)
    toTreeListObjects (filter isFolder ch)


---------------
-- instances --
---------------

instance Eq GenGUI where
  (GenGUI _ _ _ win1 _ _) == (GenGUI _ _ _ win2 _ _) = win1 == win2

instance GUIObject GenGUI where
  toGUIObject (GenGUI _ _ _ win _ _) = toGUIObject win
  cname _ = "GenGUI"

instance Destructible GenGUI where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Interactive GenGUI

instance ToplevelWindow GenGUI where
  iconify (GenGUI _ _ _ win _ _) = iconify win
  deiconify (GenGUI _ _ _ win _ _) = deiconify win
  withdraw (GenGUI _ _ _ win _ _) = withdraw win
  putWinOnTop (GenGUI _ _ _ win _ _) = putWinOnTop win
  putWinAtBottom (GenGUI _ _ _ win _ _) = putWinAtBottom win

instance Synchronized GenGUI where
  synchronize (GenGUI _ _ _ win _ _) = synchronize win


-- temp --

ifun _ = folderImg

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]
