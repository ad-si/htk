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

  GenGUI,        -- type
  newGenGUI, {- :: CItem c => Maybe (GenGUIState c) -> IO (GenGUI c)    -}

  setStatus,      {- :: CItem c => GenGUI c-> String-> IO ()            -}
  clearStatus,    {- :: CItem c => GenGUI c-> IO ()                     -}
  updateTextArea, {- :: CItem c => GenGUI c-> [MarkupText] -> IO ()     -}
  clearTextArea,  {- :: CItem c => GenGUI c-> IO ()                     -}
  genGUIMainMenu, {- :: CItem c => GenGUI c-> Menu                      -}

  NewItem(..),   -- external object representation
  Item,          -- internal object representation

  Name(..),
  CItem(..),

  root,     {- :: CItem c => GenGUI c -> IO (Item c)                    -}
  openedFolder, {- :: CItem c=> GenGUI c-> IO (Maybe (Item c))          -}
  addItem,  {- :: CItem c => Item c -> NewItem c -> IO (Item c)         -}
  children, {- :: CItem c => Item c -> IO [Item c]                      -}
  content,  {- :: CItem c => (Item c) -> c                              -}

  GenGUIEvent(..),
  bindGenGUIEv, {- :: CItem c => GenGUI c ->                            -}
                {-               IO (Event (GenGUIEvent c), IO())       -}

  GenGUIState,   -- representation of the gui's state
  exportGenGUIState, {- :: CItem c => GenGUI c -> IO (GenGUIState c)    -}
  importGenGUIState  {- :: CItem c => GenGUI c -> GenGUIState c -> IO ()-}

) where

import HTk
import ScrollBox
import TreeList
import Notepad
import ReferenceVariables
import Name
import Core
import List
import MarkupText
import PrelBase(not)
import CItem
import Maybe


--------------------------------------------------------------------------
-- external representation
--------------------------------------------------------------------------

data CItem c => NewItem c =

    LeafItem c (Maybe (Position,     -- position on notepad
                       Bool          -- selected in notepad
                      ))
  | FolderItem c [NewItem c] (Maybe (Bool, -- open in TreeList
                                           -- (recover only)
                                     Bool  -- displayed in notepad
                                    ))

isNewItemFolder :: CItem c => NewItem c -> Bool
isNewItemFolder (FolderItem _ _ _) = True
isNewItemFolder _ = False


--------------------------------------------------------------------------
-- internal type & functionality
--------------------------------------------------------------------------

data CItem c => Item c =
    IntFolderItem (NewItem c)                   -- external representation
                  (Ref [Item c])                               -- subitems
  | IntLeafItem (NewItem c)                     -- external representation
                (Ref Position)                      -- position on notepad
                (Ref Bool)                          -- selected on notepad
  | Root (Ref [Item c])

instance CItem c => Eq (Item c) where
  item1 == item2 = content item1 == content item2

instance CItem c => CItem (Item c) where
  getName = getName . content
  getIcon = getIcon . content

isItemFolder :: Item c -> Bool
isItemFolder (IntFolderItem _ _) = True
isItemFolder _ = False

isItemLeaf :: Item c -> Bool
isItemLeaf = PrelBase.not . isItemFolder

toItem :: CItem c => NewItem c -> IO (Item c)
toItem it@(FolderItem _ ch _) =
  do
    intch <- mapM toItem ch
    intchref <- newRef intch
    return (IntFolderItem it intchref)
toItem it@(LeafItem _ Nothing) =
  do
    posref <- newRef (-1, -1)
    selref <- newRef False
    return (IntLeafItem it posref selref)
toItem it@(LeafItem _ (Just (pos, selected))) =
  do
    posref <- newRef pos
    selref <- newRef selected
    return (IntLeafItem it posref selref)


--------------------------------------------------------------------------
-- external handle for root object
--------------------------------------------------------------------------

root :: CItem c => GenGUI c -> IO (Item c)
root gui = return (Root (root_obj gui))


--------------------------------------------------------------------------
-- state import/export
--------------------------------------------------------------------------

type GenGUIState c = [NewItem c]

exportGenGUIState :: CItem c => GenGUI c -> IO (GenGUIState c)
exportGenGUIState gui =
  do
    saveNotepadItemStates gui
    items <- getRef (root_obj gui)
    mopenobj <- getRef (open_obj gui)
    export items mopenobj
  where export (item@(IntFolderItem (FolderItem c _ _) subitemsref) :
                items) mopenobj =
          do
            subitems <- getRef subitemsref
            subnewitems <- export subitems mopenobj
            is_open <- isTreeListObjectOpen (treelist gui) item
            rest <- export items mopenobj
            return (FolderItem c subnewitems
                               (Just (is_open, Just item == mopenobj)) :
                    rest)
        export (IntLeafItem (LeafItem c _) posref selref : items)
               mopenobj =
          do
            pos <- getRef posref
            selected <- getRef selref
            rest <- export items mopenobj
            return (LeafItem c (Just (pos, selected)) : rest)
        export _ _ = return []

importGenGUIState :: CItem c => GenGUI c -> GenGUIState c -> IO ()
importGenGUIState _ _ = done  -- Scheitert leider während das GUI läuft
                              -- noch an den Treelisten (bug!).
                              -- GenGUI kann aber bereits mit State
                              -- erzeugt werden!


--------------------------------------------------------------------------
-- type and constructor
--------------------------------------------------------------------------

data CItem c => GenGUI c =
  GenGUI
    { -- the treelist
      treelist :: TreeList (Item c),

      -- the notepad
      notepad :: Notepad (Item c),

      -- the markup text container
      editor :: Editor String,

      -- the status bar
      status :: Label String,

      -- GenGUI's main menu
      topmenu :: Menu,

      -- GenGUI's toplevel window
      win :: Toplevel,

      -- item displayed in notepad
      open_obj :: Ref (Maybe (Item c)),

      -- temporary, needed for current stupid placement of notepad items
      place :: Ref Position,

      -- internal state
      root_obj :: Ref [Item c],

      -- events
      event_queue :: Ref (Maybe (Channel (GenGUIEvent c))) }

newGenGUI :: CItem c => Maybe (GenGUIState c) -> IO (GenGUI c)
newGenGUI mstate =
  do
    -- main window
    main <- createToplevel [text "GenGUI"]

    -- GenGUI menubar
    menubar <- createMenu main False []
    main # menu menubar

    -- references
    intstate <- case mstate of
                  Just state -> do
                                  state <- mapM toItem state
                                  newRef state
                  Nothing -> newRef []
    displayref <- newRef Nothing

    -- construct main widgets
    let constructTreeListState intend
                               (newitem@(FolderItem c subnewitems
                                                    (Just (open,
                                                           selected))) :
                                newitems) =
          do
            subtreelistitems <- if open then
                                  constructTreeListState (intend + 1)
                                                         subnewitems
                                else return []
            rest <- constructTreeListState intend newitems
            item <- toItem newitem

            if selected then setRef displayref (Just item) else done
            -- side effect: set reference for item displayed in notepad

            return ([TreeListExportItem
                       { TreeList.obj_val = item,
                         obj_type =
                           if (any isNewItemFolder subnewitems) then Node
                           else Leaf,
                         open = open,
                         intend = intend,
                         TreeList.selected = selected }] ++
                    subtreelistitems ++ rest)
        constructTreeListState intend (_ : newitems) =
          constructTreeListState intend newitems
        constructTreeListState _ _ = return []

    stlab <- newLabel main [text "Welcome", relief Sunken,
                            HTk.font (Lucida, 12::Int)]
    pack stlab [Side AtBottom, PadX 5, PadY 2, Fill X]


    treeliststate <-
      case mstate of
        Just state -> do
                        tlstate <- constructTreeListState 0 state
                        return (Just tlstate)
        Nothing -> return Nothing

    (tl, np, edscr, ed) <-
      (if tixAvailable then
         do
           objects_n_editor <- newPanedWindow main Horizontal []
           paneh1 <- createPane objects_n_editor [initsize 430] []
           paneh2 <- createPane objects_n_editor [initsize 370] []
           objects <- newPanedWindow paneh1 Vertical []
           panev1 <- createPane objects [initsize 220] []
           panev2 <- createPane objects [initsize 220] []
           pack objects [Fill Both, Expand On]
           pack objects_n_editor [Fill Both, Expand On]
           tl <- case treeliststate of
                   Just state ->
                     recoverTreeList panev1 cfun state
                                     [background "white"]
                   _ -> newTreeList panev1 cfun [] [background "white"]
           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad panev2 Scrolled (12, 12) Nothing
                            [background "white", size (800, 800)]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           (edscr, ed) <- newScrollBox paneh2
                            (\par -> newEditor par [width 40]) []
           pack edscr [PadX 6, PadY 6, Fill Both, Expand On]
           return (tl, np, edscr, ed)
       else
         do
           objects <- newFrame main []
           pack objects [Side AtLeft, Fill Both, Expand On]
           tl <- case treeliststate of
                   Just state -> recoverTreeList objects cfun state
                                   [background "white", size (380, 200)]
                   _ -> newTreeList objects cfun []
                          [background "white", size (380, 200)]
           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad objects Scrolled (12, 12) Nothing
                            [size (800, 800), background "white"]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           (edscr, ed) <- newScrollBox main
                            (\par -> newEditor par [width 40]
                                       :: IO (Editor String)) []
           pack edscr [PadX 5, PadY 5, Fill Both, Expand On]
           return (tl, np, edscr, ed))

    ed # state Disabled

    -- temporary (placement of objects in notepad / TD)
    posref <- newRef (0, 0)
--    initItemPosition posref

    -- event queue
    evq <- newRef Nothing

    -- GenGUI value
    let gui = GenGUI { treelist = tl,
                       notepad = np,
                       editor = ed,
                       status = stlab,
                       topmenu = menubar,
                       win = main,
                       open_obj = displayref,
                       place = posref,
                       root_obj = intstate,
                       event_queue = evq }

    -- listening events
    clipboard <- newRef ((0,0), [])   -- drop on editor or treelist
    (enter_ed, _) <- bind ed [WishEvent [] Enter]
    (leave_np, _) <- bind np [WishEvent [] Leave]

    (np_ev, _) <- bindNotepadEv np
    (tl_ev, _) <- bindTreeListEv tl

    spawnEvent (forever ((do
                            ev <- np_ev
                            always
                              (case ev of
                                 Notepad.Selected c ->
                                   npItemSelected gui (c, True)
                                 Deselected c ->
                                   npItemSelected gui (c, False)
                                 Notepad.Dropped inf ->
                                   npDropEvent gui inf
                                 Notepad.Doubleclick inf ->
                                   npDoubleClick gui inf
                                 Notepad.Rightclick inf ->
                                   npRightClick gui inf
                                 _ -> done)) +>
                         (do
                            ev <- tl_ev
                            always
                              (case ev of
                                 TreeList.Selected mobj ->
                                   tlObjectSelected gui mobj
                                 Focused mobjninf ->
                                   tlObjectFocused gui clipboard mobjninf
                                 _ -> done)) +>
                         (do
                            ev_inf <- enter_ed
                            always
                              (do
                                 ((x, y), items) <- getRef clipboard
                                 (if x == xRoot ev_inf &&
                                     y == yRoot ev_inf then
                                    do
                                      putStrLn "drag and drop action"
                                      sendEv gui (DroppedOnTextArea items)
                                      undoLastMotion np
                                  else done))) +>
                         (do
                            ev_inf <- leave_np
                            always
                              (do
                                 selected_notepaditems <-
                                   getSelectedItems np
                                 selected_items <-
                                   mapM getItemValue selected_notepaditems
                                 setRef clipboard
                                   ((xRoot ev_inf, yRoot ev_inf),
                                    selected_items)))))

    ditem <- getRef displayref
    case ditem of
      Just item ->
        tlObjectSelected gui (Just (newTreeListObject item Node))
      _ -> done

    return gui


--------------------------------------------------------------------------
-- internal event handling
--------------------------------------------------------------------------

saveNotepadItemStates :: CItem c => GenGUI c -> IO ()
saveNotepadItemStates gui =
  do
    npitems <- getItems (notepad gui)
    mapM (saveNotepadItemState gui) npitems
    done

saveNotepadItemState :: CItem c => GenGUI c -> NotepadItem (Item c) ->
                                   IO ()
saveNotepadItemState gui npitem =
  do
    IntLeafItem _ posref selref <- getItemValue npitem
    pos <- getPosition npitem
    sel <- isNotepadItemSelected (notepad gui) npitem
    setRef posref pos
    setRef selref sel

tlObjectSelected :: CItem c => GenGUI c ->
                               Maybe (TreeListObject (Item c)) -> IO ()
tlObjectSelected gui mobj =
  let addNotepadItem item@(IntLeafItem _ posref selref) =
        do
          lastpos <- getRef posref
          pos <- case lastpos of
                   (-1, -1) -> do
                                 pos <- getNewItemPosition gui
                                 setItemPosition item pos
                                 return pos
                   _ -> return lastpos
          npitem <- createNotepadItem item (notepad gui) [position pos]
          b <- getRef selref
          if b then selectAnotherItem (notepad gui) npitem else done
  in case mobj of
       Nothing -> do
                    mch <- getRef (event_queue gui)
                    case mch of
                      Just ch ->
                        syncNoWait (send ch (SelectTreeList Nothing))
                      _ -> done
       Just obj ->
         do
           mch <- getRef (event_queue gui)
           case mch of
             Just ch ->
               syncNoWait (send ch (SelectTreeList
                                      (Just
                                         (getTreeListObjectValue obj))))
             _ -> done
           synchronize gui (do
                              saveNotepadItemStates gui
                              ch <- children (getTreeListObjectValue obj)
                              clearNotepad (notepad gui)
                              mapM addNotepadItem
                                   (filter isItemLeaf ch)
                              done)
           setRef (open_obj gui) (Just (getTreeListObjectValue obj))

tlObjectFocused :: CItem c => GenGUI c -> Ref (Position, [Item c]) ->
                              (Maybe (TreeListObject (Item c)),
                                     EventInfo) ->
                              IO ()
tlObjectFocused gui clipboard (mobj, ev_inf) =
  do
    mch <- getRef (event_queue gui)
    case mobj of
      Just obj -> do
                    ((x, y), items) <- getRef clipboard
                    (if x == xRoot ev_inf &&
                        y == yRoot ev_inf then
                       do
                         putStrLn "moving items"
                         let item = getTreeListObjectValue obj
                         undoLastMotion (notepad gui)
                         selected_notepaditems <-
                           getSelectedItems (notepad gui)
                         mapM (saveNotepadItemState gui)
                              selected_notepaditems
                         moveItems gui items item
                     else case mch of
                            Just ch ->
                              do
                                let item = getTreeListObjectValue obj
                                syncNoWait
                                  (send ch
                                     (FocusTreeList (Just item)))
                            _ -> done)
      _ -> case mch of
             Just ch -> syncNoWait (send ch (FocusTreeList Nothing))
             _ -> done

moveItems :: CItem c => GenGUI c -> [Item c] -> Item c -> IO ()
moveItems gui items target@(IntFolderItem _ subitemsref) =
  let initItem (IntLeafItem _ posref selref) =
        setRef posref (-1, -1) >> setRef selref False
  in do
       Just ditem@(IntFolderItem _ dsubitemsref) <- getRef (open_obj gui)
       (if (ditem == target) then done
        else do
               dsubitems <- getRef dsubitemsref
               setRef dsubitemsref (dsubitems \\ items)
               subitems <- getRef subitemsref
               mapM initItem items
               setRef subitemsref (subitems ++ items)
               npitems <- getItems (notepad gui)
               mapM (\npitem -> do
                                  item <- getItemValue npitem
                                  let b = any (\item' -> item == item')
                                              items
                                  (if b then
                                     deleteItem (notepad gui) npitem
                                   else done)) npitems
               done)


--------------------------------------------------------------------------
-- notepad events
--------------------------------------------------------------------------

npItemSelected :: CItem c => GenGUI c -> (NotepadItem (Item c), Bool) ->
                             IO ()
npItemSelected gui (npitem, b) =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch ->
        do
          item <- getItemValue npitem
          syncNoWait (send ch (FocusNotepad (item, b)))
      _ -> done

npDropEvent :: CItem c => GenGUI c ->
                          (NotepadItem (Item c),
                           [NotepadItem (Item c)]) -> IO ()
npDropEvent gui (npitem, npitems) =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   item <- getItemValue npitem
                   items <- mapM getItemValue npitems
                   syncNoWait (send ch (GenGUI.Dropped (item, items)))
      _ -> done

npDoubleClick :: CItem c => GenGUI c -> NotepadItem (Item c) -> IO ()
npDoubleClick gui npitem =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   item <- getItemValue npitem
                   syncNoWait (send ch (GenGUI.Doubleclick item))
      _ -> done

npRightClick :: CItem c => GenGUI c -> [NotepadItem (Item c)] -> IO ()
npRightClick gui npitems =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   items <- mapM getItemValue npitems
                   syncNoWait (send ch (GenGUI.Rightclick items))
      _ -> done


--------------------------------------------------------------------------
-- notepad item placement (temporary)
--------------------------------------------------------------------------

getNewItemPosition :: CItem c => GenGUI c -> IO Position
getNewItemPosition gui = getFreeItemPosition (notepad gui)

setItemPosition :: CItem c => Item c -> Position -> IO ()
setItemPosition (IntLeafItem _ posref _) pos = setRef posref pos


--------------------------------------------------------------------------
-- exported GenGUI functionality
--------------------------------------------------------------------------

setStatus :: CItem c => GenGUI c-> String-> IO ()
setStatus gui txt = (status gui) # text txt >> done

clearStatus :: CItem c => GenGUI c-> IO ()
clearStatus gui = (status gui) # text "" >> done

updateTextArea :: CItem c => GenGUI c-> [MarkupText] -> IO ()
updateTextArea gui mtxt = (editor gui) # new mtxt >> done

clearTextArea :: CItem c => GenGUI c-> IO ()
clearTextArea gui = (editor gui) # clear >> done

genGUIMainMenu :: CItem c => GenGUI c-> Menu
genGUIMainMenu gui = topmenu gui

children :: CItem c => Item c -> IO [Item c]
children (IntFolderItem _ chref) = getRef chref
children (Root chref) =
  do
    items <- getRef chref
    return items
children _ = error "GenGUI (children) : called for a leaf"

openedFolder :: CItem c=> GenGUI c-> IO (Maybe (Item c))
openedFolder = getRef . open_obj

addItem :: CItem c => GenGUI c -> Item c -> NewItem c -> IO (Item c)
addItem gui par@(IntFolderItem _ chref) newitem =
  synchronize gui
    (do
       mditem <- getRef (open_obj gui)
       ch <- getRef chref
       item <- toItem newitem
       setRef chref (ch ++ [item])
       mch <- getRef (event_queue gui)
       case mch of
         Just ch -> syncNoWait (send ch (Addition item))
         _ -> done
       (if isItemFolder item then
          do
            mkNode (treelist gui) par
            nuch <- children item
            let nod = if (any isItemFolder nuch) then Node else Leaf
            case newitem of
              FolderItem c _ _ ->
                addTreeListSubObject (treelist gui) par
                  (newTreeListObject item nod)
              LeafItem c _ ->
                addTreeListSubObject (treelist gui) par
                  (newTreeListObject item nod)
        else done)
       case mditem of
         Just ditem -> if ditem == par then
                         do
                           pos <- getNewItemPosition gui
                           setItemPosition item pos
                           it <- createNotepadItem item (notepad gui)
                                                   [position pos]

                           scrollTo (notepad gui) it

                           done
                       else done
         _ -> done
       return item)
addItem gui (Root chref) newitem =
  synchronize gui
    (do
       items <- getRef chref
       item <- toItem newitem
       setRef chref (items ++ [item])
       mch <- getRef (event_queue gui)
       case mch of
         Just ch -> syncNoWait (send ch (Addition item))
         _ -> done
       (if isItemFolder item then
          do
            ch <- children item
            let nod = if (any isItemFolder ch) then Node else Leaf
            case newitem of
              FolderItem c _ _ ->
                addTreeListRootObject (treelist gui)
                  (newTreeListObject item nod)
              LeafItem c _ ->
                  addTreeListRootObject (treelist gui)
                    (newTreeListObject item nod)
        else done)
       return item)
addItem _ _ _ = error "GenGUI (addItem) : called for a leaf"

content :: CItem c => (Item c) -> c
content (IntFolderItem (FolderItem c _ _) _) = c
content (IntLeafItem (LeafItem c _) _ _) = c
content _ = error "GenGUI (content) : called for root"


--------------------------------------------------------------------------
-- events
--------------------------------------------------------------------------

data CItem c => GenGUIEvent c =
    FocusTreeList (Maybe (Item c)) 
  | SelectTreeList (Maybe (Item c))
  | FocusNotepad (Item c, Bool) -- what's the Bool?
  | Dropped (Item c, [Item c])
  | Doubleclick (Item c)
  | Rightclick [Item c]
  | Addition (Item c)
  | DroppedOnTextArea [Item c]
--  | TextEntry (String) -- Text Entry Window  ???

bindGenGUIEv :: CItem c => GenGUI c -> IO (Event (GenGUIEvent c), IO())
bindGenGUIEv gui =
  do
    ch <- newChannel
    setRef (event_queue gui) (Just ch)
    return (receive ch, setRef (event_queue gui) Nothing)

sendEv :: CItem c => GenGUI c -> GenGUIEvent c -> IO ()
sendEv gui ev =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> syncNoWait (send ch ev)
      _ -> done


--------------------------------------------------------------------------
-- treelist children function
--------------------------------------------------------------------------

toTreeListObjects :: CItem c => [Item c] -> IO [TreeListObject (Item c)]
toTreeListObjects (it : items) =
  do
    rest <- toTreeListObjects items
    ch <- children it
    let nod = if (any isItemFolder ch) then Node else Leaf
    return (newTreeListObject it nod : rest)
toTreeListObjects _ = return []

cfun :: CItem c => ChildrenFun (Item c)
cfun tlobj =
  do
    let item = getTreeListObjectValue tlobj
    ch <- children item
    toTreeListObjects (filter isItemFolder ch)


--------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------

instance CItem c => Eq (GenGUI c) where
  gui1 == gui2 = win gui1 == win gui2

instance CItem c => GUIObject (GenGUI c) where
  toGUIObject gui = toGUIObject (win gui)
  cname _ = "GenGUI"

instance CItem c => Destroyable (GenGUI c) where
  destroy = destroy . toGUIObject

instance CItem c => Window (GenGUI c) where
  iconify gui = iconify (win gui)
  deiconify gui  = deiconify (win gui)
  withdraw gui = withdraw (win gui)
  putWinOnTop gui = putWinOnTop (win gui)
  putWinAtBottom gui = putWinAtBottom (win gui)

instance CItem c => Synchronized (GenGUI c) where
  synchronize gui = synchronize (win gui)
