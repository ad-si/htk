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

  GenGUI,
  newGenGUI, {- :: CItem c => Maybe (GenGUIState c) -> IO (GenGUI c)    -}

  setStatus,      {- :: CItem c => GenGUI c-> String-> IO ()            -}
  clearStatus,    {- :: CItem c => GenGUI c-> IO ()                     -}
  updateTextArea, {- :: CItem c => GenGUI c-> [MarkupText] -> IO ()     -}
  clearTextArea,  {- :: CItem c => GenGUI c-> IO ()                     -}
  genGUIMainMenu, {- :: CItem c => GenGUI c-> Menu                      -}

  NewItem(..),
  Item,

  Name(..),

  CItem(..),

  root,     {- :: CItem c => GenGUI c -> IO (Item c)                    -}
  addItem,  {- :: CItem c => Item c -> NewItem c -> IO (Item c)         -}
  children, {- :: CItem c => Item c -> IO [Item c]                      -}
  content,  {- :: CItem c => (Item c) -> c                              -}

  GenGUIEvent(..),
  bindGenGUIEv, {- :: CItem c => GenGUI c ->                            -}
                {-               IO (Event (GenGUIEvent c), IO())       -}

--  GenGUIExportItem(..),
  GenGUIState,
--  exportGenGUIState, {- :: CItem c => GenGUI c -> IO (GenGUIState c)    -}
--  importGenGUIState  {- :: CItem c => GenGUI c -> GenGUIState c -> IO ()-}

) where

import HTk
import ScrollBox
import TreeList
import Notepad
import ReferenceVariables
import Name
import Core
import List(find)
import MarkupText
import PrelBase(not)
import CItem
import Maybe


-----------------------------
-- external representation --
-----------------------------

data CItem c => NewItem c =
    LeafItem c (Maybe Position)
  | FolderItem c [NewItem c] (Maybe (Position, Bool))  -- Bool ??


{- ich br�uchte:

data CItem c => NewItem c =

    LeafItem c (Maybe (Position,     -- position on notepad
                       Bool          -- selected in notepad ?
                      ))
  | FolderItem c [NewItem c] (Maybe (Bool, -- open in TreeList
                                           -- (recover only)
                                     Bool, -- selected in TreeList,
                                           -- (recover only)
                                    ))

-}



-----------------------------------
-- internal type & functionality --
-----------------------------------

data CItem c => Item c =
    IntFolderItem (GenGUI c)                                 -- parent gui
                  (NewItem c)                   -- external representation
                  (Ref [Item c])                               -- subitems
  | IntLeafItem (GenGUI c)                                   -- parent gui
                (NewItem c)                     -- external representation
  | Root (Ref (Maybe (GenGUI c)))

{-
newID :: (Ref Int) -> IO String
newID idref =
  do
    n <- getRef idref
    setRef idref (n + 1)
    return ("item" ++ show n)
-}

{-
getID :: CItem c => Item c -> Id
getID (IntFolderItem _ id _ _) = id
getID (IntLeafItem _ id _) = id
getID (Root id _) = id
-}

isItemFolder :: Item c -> Bool
isItemFolder (IntFolderItem _ _ _) = True
isItemFolder _ = False

isItemLeaf :: Item c -> Bool
isItemLeaf = PrelBase.not . isItemFolder

isRoot :: Item c -> Bool
isRoot (Root _) = True
isRoot _ = False

getGenGUI :: Item c -> IO (GenGUI c)
getGenGUI (IntFolderItem gui _ _) = return gui
getGenGUI (IntLeafItem gui _) = return gui
getGenGUI (Root mguiref) =
  do
    mgui <- getRef mguiref
    case mgui of Just gui -> return gui
                 _ -> error "GenGUI (getGenGUI) : root empty"
{-
getItemImage :: CItem c => TreeListObject c -> IO Image
getItemImage tlobj =
  getIcon (getTreeListObjectValue tlobj)
-}

toItem :: CItem c => GenGUI c -> NewItem c -> IO (Item c)
toItem gui it@(FolderItem _ ch _) =
  do
--    id <- newID (obj_cnt gui)
    intch <- mapM (toItem gui) ch
    intchref <- newRef intch
    return (IntFolderItem gui it intchref)
toItem gui it@(LeafItem _ _) =
  do
--    id <- newID (obj_cnt gui)
    return (IntLeafItem gui it)

instance CItem c => Eq (Item c) where
  item1 == item2 = content item1 == content item2
{-
  (IntFolderItem gui1 id1 _ _) == (IntFolderItem gui2 id2 _ _) =
    id1 == id2 && gui1 == gui2
  (IntLeafItem gui1 id1 _) == (IntLeafItem gui2 id2 _) =
    id1 == id2 && gui1 == gui2
  (Root id1 _) == (Root id2 _) = id1 == id2
  _ == _ = False
-}

--------------------
-- internal state --
--------------------

root :: CItem c => GenGUI c -> IO (Item c)
root gui =
  do
    mguiref <- newRef (Just gui)
    return (Root mguiref)


-------------------------
-- state import/export --
-------------------------

{-
data GenGUIExportItem c =
    GenGUIExportFolderItem (NewItem c) [GenGUIExportItem c]
  | GenGUIExportLeafItem (NewItem c)
  | GenGUIExportRootItem [GenGUIExportItem c]
-}

type GenGUIState c =
  ([NewItem c], TreeListState c, NotepadState c, [(c, NotepadState c)])

{-
exportGenGUIState :: CItem c => GenGUI c -> IO (GenGUIState c)
exportGenGUIState gui =
  do
    items <- getRef (root_obj gui)
    npstates <- getRef (np_states gui)
    items' <- subitems items
    tl_state <- exportTreeListState (treelist gui)
    np_state <- exportNotepadState (notepad gui)
    displayed_obj <- getRef (open_obj gui)
    return (items', tl_state, np_state,
            npstates, displayed_obj, i)
  where subitems :: CItem c => [Item c] -> IO [GenGUIExportItem c]
        subitems (IntFolderItem _ newitem subitemsref : items) =
          do
            sub <- getRef subitemsref
            sub' <- subitems sub
            rest <- subitems items
            let (FolderItem c 
            return (FolderItem newitem sub' : rest)
        subitems (IntLeafItem _ newitem : items) =
          do
            rest <- subitems items
            return (GenGUIExportLeafItem newitem : rest)
        subitems _ = return []
-}

importGenGUIState :: CItem c => GenGUI c -> GenGUIState c -> IO ()
importGenGUIState _ _ = done  -- Scheitert leider w�hrend das GUI l�uft
                              -- noch an den Treelisten (bug!).
                              -- GenGUI kann aber bereits mit State
                              -- erzeugt werden!


--------------------------
-- type and constructor --
--------------------------

data CItem c => GenGUI c =
  GenGUI
    { -- the id of this gui
--      gui_id :: Id,

      -- the treelist
      treelist :: TreeList c,

      -- the notepad
      notepad :: Notepad c,

      -- the markup text container
      editor :: Editor String,

      -- the status bar
      status :: Label String,

      -- GenGUI's main menu
      topmenu :: Menu,

      -- GenGUI's toplevel window
      win :: Toplevel,

      -- counter, needed for ids
--      obj_cnt :: Ref Int,

      -- item displayed in notepad
      open_obj :: Ref (Maybe (Item c)),

      -- temporary, needed for current stupid placement of notepad items
      place :: Ref Position,

      -- internal state
      root_obj :: Ref [Item c],

      -- saved positions, selection etc. of the notepad
      np_states :: Ref [(c, NotepadState c)],

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

    -- references (saved state may be recovered)
    intstate <- newRef []
    guiref <- newRef Nothing
    ({-id, idref,-} displayref, npstates) <-
      case mstate of
        Nothing ->
          do
            idref <- newRef 0
--            id <- newID idref
            displayref <- newRef Nothing
            npstates <- newRef []
            return ({-id, idref,-} displayref, npstates)
{-
        Just (exportitem@(GenGUIExportRootItem _), tlstate, npstate,
              npstates, displayed_obj, i) ->
          do
            idref <- newRef i
            displayref <- newRef displayed_obj
            npstates <- newRef npstates
            return ({-id, idref,-} displayref, npstates)
-}

    -- construct main widgets

    let getTreeListState :: Maybe (GenGUIState c) ->
                            Maybe (TreeListState c)
--        getTreeListState (Just (_, tlstate, _, _, _, _)) = Just tlstate
        getTreeListState _ = Nothing

        getNotepadState :: Maybe (GenGUIState c) -> Maybe (NotepadState c)
--        getNotepadState (Just (_, _, npstate, _, _, _)) = Just npstate
        getNotepadState _ = Nothing

    (tl, np, edscr, ed) <-
      (if tixAvailable then
         do
           objects_n_editor <- newPanedWindow main Horizontal []
           paneh1 <- createPane objects_n_editor [initsize 400] []
           paneh2 <- createPane objects_n_editor [initsize 500] []
           objects <- newPanedWindow paneh1 Vertical []
           panev1 <- createPane objects [initsize 250] []
           panev2 <- createPane objects [initsize 300] []
           pack objects [Fill Both, Expand On]
           pack objects_n_editor [Fill Both, Expand On]
           tl <- case getTreeListState mstate of
                   Just state ->
                     recoverTreeList panev1 (cfun guiref)
                                     state [background "white"]
                   _ -> newTreeList panev1 (cfun guiref) []
                                    [background "white"]
           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad panev2 Scrolled (12, 12)
                            (getNotepadState mstate)
                            [background "white",
                             npScrollRegion ((0, 0), (800, 800))]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           (edscr, ed) <- newScrollBox paneh2
                            (\par -> newEditor par [width 60]) []
           pack edscr [PadX 6, PadY 6, Fill Both, Expand On]
           return (tl, np, edscr, ed)
       else
         do
           objects <- newFrame main []
           pack objects [Side AtLeft, Fill Both, Expand On]
           tl <- case getTreeListState mstate of
                   Just state ->
                     recoverTreeList objects (cfun guiref){-getItemImage-}
                                     state
                                     [background "white", size (500, 250)]
                   _ -> newTreeList objects (cfun guiref) {-getItemImage-}
                                    []
                          [background "white", size (500, 250)]

           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad objects Scrolled (12, 12)
                            (getNotepadState mstate)
                            [size (500, 280), background "white",
                             npScrollRegion ((0, 0), (800, 800))]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           (edscr, ed) <- newScrollBox main
                            (\par -> newEditor par [width 60]
                                       :: IO (Editor String)) []
           pack edscr [PadX 5, PadY 5, Fill Both, Expand On]
           return (tl, np, edscr, ed))

    stlab <- newLabel main [text "Welcome", relief Ridge,
                            HTk.font (Lucida, 12::Int)]
    pack stlab [Side AtBottom, PadX 5, PadY 2, Fill X]

    ed # state Disabled


    -- temporary (placement of objects in notepad / TD)
    posref <- newRef (0, 0)
    initItemPosition posref

    -- event queue
    evq <- newRef Nothing

    -- GenGUI value
    let gui = GenGUI { {-gui_id = id,-}
                       treelist = tl,
                       notepad = np,
                       editor = ed,
                       status = stlab,
                       topmenu = menubar,
                       win = main,
--                       obj_cnt = idref,
                       open_obj = displayref,
                       place = posref,
                       root_obj = intstate,
                       np_states = npstates,
                       event_queue = evq }

    case mstate of
{-
      Just (exportitem, _, _, _, _, _) ->
        let 
            recoverState (GenGUIExportRootItem items) =
              recoverSubitems items

            recoverSubitems (GenGUIExportFolderItem newitem sitems :
                             items) =
              do
                sub <- recoverSubitems sitems
                rest <- recoverSubitems items
                subitemsref <- newRef sub
                return (IntFolderItem gui newitem subitemsref : rest)
            recoverSubitems (GenGUIExportLeafItem newitem : items) =
              do
                rest <- recoverSubitems items
                return (IntLeafItem gui newitem : rest)
            recoverSubitems _ = return []
        in do
             st <- recoverState exportitem
             setRef intstate st
-}
      _ -> done

    setRef guiref (Just gui)

    -- listening events
    clipboard <- newRef ((0,0), [])   -- drop on editor
    (enter_ed, _) <- bind ed [WishEvent [] Enter]
    (leave_np, _) <- bind np [WishEvent [] Leave]

    (np_ev, _) <- bindNotepadEv np

    spawnEvent (forever ((do
                            ev <- np_ev
                            always
                              (case ev of
                                 Selected c ->
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
                         (receive (selectionEvent tl) >>>=
                            tlObjectSelected gui) +>
                         (receive (focusEvent tl) >>>=
                            tlObjectFocused gui) +>
                         (do
                            -- evtl. threadDelay ??
                            ev_inf <- enter_ed
                            always
                              (do
                                 ((x, y), cs) <- getRef clipboard
                                 (if x == xRoot ev_inf &&
                                     y == yRoot ev_inf then
                                    do
                                      putStrLn "drag and drop action"
                                      items <-
                                        mapM (\c -> do
                                                      mobj <-
                                                        getGenGUIObject
                                                          gui c
                                                      return
                                                        (fromJust mobj))
                                             cs
                                      sendEv gui
                                             (DroppedOnTextArea items)
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
    return gui


--------------------
-- event handling --
--------------------

tlObjectSelected :: CItem c => GenGUI c ->
                               Maybe (TreeListObject c) -> IO ()
tlObjectSelected gui mobj =
  let addNotepadItem :: CItem c => (Item c) -> IO ()
      addNotepadItem item@(IntLeafItem gui (LeafItem c _)) =
        do
          pos <- getNewItemPosition (place gui)
          createNotepadItem c (notepad gui) [position pos]
          done
  in case mobj of
       Nothing -> do
                    mch <- getRef (event_queue gui)
                    case mch of
                      Just ch ->
                        syncNoWait (send ch (SelectTreeList Nothing))
                      _ -> done
       Just obj ->
         let c = getTreeListObjectValue obj
         in do
--              gui <- getGenGUI item
--              (if isRoot item then done
--               else do
              mch <- getRef (event_queue gui)
              mobj <- getGenGUIObject gui c
              case mch of
                Just ch ->
                  syncNoWait (send ch (SelectTreeList (Just (fromJust mobj))))
                _ -> done
              saveNotepadState gui
              mstate <- revisit gui c
              case mstate of
                Just notepadstate -> importNotepadState (notepad gui)
                                                        notepadstate
                _ ->
                  synchronize gui (do
                                     mguiobj <- getGenGUIObject gui c
                                     let guiobj = fromJust mguiobj
                                     ch <- children guiobj
                                     clearNotepad (notepad gui)
                                     initItemPosition (place gui)
                                     mapM addNotepadItem
                                          (filter isItemLeaf ch)
                                     done)
              mitem <- getGenGUIObject gui c
              let item = fromJust mitem
              setRef (open_obj gui) (Just item)

revisit :: CItem c => GenGUI c -> c -> IO (Maybe (NotepadState c))
revisit gui c =
  do
    states <- getRef (np_states gui)
    let mstate = find (\ (c', _) -> c == c') states
    case mstate of
      Just (_, state) -> return (Just state)
      _ -> return Nothing

saveNotepadState :: CItem c => GenGUI c -> IO ()
saveNotepadState gui =
  synchronize gui
    (do
       mcurrent <- getRef (open_obj gui)
       case mcurrent of
         Just current_item ->
           do
             let c = content current_item
             npstate <- exportNotepadState (notepad gui)
             states <- getRef (np_states gui)
             let states' = filter (\ (c', _) -> c' /= c)
                                  states
             setRef (np_states gui) ((c, npstate) : states')
         _ -> done)

tlObjectFocused :: CItem c => GenGUI c ->
                              Maybe (TreeListObject c) -> IO ()
tlObjectFocused gui mobj =
  do
    mch <- getRef (event_queue gui)
    case mobj of
      Just obj -> do
                    case mch of
                      Just ch ->
                        do
                          let c = getTreeListObjectValue obj
                          mobj <- getGenGUIObject gui c
                          syncNoWait
                            (send ch
                               (FocusTreeList (Just (fromJust mobj))))
                      _ -> done
      _ -> case mch of
             Just ch -> syncNoWait (send ch (FocusTreeList Nothing))
             _ -> done


--------------------------------------------------------------------------
-- notepad events
--------------------------------------------------------------------------

npItemSelected :: CItem c => GenGUI c -> (NotepadItem c, Bool) -> IO ()
npItemSelected gui (npitem, b) =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch ->
        do
          c <- getItemValue npitem
          mitem <- getGenGUIObject gui c
          syncNoWait (send ch (FocusNotepad (fromJust mitem, b)))
      _ -> done

npDropEvent :: CItem c => GenGUI c -> (NotepadItem c, [NotepadItem c]) ->
                          IO ()
npDropEvent gui (npitem, npitems) =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                  c <- getItemValue npitem
                  mitem <- getGenGUIObject gui c
                  cs <- mapM getItemValue npitems
                  mitems <- mapM (getGenGUIObject gui) cs
                  syncNoWait
                    (send ch (GenGUI.Dropped (fromJust mitem,
                                              map fromJust mitems)))
      _ -> done

npDoubleClick :: CItem c => GenGUI c -> NotepadItem c -> IO ()
npDoubleClick gui npitem =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   c <- getItemValue npitem
                   mobj <- getGenGUIObject gui c
                   syncNoWait
                     (send ch (GenGUI.Doubleclick (fromJust mobj)))
      _ -> done

npRightClick :: CItem c => GenGUI c -> [NotepadItem c] -> IO ()
npRightClick gui npitems =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   cs <- mapM (\npitem -> do
                                            c <- getItemValue npitem
                                            mobj <- getGenGUIObject gui c
                                            return (fromJust mobj))
                              npitems
                   syncNoWait (send ch (GenGUI.Rightclick cs))
      _ -> done


--------------------------------------------------------------------------
-- notepad item placement (temporary)
--------------------------------------------------------------------------

notepaddx :: Int
notepaddx = 90

notepaddy :: Int
notepaddy = 40

num_cols :: Int
num_cols = 4

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
children (IntFolderItem _ _ chref) = getRef chref
children (Root mguiref) =
  do
    mgui <- getRef mguiref
    case mgui of
      Just gui ->
        do
          items <- getRef (root_obj gui)
          return items
      _ -> return []
children _ = error "GenGUI (children) : called for a leaf"

addItem :: CItem c => Item c -> NewItem c -> IO (Item c)
addItem par@(IntFolderItem gui _ chref) newitem =
  synchronize gui
    (do
       mditem <- getRef (open_obj gui)
       ch <- getRef chref
       item <- toItem gui newitem
       setRef chref (ch ++ [item])
       mch <- getRef (event_queue gui)
       case mch of
         Just ch -> do
                      mobj <- getGenGUIObject gui (content item)
                      syncNoWait (send ch (Addition (fromJust mobj)))
         _ -> done
       (if isItemFolder item then
          do
            mkNode (treelist gui) (content par)
            nuch <- children item
            let nod = if (any isItemFolder nuch) then Node else Leaf
            case newitem of
              FolderItem c _ _ ->
                addTreeListSubObject (treelist gui) (content par)
                  (newTreeListObject (content item) nod)
              LeafItem c _ ->
                addTreeListSubObject (treelist gui) (content par)
                  (newTreeListObject (content item) nod)
        else done)
       case mditem of
         Just ditem -> if ditem == par then
                         case newitem of
                           LeafItem c _ ->
                             do
                               pos <- getNewItemPosition (place gui)
                               createNotepadItem c (notepad gui)
                                                 [position pos]
                               done
                           _ -> done
                       else done
         _ -> done
       return item)
addItem par@(Root mguiref) newitem =
  do
    mgui <- getRef mguiref
    case mgui of
      Just gui ->
        synchronize gui
          (do
             items <- getRef (root_obj gui)
             item <- toItem gui newitem
             setRef (root_obj gui) (items ++ [item])
             chs <- children item
             mch <- getRef (event_queue gui)
             case mch of
               Just ch -> do
                            mobj <- getGenGUIObject gui (content item)
                            syncNoWait
                              (send ch (Addition (fromJust mobj)))
               _ -> done
             (if isItemFolder item then
                do
                  ch <- children item
                  let nod = if (any isItemFolder ch) then Node else Leaf
                  case newitem of
                    FolderItem c _ _ ->
                      addTreeListRootObject (treelist gui)
                        (newTreeListObject (content item) nod)
                    LeafItem c _ ->
                        addTreeListRootObject (treelist gui)
                          (newTreeListObject (content item) nod)
              else done)
             return item)
      _ -> error "GenGUI (addItem) : root empty"
addItem _ _ = error "GenGUI (addItem) : called for a leaf"


content :: CItem c => (Item c) -> c
content (IntFolderItem _ (FolderItem c _ _) _) = c
content (IntLeafItem _ (LeafItem c _)) = c
content _ = error "GenGUI (content) : called for root"


------------
-- events --
------------

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

toTreeListObjects :: CItem c => [Item c] -> IO [TreeListObject c]
toTreeListObjects (it@(IntFolderItem gui (FolderItem c _ _) chref) :
                   items) =
  do
    rest <- toTreeListObjects items
    ch <- children it
    let nod = if (any isItemFolder ch) then Node else Leaf
    return (newTreeListObject c nod : rest)
toTreeListObjects _ = return []

getGenGUIObject :: CItem c => GenGUI c -> c -> IO (Maybe (Item c))
getGenGUIObject gui c =
  do
    items <- getRef (root_obj gui)
    getObject c items
  where getObject :: CItem c => c -> [Item c] -> IO (Maybe (Item c))
        getObject c (item@(IntFolderItem _ _ subitemsref) : items) =
          if content item == c then return (Just item)
          else do
                 subitems <- getRef subitemsref
                 mobj <- getObject c subitems
                 case mobj of
                   Just obj -> return (Just obj)
                   _ -> getObject c items
        getObject c (item : items) =
          if content item == c then return (Just item)
          else getObject c items
        getObject _ _ = return Nothing

cfun :: CItem c => Ref (Maybe (GenGUI c)) -> ChildrenFun c
cfun guiref tlobj =
  do
    mgui <- getRef guiref
    let gui = fromJust mgui
    let c = getTreeListObjectValue tlobj
    mguiobj <- getGenGUIObject gui c
    case mguiobj of
      Just guiobj ->
        do
          ch <- children guiobj
          toTreeListObjects (filter isItemFolder ch)
      _ -> error "GenGUI: cfun called for non-existing object"


---------------
-- instances --
---------------

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


-- temp --

folderImg = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAAP///////////////yH+FUNyZWF0ZWQgd2l0aCBUaGUg
R0lNUAAh+QQBCgAEACwAAAAADAAMAEADLUi6vCAihDjBIA9qaBWYAtBgkESFF6Cu7OWpI3myXlSW
3QPueTmZHcJgSCwmAAA7"]
