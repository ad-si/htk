-- | A generic graphical user interface.
module HTk.Toolkit.GenGUI (

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

) where

import Data.List
import Data.Maybe

import Util.Computation

import Events.Events
import Events.Channels
import Events.Synchronized

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk hiding (font)
import qualified HTk.Toplevel.HTk as HTk (font)
import HTk.Toolkit.ScrollBox
import qualified HTk.Toolkit.TreeList as TreeList (obj_val, TreeListEvent(Selected), selected)
import HTk.Toolkit.TreeList hiding (obj_val, TreeListEvent(Selected), selected)
import qualified HTk.Toolkit.Notepad as Notepad (NotepadEvent(Dropped, Doubleclick, Rightclick))
import HTk.Toolkit.Notepad hiding (NotepadEvent(Dropped, Doubleclick, Rightclick))
import HTk.Kernel.Core

import HTk.Toolkit.MarkupText
import HTk.Toolkit.CItem


--------------------------------------------------------------------------
-- external object representation
--------------------------------------------------------------------------

-- | External representation of gengui objects.
data NewItem c =

    LeafItem c (Maybe (Position,     -- position on notepad
                       Bool          -- selected in notepad
                      ))
  | FolderItem c [NewItem c] (Maybe (Bool, -- open in TreeList
                                           -- (recover only)
                                     Bool  -- displayed in notepad
                                    ))

-- | Gets the name of a newitem object.
getNameFromNewItem :: CItem c => NewItem c -> IO Name
getNameFromNewItem (LeafItem c _) = getName c
getNameFromNewItem (FolderItem c _ _) = getName c

-- | Returns whether the given object is a folder or not.
isNewItemFolder :: CItem c => NewItem c -> Bool
isNewItemFolder (FolderItem _ _ _) = True
isNewItemFolder _ = False


--------------------------------------------------------------------------
-- internal type & functionality
--------------------------------------------------------------------------

-- | internal object representation
data Item c =
    IntFolderItem (NewItem c)                   -- external representation
                  (Ref [Item c])                               -- subitems
  | IntLeafItem (NewItem c)                     -- external representation
                (Ref Position)                      -- position on notepad
                (Ref Bool)                          -- selected on notepad
  | Root (Ref [Item c])

-- | Internal.
instance CItem c => Eq (Item c) where
  item1 == item2 = content item1 == content item2

-- | Objects must have a name and an icon.
instance CItem c => CItem (Item c) where
  -- Gets the object\'s name.
  getName = getName . content
  -- Gets the object\'s icon.
  getIcon = getIcon . content

-- | Returns whether an item is a folder or not.
isItemFolder :: Item c
   -- ^ the concerned item.
   -> Bool
   -- ^ @True@ if the given item is a folder,
   -- otherwise @False@.
isItemFolder it@(IntFolderItem _ _) = True
isItemFolder _ = False


-- | Returns whether an item is a folder or not.
isItemLeaf :: Item c -> Bool
isItemLeaf = Prelude.not . isItemFolder

-- | Converts the external object representation to the internal object
-- representation.
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

-- | GenGUI\'s root object.
root :: CItem c => GenGUI c -> IO (Item c)
root gui = return (Root (root_obj gui))


--------------------------------------------------------------------------
-- state import/export
--------------------------------------------------------------------------

-- | The gui\'s state.
type GenGUIState c = [NewItem c]

-- | Exports the gui\'s state.
exportGenGUIState :: CItem c => GenGUI c
   -- ^ the concerned GenGUI.
   -> IO (GenGUIState c)
   -- ^ the gui\'s state.
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


--------------------------------------------------------------------------
-- type and constructor
--------------------------------------------------------------------------

-- | The @GenGUI@ datatye.
data GenGUI c =
  GenGUI
    { -- the treelist
      treelist :: TreeList (Item c),

      -- the notepad
      notepad :: Notepad (Item c),

      -- the markup text container
      editor :: Editor,

      -- the status bar
      status :: Label,

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
      event_queue :: Ref (Maybe (Channel (GenGUIEvent c))),

      show_leaves_in_tree :: Bool }


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Constructs a new gui and returns a handler.
newGenGUI :: CItem c => Maybe (GenGUIState c)
   -- ^ an optional GenGUI state to recover.
   -> Bool
   -- ^ @True@ if lleaves should be
   -- displayed in the tree list.
   -> IO (GenGUI c)
   -- ^ A gui.
newGenGUI mstate showLeavesInTree =
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

    tix <- isTixAvailable
    (tl, np, edscr, ed) <-
      (if tix then
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
                     recoverTreeList panev1 (cfun showLeavesInTree) state
                                     [background "white"]
                   _ -> newTreeList panev1 (cfun showLeavesInTree) []
                                    [background "white"]
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
                   Just state -> recoverTreeList objects
                                   (cfun showLeavesInTree) state
                                   [background "white", size (380, 200)]
                   _ -> newTreeList objects (cfun showLeavesInTree) []
                          [background "white", size (380, 200)]
           pack tl [PadX 5, PadY 5, Fill Both, Expand On]
           np <- newNotepad objects Scrolled (12, 12) Nothing
                            [size (800, 800), background "white"]
           pack np [PadX 5, PadY 5, Fill Both, Expand On]
           (edscr, ed) <- newScrollBox main
                            (\par -> newEditor par [width 40]) []
           pack edscr [PadX 5, PadY 5, Fill Both, Expand On]
           return (tl, np, edscr, ed))

    ed # state Disabled

    -- temporary (placement of objects in notepad / TD)
    posref <- newRef (0, 0)

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
                       event_queue = evq,
                       show_leaves_in_tree = showLeavesInTree }

    -- listening events
    clipboard_dnd <- newRef ((-1,-1), [])   -- drop on editor
    clipboard_mov <- newRef ((-1,-1), [], Nothing)   -- drop on treelist
    (enter_ed, _) <- bind ed [WishEvent [] Enter]

    (np_ev, _) <- bindNotepadEv np
    (tl_ev, _) <- bindTreeListEv tl

    _ <- spawnEvent (forever ((do
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
                                 ReleaseMovement ev_inf ->
                                   synchronize (notepad gui)
                                     (do
                                        ((x1, y1), items1) <-
                                          getRef clipboard_dnd
                                        ((x2, y2), items2, mitem) <-
                                          getRef clipboard_mov
                                        (if x1 == xRoot ev_inf &&
                                            y1 == yRoot ev_inf then
                                           do
                                             sendEv gui
                                               (DroppedOnTextArea items1)
                                             undoLastMotion np
                                         else
                                           if x2 == xRoot ev_inf &&
                                              y2 == yRoot ev_inf &&
                                              isJust mitem then
                                             do
                                               let item = fromJust mitem
                                               undoLastMotion (notepad gui)
                                               selected_notepaditems <-
                                                 getSelectedItems
                                                   (notepad gui)
                                               mapM
                                                 (saveNotepadItemState gui)
                                                 selected_notepaditems
                                               moveItems gui items2 item
                                           else
                                             do
                                               selected_notepaditems <-
                                                 getSelectedItems np
                                               selected_items <-
                                                 mapM getItemValue
                                                      selected_notepaditems
                                               setRef clipboard_dnd
                                                 ((xRoot ev_inf,
                                                   yRoot ev_inf),
                                                  selected_items)
                                               setRef clipboard_mov
                                                 ((xRoot ev_inf,
                                                   yRoot ev_inf),
                                                  selected_items, Nothing)))
                                 _ -> done)) +>
                         (do
                            ev <- tl_ev
                            always
                              (case ev of
                                 TreeList.Selected mobj ->
                                   tlObjectSelected gui mobj
                                 Focused mobjninf ->
                                   tlObjectFocused gui clipboard_mov
                                                   mobjninf
                                 )) +>
                         (do
                            ev_inf <- enter_ed
                            always
                              (do
                                 ((x, y), items) <- getRef clipboard_dnd
                                 (if x == xRoot ev_inf &&
                                     y == yRoot ev_inf then
                                    do
                                      sendEv gui (DroppedOnTextArea items)
                                      undoLastMotion np
                                  else
                                    do
                                      selected_notepaditems <-
                                        getSelectedItems np
                                      selected_items <-
                                        mapM getItemValue
                                             selected_notepaditems
                                      setRef clipboard_dnd
                                        ((xRoot ev_inf, yRoot ev_inf),
                                         selected_items))))))
    ditem <- getRef displayref
    case ditem of
      Just item ->
        tlObjectSelected gui (Just (newTreeListObject item Node))
      _ -> done

    return gui


--------------------------------------------------------------------------
-- internal event handling
--------------------------------------------------------------------------

-- | Saves the state (position etc.) of the currently displayed notepad
-- items.
saveNotepadItemStates :: CItem c => GenGUI c -> IO ()
saveNotepadItemStates gui =
  do
    npitems <- getItems (notepad gui)
    mapM (saveNotepadItemState gui) npitems
    done

-- | Saves the state of a single currently displayed notepad item.
saveNotepadItemState :: CItem c => GenGUI c -> NotepadItem (Item c) ->
                                   IO ()
saveNotepadItemState gui npitem =
  do
    IntLeafItem _ posref selref <- getItemValue npitem
    pos <- getPosition npitem
    sel <- isNotepadItemSelected (notepad gui) npitem
    setRef posref pos
    setRef selref sel

-- | Treelist selection event handler.
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
          npitem <- createNotepadItem item (notepad gui) False
                                      [position pos]
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
                              updNotepadScrollRegion (notepad gui)
                              done)
           setRef (open_obj gui) (Just (getTreeListObjectValue obj))

-- | Treelist focus event handler.
tlObjectFocused :: CItem c => GenGUI c ->
                              Ref (Position, [Item c], Maybe (Item c)) ->
                              (Maybe (TreeListObject (Item c)),
                                     EventInfo) ->
                              IO ()
tlObjectFocused gui clipboard (mobj, ev_inf) =
  do
    mch <- getRef (event_queue gui)
    case mobj of
      Just obj -> do
                    let item = getTreeListObjectValue obj
                    ((x, y), items, mitem) <- getRef clipboard
                    (if x == xRoot ev_inf &&
                        y == yRoot ev_inf &&
                        isNothing mitem then
                       do
                         undoLastMotion (notepad gui)
                         selected_notepaditems <-
                           getSelectedItems (notepad gui)
                         mapM (saveNotepadItemState gui)
                              selected_notepaditems
                         moveItems gui items item
                     else do
                            case mch of
                              Just ch ->
                                do
                                  let item = getTreeListObjectValue obj
                                  syncNoWait
                                    (send ch
                                       (FocusTreeList (Just item)))
                              _ -> done
                            selected_notepaditems <-
                              getSelectedItems (notepad gui)
                            selected_items <-
                              mapM getItemValue selected_notepaditems
                            setRef clipboard
                                   ((xRoot ev_inf, yRoot ev_inf),
                                     selected_items, Just item))
      _ -> case mch of
             Just ch -> syncNoWait (send ch (FocusTreeList Nothing))
             _ -> done

-- | Moves the given leaf objects to another folder.
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

-- | Notepad selection event handler.
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

-- | Notepad drop event handler.
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
                   syncNoWait (send ch (Dropped (item, items)))
      _ -> done

-- | Notepad double click event handler.
npDoubleClick :: CItem c => GenGUI c -> NotepadItem (Item c) -> IO ()
npDoubleClick gui npitem =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   item <- getItemValue npitem
                   syncNoWait (send ch (Doubleclick item))
      _ -> done

-- | Notepad right click event handler.
npRightClick :: CItem c => GenGUI c -> [NotepadItem (Item c)] -> IO ()
npRightClick gui npitems =
  do
    mch <- getRef (event_queue gui)
    case mch of
      Just ch -> do
                   items <- mapM getItemValue npitems
                   syncNoWait (send ch (Rightclick items))
      _ -> done


--------------------------------------------------------------------------
-- notepad item placement
--------------------------------------------------------------------------

-- | Gets the next free item position on the notepad.
getNewItemPosition :: CItem c => GenGUI c -> IO Position
getNewItemPosition gui = getFreeItemPosition (notepad gui)

-- | Sets the items position reference.
setItemPosition :: CItem c => Item c -> Position -> IO ()
setItemPosition (IntLeafItem _ posref _) pos = setRef posref pos


--------------------------------------------------------------------------
-- exported GenGUI functionality
--------------------------------------------------------------------------

-- | Sets the status label\'s text.
setStatus :: CItem c => GenGUI c-> String-> IO ()
setStatus gui txt = (status gui) # text txt >> done

-- | Clears the status label.
clearStatus :: CItem c => GenGUI c-> IO ()
clearStatus gui = (status gui) # text "" >> done

-- | Displays the given markup text on the editor pane.
updateTextArea :: CItem c => GenGUI c-> [MarkupText] -> IO ()
updateTextArea gui mtxt = (editor gui) # new mtxt >> done

-- | Clears the editor pane.
clearTextArea :: CItem c => GenGUI c-> IO ()
clearTextArea gui = (editor gui) # clear >> done

-- | Gets the gui\'s menu container.
genGUIMainMenu :: CItem c => GenGUI c-> Menu
genGUIMainMenu gui = topmenu gui

-- | Gets the children from a folder item.
children :: CItem c => Item c -> IO [Item c]
children (IntFolderItem _ chref) = getRef chref
children (Root chref) =
  do
    items <- getRef chref
    return items
children _ = return []

-- | Gets the item that is currently open (displayed on notepad).
openedFolder :: CItem c=> GenGUI c-> IO (Maybe (Item c))
openedFolder = getRef . open_obj

-- | Adds a gengui object.
addItem :: CItem c => GenGUI c
   -- ^ the concerned gui.
   -> Item c
   -- ^ the parent (folder) object.
   -> NewItem c
   -- ^ the external representation of the new object.
   -> IO (Item c)
   -- ^ the internal representation of the new object.
addItem gui par@(IntFolderItem (FolderItem c _ _)  chref) newitem =
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
       (if (isItemFolder item || show_leaves_in_tree gui) then
          do
            mkNode (treelist gui) par
            nuch <- children item
            let nod = if show_leaves_in_tree gui then
                        if Prelude.not (null nuch) then Node else Leaf
                      else
                        if (any isItemFolder nuch) then Node else Leaf
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
                           it <- createNotepadItem item (notepad gui) True
                                                   [position pos]
                           done
                       else done
         _ -> done
       return item)
addItem gui (Root chref) newitem =
  synchronize gui
    (do
       nm <- getNameFromNewItem newitem
       items <- getRef chref
       item <- toItem newitem
       setRef chref (items ++ [item])
       mch <- getRef (event_queue gui)
       case mch of
         Just ch -> syncNoWait (send ch (Addition item))
         _ -> done
       (if (isItemFolder item || show_leaves_in_tree gui) then
          do
            ch <- children item
            let nod = if show_leaves_in_tree gui then
                        if Prelude.not (null ch) then Node else Leaf
                      else
                        if (any isItemFolder ch) then Node else Leaf
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

-- | Returns the @CItem@ content of an item.
content :: CItem c => (Item c) -> c
content (IntFolderItem (FolderItem c _ _) _) = c
content (IntLeafItem (LeafItem c _) _ _) = c
content _ = error "GenGUI (content) : called for root"


--------------------------------------------------------------------------
-- events
--------------------------------------------------------------------------

-- | The @GenGUIEvent@ datatype.
data GenGUIEvent c =
    FocusTreeList (Maybe (Item c))
  | SelectTreeList (Maybe (Item c))
  | FocusNotepad (Item c, Bool) -- what's the Bool?
  | Dropped (Item c, [Item c])
  | Doubleclick (Item c)
  | Rightclick [Item c]
  | Addition (Item c)
  | DroppedOnTextArea [Item c]

-- | Binds a listener for gengui events to the gengui and returns
-- a corresponding event and an unbind action.
bindGenGUIEv :: CItem c => GenGUI c
   -- ^ the concerned gui.
   -> IO (Event (GenGUIEvent c), IO())
   -- ^ A pair of (event, unbind action).
bindGenGUIEv gui =
  do
    ch <- newChannel
    setRef (event_queue gui) (Just ch)
    return (receive ch, setRef (event_queue gui) Nothing)

-- | Sends the given event if bound.
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

-- | Makes treelist objects from item.
toTreeListObjects :: CItem c => Bool -> [Item c] ->
                                IO [TreeListObject (Item c)]
toTreeListObjects showLeavesInTree (it : items) =
  do
    rest <- toTreeListObjects showLeavesInTree items
    ch <- children it
    let nod = if showLeavesInTree then
                 if (any isItemFolder ch) then Node else Leaf
              else
                 if Prelude.not (null ch) then Node else Leaf
    return (newTreeListObject it nod : rest)
toTreeListObjects _ _ = return []

-- | The treelists children function.
cfun :: CItem c => Bool -> ChildrenFun (Item c)
cfun showLeavesInTree tlobj =
  do
    let item = getTreeListObjectValue tlobj
    ch <- children item
    toTreeListObjects showLeavesInTree (if showLeavesInTree then ch
                                        else filter isItemFolder ch)


--------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------

-- | Internal.
instance CItem c => Eq (GenGUI c) where
  gui1 == gui2 = win gui1 == win gui2

-- | Internal.
instance CItem c => GUIObject (GenGUI c) where
  toGUIObject gui = toGUIObject (win gui)
  cname _ = "GenGUI"

-- | A @GenGUI@ object can be destroyed.
instance CItem c => Destroyable (GenGUI c) where
  -- Destroys a @GenGUI@ object.
  destroy = destroy . toGUIObject

-- | A @GenGUI@ object is a window.
instance CItem c => Window (GenGUI c) where
  -- Iconifies the gui.
  iconify gui = iconify (win gui)
  -- Deiconifies the gui.
  deiconify gui  = deiconify (win gui)
  -- Withdraws the gui.
  withdraw gui = withdraw (win gui)
  -- Puts the gui window on top.
  putWinOnTop gui = putWinOnTop (win gui)
  -- Puts the gui window at bottom-
  putWinAtBottom gui = putWinAtBottom (win gui)

-- | You can synchronize on a gengui object.
instance CItem c => Synchronized (GenGUI c) where
  -- Synchronizes on a gengui object.
  synchronize gui = synchronize (win gui)
