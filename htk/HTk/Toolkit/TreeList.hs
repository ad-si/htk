-- | HTk\'s /TreeList/ module.
module HTk.Toolkit.TreeList (

  newTreeList, {- :: (Container par, Eq a) =>
                     par -> ChildrenFun a -> [TreeListObject a] ->
                     [Config (TreeList a)] -> IO (TreeList a)           -}
  TreeList,

  bindTreeListEv, {-  :: TreeList c ->
                         IO (Event (TreeListEvent c), IO ())            -}
  TreeListEvent(..),

  removeTreeListObject, {- :: Eq a=> TreeList a-> a-> IO () -}

  updateTreeList,        {- :: Eq a => TreeList a -> IO ()              -}
  addTreeListRootObject, {- :: Eq a => TreeList a ->
                                       TreeListObject a -> IO ()        -}
  addTreeListSubObject,  {- :: Eq a => TreeList a -> a ->
                                       TreeListObject a -> IO ()        -}

  newTreeListObject, {- :: Eq a => a -> TreeListObjectType ->
                                   TreeListObject a -}

  TreeListObject,
  TreeListObjectType(..),

  isLeaf, {- :: Eq a => TreeList a -> a -> IO (Maybe Bool)              -}
  isNode, {- :: Eq a => TreeList a -> a -> IO (Maybe Bool)              -}
  mkLeaf, {- :: Eq a => TreeList a -> a -> IO ()                        -}
  mkNode, {- :: Eq a => TreeList a -> a -> IO ()                        -}

  getTreeListObjectValue, {- :: TreeListObject a -> a                   -}
  getTreeListObjectType,  {- :: TreeListObject a -> TreeListObjectType  -}

  isTreeListObjectOpen,

  ChildrenFun,
     {- type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]  -}

  setImage,
  setTreeListObjectName,

  TreeListExportItem(..),
  TreeListState,
  exportTreeListState,
  importTreeListState,
  recoverTreeList,

  module HTk.Toolkit.CItem

) where

import Data.Maybe

import System.IO.Unsafe

import Util.Computation

import Events.Events
import Events.Channels
import Events.Synchronized

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk
import HTk.Toolkit.ScrollBox
import Data.List
import HTk.Kernel.Core
import HTk.Toolkit.CItem
import HTk.Toolkit.Name

-- internal options
intendation = 19
lineheight = 20
cwidth = 15


-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- tree lists
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- basic types
-- -----------------------------------------------------------------------

data StateEntry a =
  StateEntry (TREELISTOBJECT a)                                  -- object
             Bool                            -- open: True / closed: False
             Int                                            -- intendation
             [a]           -- ids of previously open subobjects for reopen
    deriving Eq

-- | The @ChildrenFun@ type.
type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]

-- | The @TreeList@ datatype.
data CItem c => TreeList c =
  TreeList { -- main canvas
             cnv :: Canvas,

             -- scrollbox
             scrollbox :: (ScrollBox Canvas),

             -- treelist state
             internal_state :: (Ref [StateEntry c]),

             -- node children function
             cfun :: (ChildrenFun c),

             -- selected object
             selected_object :: (Ref (Maybe (TREELISTOBJECT c))),

             -- tree list event queue
             event_queue :: Ref (Maybe (Channel (TreeListEvent c))),

             -- clean up on destruction
             clean_up :: Ref [IO ()] }


-- -----------------------------------------------------------------------
-- tree list construction
-- -----------------------------------------------------------------------

-- | Constructs a new tree list.
newTreeList :: (Container par, CItem a) =>
   par
   -- ^ the parent widget, which has to be a container widget.
   -> ChildrenFun a
   -- ^ the tree list\'s children function.
   -> [TreeListObject a]
   -- ^ the initial list of tree list objects.
   ->
   [Config (TreeList a)]
   -- ^ the list of configuration options for this tree list.
   -> IO (TreeList a)
   -- ^ A tree list.
newTreeList par cfun objs cnf =
  do
    (scr, cnv) <- newScrollBox par (\p -> newCanvas p []) []
    stateref <- newRef []
    selref <- newRef Nothing
    evq <- newRef Nothing
    cleanup <- newRef []
    let treelist = TreeList { cnv = cnv,
                              scrollbox = scr,
                              internal_state = stateref,
                              cfun = cfun,
                              selected_object = selref,
                              event_queue = evq,
                              clean_up = cleanup }
    foldl (>>=) (return treelist) cnf
    rootobjs <- mapM (\ (TreeListObject (val, objtype)) ->
                          do
                            nm <- getName val
                            mkTreeListObject treelist val
                                             (objtype == Node)
                                             False [name nm]) objs
    let toStateEntry obj = StateEntry obj False 0 []
    setRef stateref (map toStateEntry rootobjs)
    let setImg obj =
          do
            pho <- getIcon (val obj)
            obj_img obj # photo pho
    mapM setImg rootobjs
    (if not(null rootobjs) then
       do
         packTreeListObject (head rootobjs) True (5, 5)
         let packObjs :: CItem a => Position -> [TREELISTOBJECT a] ->
                                    IO ()
             packObjs (x, y) (obj : objs) =
               packTreeListObject obj False (x, y) >>
               packObjs (x,  y + Distance lineheight) objs
             packObjs _ _ = done
         packObjs (5, 5 + Distance lineheight) (tail rootobjs)
         updScrollRegion cnv stateref
     else done)
    (press, ub) <- bindSimple cnv (ButtonPress (Just 1))
    death <- newChannel
    let listenCnv :: Event ()
        listenCnv =
          (press >> always (deselect treelist) >> listenCnv) +>
          receive death
    _ <- spawnEvent listenCnv
    setRef cleanup [ub, syncNoWait (send death ())]
    return treelist

-- | Binds a listener for tree list events to the tree list and returns
-- a corresponding event and an unbind action.
bindTreeListEv :: CItem c => TreeList c
   -- ^ the concerned tree list.
   ->
   IO (Event (TreeListEvent c), IO ())
   -- ^ A pair of (event, unbind action).
bindTreeListEv tl =
  do
    ch <- newChannel
    setRef (event_queue tl) (Just ch)
    return (receive ch, setRef (event_queue tl) Nothing)

-- | The @TreeListEvent@ datatype.
data TreeListEvent c =
    Selected (Maybe (TreeListObject c))
  | Focused (Maybe (TreeListObject c), EventInfo)
              -- event info needed for drag & drop in GenGUI

-- send an event if bound
sendEv :: CItem c => TreeList c -> TreeListEvent c -> IO ()
sendEv tl ev =
  do
    mch <- getRef (event_queue tl)
    case mch of
      Just ch -> syncNoWait (send ch ev)
      _ -> done


-- | Constructs a new tree list recovering a previously saved state.
recoverTreeList :: (Container par, CItem a) =>
   par
   -- ^ the parent widget, which has to be a container widget.
   -> ChildrenFun a
   -- ^ the tree list\'s children function.
   -> TreeListState a
   -- ^ the state to recover.
   ->
   [Config (TreeList a)]
   -- ^ the list of configuration options for this tree list.
   ->
   IO (TreeList a)
   -- ^ A tree list.
recoverTreeList par cfun st cnf =
  do
    (scr, cnv) <- newScrollBox par (\p -> newCanvas p []) []
    stateref <- newRef []
    selref <- newRef Nothing
    evq <- newRef Nothing
    cleanup <- newRef []
    let tl = TreeList { cnv = cnv,
                        scrollbox = scr,
                        internal_state = stateref,
                        cfun = cfun,
                        selected_object = selref,
                        event_queue = evq,
                        clean_up = cleanup }
    foldl (>>=) (return tl) cnf
    state <- mkEntries tl st
    setRef stateref state
    let (StateEntry root _ _ _) = head state
    pho <- getIcon (val root)
    obj_img root # photo pho
    packTreeListObject root True (5, 5)
    let mselexp = find (\ exportitem -> selected exportitem) st
    case mselexp of
      Just selexp ->
        let (Just (StateEntry obj _ _ _)) =
              find (\ (StateEntry obj' _ _
                                  _) -> val obj' == obj_val selexp) state
        in obj_nm obj # fg "white" >> obj_nm obj # bg "blue" >>
           setRef selref (Just obj)
      _ -> done
    insertObjects tl (5 + Distance intendation, 5)
                  (toObjects (tail state))
    updScrollRegion cnv stateref
    (press, ub) <- bindSimple cnv (ButtonPress (Just 1))
    death <- newChannel
    let listenCnv :: Event ()
        listenCnv = (press >> always (deselect tl) >> listenCnv) +>
                    receive death
    _ <- spawnEvent listenCnv
    setRef cleanup [ub, syncNoWait (send death ())]
    return tl

-- | Deletes all objects from the tree list.
clearTreeList :: CItem c => TreeList c
   -- ^ the concerned tree list.
   -> IO ()
   -- ^ None.
clearTreeList tl =
  do
    state <- getRef (internal_state tl)
    mapM (\ (StateEntry obj _ _ _) -> removeObject obj) state
    setRef (internal_state tl) []

getObjectFromTreeList :: CItem a => TreeList a -> a ->
                                    IO (Maybe (TREELISTOBJECT a, Bool))
getObjectFromTreeList tl objval =
  do
    state <- getRef (internal_state tl)
    let msentry = find (entryEqualsObject objval) state
    case msentry of
      Just sentry@(StateEntry obj _ _ _) ->
        return (Just (obj, head state == sentry))
      _ -> return Nothing
  where entryEqualsObject :: CItem a => a -> StateEntry a -> Bool
        entryEqualsObject objval (StateEntry obj _ _ _) =
          objval == val obj

-- | Checks for a given tree list object value if the corresponding
-- object is a node.
isNode :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned tree list object value.
   -> IO (Maybe Bool)
   -- ^ @Nothing@ if no corresponding object is
   -- found, otherwise @Just True@ if
   -- the corresponding object is a node, otherwise
   -- @Just False@.
isNode tl val =
  do
    mobj <- getObjectFromTreeList tl val
    return (case mobj of
              Just (obj, _) -> Just (is_node obj)
              _ -> Nothing)

-- | Checks for a given tree list object value if the corresponding
-- object is a leaf.
isLeaf :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned tree list object value.
   -> IO (Maybe Bool)
   -- ^ @Nothing@ if no corresponding object is
   -- found, otherwise @Just True@ if
   -- the corresponding object is a leaf, otherwise
   -- @Just False@.
isLeaf tl val =
  do
    mnode <- isNode tl val
    case mnode of
      Just b -> return (Just (not b))
      _ -> return Nothing

-- | Converts the corresponding object to a given tree list object value
-- to a node.
mkNode :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned treelist object\'s value.
   -> IO ()
   -- ^ None.
mkNode tl val =
  do
    mleaf <- isLeaf tl val
    case mleaf of
      Just True ->
        do
          Just (obj, isroot) <- getObjectFromTreeList tl val
          nm <- getTreeListObjectName obj
          [(x, y)] <- getCoord (embedded_win obj)
          removeObject obj
          nuobj <- mkTreeListObject tl val True False [name nm]
          objectChanged tl obj nuobj
          pho <- getIcon val
          obj_img nuobj # photo pho
          packTreeListObject nuobj isroot (x - 15, y)
      _ -> done

-- | Converts the corresponding object to a given tree list object value
-- to a leaf.
mkLeaf :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned tree list object\'s value.
   -> IO ()
   -- ^ None.
mkLeaf tl val =
  do
    mnode <- isNode tl val
    case mnode of
      Just True ->
        do
          Just (obj, isroot) <- getObjectFromTreeList tl val
          Just (ch, _) <- getChildrenAndUpper tl val
          (if null ch then done
           else error "TreeList (mkLeaf) : node is not empty")
          nm <- getTreeListObjectName obj
          [(x, y)] <- getCoord (embedded_win obj)
          removeObject obj
          nuobj <- mkTreeListObject tl val False False [name nm]
          objectChanged tl obj nuobj
          pho <- getIcon val
          obj_img nuobj # photo pho
          packTreeListObject nuobj isroot (x - 15, y)
      _ -> done

-- | Removes the corresponding objects to a given tree list object value
-- from the tree list.
removeTreeListObject :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned tree list object\'s value.
   -> IO ()
   -- ^ None.
removeTreeListObject tl val =
  do
    mobj <- getObjectFromTreeList tl val
    case mobj of
      Just (obj, _) ->
        do mch <- getChildrenAndUpper tl val
           case mch of
             Just (ch, upper) ->
               do mapM removeObject (obj : ch)
                  done
             _ -> done
      _ -> done

getChildrenAndUpper :: CItem a => TreeList a -> a ->
                                  IO (Maybe ([TREELISTOBJECT a],
                                             [TREELISTOBJECT a]))
getChildrenAndUpper tl objval =
  let getChildrenAndUpper' :: CItem a => a -> [StateEntry a] ->
                                         Maybe ([TREELISTOBJECT a],
                                                [TREELISTOBJECT a])
      getChildrenAndUpper' objval ((StateEntry obj _ intend _) : ents) =
        if val obj == objval then
          Just (getChildrenAndUpper'' ents intend [])
        else getChildrenAndUpper' objval ents
      getChildrenAndUpper' _ _ = Nothing

      getChildrenAndUpper'' :: CItem a => [StateEntry a] -> Int ->
                                          [TREELISTOBJECT a] ->
                                          ([TREELISTOBJECT a],
                                           [TREELISTOBJECT a])
      getChildrenAndUpper'' ((StateEntry obj _ intend' _) : ents) intend
                            ch =
        if intend' > intend then getChildrenAndUpper'' ents intend
                                                       (ch ++ [obj])
        else (ch, map (\ (StateEntry obj _ _ _) -> obj) ents)
  in do
       state <- getRef (internal_state tl)
       return (getChildrenAndUpper' objval state)

objectChanged :: CItem a => TreeList a -> TREELISTOBJECT a ->
                            TREELISTOBJECT a -> IO ()
objectChanged tl obj nuobj =
  let objectChanged' (ent@(StateEntry obj' isopen intend sub) : ents) =
        if obj == obj' then
          StateEntry nuobj isopen intend sub : objectChanged' ents
        else ent : objectChanged' ents
      objectChanged' _ = []
  in do
       state <- getRef (internal_state tl)
       setRef (internal_state tl) (objectChanged' state)

-- | Updates the tree list by recalling the children function for all opened
-- objects.
updateTreeList :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> IO ()
   -- ^ None.
updateTreeList tl =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       let (StateEntry root isopen _ _) = (head state)
       if isopen then pressed root >> pressed root else done)


-- -----------------------------------------------------------------------
-- adding of objects (while running)
-- -----------------------------------------------------------------------

-- | Adds a subobject to a tree list object.
addTreeListSubObject :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the parent object\'s value.
   ->
   TreeListObject a
   -- ^ the new tree list object to add.
   -> IO ()
   -- ^ None.
addTreeListSubObject tl parval obj@(TreeListObject (objval, objtype)) =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       (if visibleAndOpen state parval then
          do
            nm <- getName objval
            (lowerobj, upperobj, parintend, y) <- sep state parval
            intobj <-
              mkTreeListObject tl objval (objtype == Node) False [name nm]
            setRef (internal_state tl)
                   (lowerobj ++
                    [StateEntry intobj False (parintend + 1) []] ++
                    upperobj)
            mapM (shiftObject lineheight) upperobj
            pho <- getIcon objval
            obj_img intobj # photo pho
            let (StateEntry obj _ _ _) = last lowerobj
            packTreeListObject intobj False
                               (5 + Distance ((parintend + 1) *
                                              intendation),
                                y + Distance lineheight)
            updScrollRegion (cnv tl) (internal_state tl)
        else done))
  where visibleAndOpen :: CItem a => [StateEntry a] -> a -> Bool
        visibleAndOpen state parval =
          let msentry = find (\ (StateEntry obj _ _ _) ->
                                val obj == parval) state
          in case msentry of
               Just (StateEntry obj isopen _ _) -> isopen
               _ -> False

        sep :: CItem a => [StateEntry a] -> a ->
                          IO ([StateEntry a], [StateEntry a], Int,
                              Distance)
        sep sentries parval = sep1 sentries parval []

        sep1 :: CItem a => [StateEntry a] -> a -> [StateEntry a] ->
                           IO ([StateEntry a], [StateEntry a], Int,
                               Distance)
        sep1 (ent@(StateEntry obj _ intend _) : ents) parval lower =
          if val obj == parval then
            do
              [(_, y)] <- getCoord (embedded_win obj)
              sep2 ents intend (lower ++ [ent]) y
          else sep1 ents parval (lower ++ [ent])

        sep2 :: CItem a => [StateEntry a] -> Int -> [StateEntry a] ->
                           Distance ->
                           IO ([StateEntry a], [StateEntry a], Int,
                               Distance)
        sep2 (ent@(StateEntry obj _ intend' _) : ents) intend lower _ =
          case ents of
            [] -> do
                    [(_, y)] <- getCoord (embedded_win obj)
                    return (lower ++ [ent], [], intend, y)
            _ ->
              if intend' > intend  then sep2 ents intend
                                             (lower ++ [ent]) 0
              else do
                     [(_, y)] <- getCoord (embedded_win obj)
                     return (lower, ent : ents, intend,
                             y - Distance lineheight)
        sep2 _ intend lower y =
          return (lower, [], intend, y)

-- | Adds a toplevel tree list object.
addTreeListRootObject :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> TreeListObject a
   -- ^ the tree list object to add.
   ->
   IO ()
   -- ^ None.
addTreeListRootObject tl obj@(TreeListObject (val, objtype)) =
  synchronize tl
    (do
       nm <- getName val
       tlobj <- mkTreeListObject tl val (objtype == Node) False [name nm]
       pho <- getIcon val
       obj_img tlobj # photo pho
       objs <- getRef (internal_state tl)
       setRef (internal_state tl) (objs ++ [StateEntry tlobj False 0 []])
       packTreeListObject tlobj (length objs == 0)
                          (5, 5 + Distance (length objs * lineheight))
       updScrollRegion (cnv tl) (internal_state tl))

startObjectInteractor ::  CItem a => TREELISTOBJECT a -> IO ()
startObjectInteractor obj =
  do
    (press, ub) <- bindSimple (plusminus obj) (ButtonPress (Just 1))
    addUnbindAction obj ub
    death <- newChannel
    let listenObject :: Event ()
        listenObject =    (press >> always (pressed obj) >> listenObject)
                       +> (receive death)
    _ <- spawnEvent listenObject
    addUnbindAction obj (syncNoWait (send death ()))
    done

addUnbindAction :: CItem a => TREELISTOBJECT a -> IO () -> IO ()
addUnbindAction obj ub =
  do
    ubs <- getRef (ub_acts obj)
    setRef (ub_acts obj) (ub : ubs)

vLineLength :: CItem c => TREELISTOBJECT c -> IO Distance
vLineLength obj =
  do
    state <- getRef (internal_state (treelist obj))
    return(start obj (reverse state))
  where start :: CItem a => TREELISTOBJECT a -> [StateEntry a] ->
                            Distance
        start obj (StateEntry obj' _ intend _ : sentries) =
          if obj' == obj then inner intend 0 sentries
          else start obj sentries
        inner :: CItem a => Int -> Int -> [StateEntry a] ->  Distance
        inner intend n (StateEntry obj _ intend' _ : sentries) =
          if intend' <= intend then
            (Distance (n * lineheight) +
             Distance (if (is_node obj) then lineheight - 13
                       else lineheight - 9))
          else inner intend (n + 1) sentries
        inner _ _ _ = Distance (lineheight - 13)

-- packs an (internal) tree list object
packTreeListObject :: CItem a => TREELISTOBJECT a -> Bool -> Position ->
                                 IO ()
packTreeListObject obj isroot pos@(x, y) =
  let hline = (selHLine (obj_lines obj))
      vline = (selVLine (obj_lines obj))
  in do
       embedded_win obj # coord [(x + 15, y)]
       dist <- vLineLength obj
       (if (is_node obj) then
          do
            plusminus obj # position (x, y + 5)
            hline # coord [(x + 9, y + 9), (x + 13, y + 9)]
            if not isroot then
              vline # coord [(x + 4, y + 5),
                             (x + 4, y - dist)] >> done else done
        else
          do
            hline # coord [(x + 4, y + 9), (x + 13, y + 9)]
            (if not isroot then
               vline # coord [(x + 4, y + 9), (x + 4, y - dist)] >>
               done
             else done)
            done)
       if (is_node obj) then startObjectInteractor obj else done


-- -----------------------------------------------------------------------
-- TreeList instances
-- -----------------------------------------------------------------------

-- | Internal.
instance CItem c => GUIObject (TreeList c) where
  toGUIObject tl = toGUIObject (scrollbox tl)
  cname _ = "TreeList"

-- | A tree list can be destroyed.
instance CItem c => Destroyable (TreeList c) where
  -- Destroys a tree list.
  destroy = destroy . toGUIObject

-- | A tree list has standard widget properties
-- (concerning focus, cursor).
instance CItem c => Widget (TreeList c)

-- | You can synchronize on a tree list.
instance CItem c => Synchronized (TreeList c) where
  -- Synchronizes on a tree list.
  synchronize = synchronize . toGUIObject

-- | A tree list has a configureable border.
instance CItem c => HasBorder (TreeList c)

-- | A tree list has a configureale background colour.
instance CItem c => HasColour (TreeList c) where
  legalColourID tl = hasBackGroundColour (cnv tl)
  setColour tl cid col = setColour (cnv tl) cid col >> return tl
  getColour tl cid = getColour (cnv tl) cid

-- | A tree list has a configureable size.
instance CItem c => HasSize (TreeList c) where
  width s tl = (cnv tl) # width s >> return tl
  getWidth tl = getWidth (cnv tl)
  height s tl = (cnv tl) # height s >> return tl
  getHeight tl = getHeight (cnv tl)


-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- tree list objects
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- basic types
-- -----------------------------------------------------------------------

-- | The @TreeListObjectType@ datatype.
data TreeListObjectType = Node | Leaf deriving Eq

-- | The @TreeListObject@ datatype.
newtype TreeListObject a =
  TreeListObject (a, TreeListObjectType)

data CItem a => TREELISTOBJECT a =        -- internal representation
  TREELISTOBJECT { val :: a,                                      -- value
                   treelist :: TreeList a,                       -- parent
                   is_node :: Bool,                        -- true if node
                   plusminus :: ImageItem,                    -- plusminus
                   obj_lines :: (Line, Line),                     -- lines
                   obj_img :: Label,                       -- object image
                   obj_nm :: Label,                         -- object name
                   embedded_win :: EmbeddedCanvasWin,        -- main frame
                   ub_acts :: Ref [IO ()] }              -- unbind actions


-- -----------------------------------------------------------------------
-- construction of tree list objects
-- -----------------------------------------------------------------------

-- | Constructs a new tree list object.
newTreeListObject :: CItem a => a
   -- ^ the object\'s value.
   -> TreeListObjectType
   -- ^ the object\'s type (node or leaf).
   ->
   TreeListObject a
   -- ^ A tree list object.
newTreeListObject val objtype = TreeListObject (val, objtype)


-- -----------------------------------------------------------------------
-- exported functionality on tree list objects
-- -----------------------------------------------------------------------

-- | Selector for the value of a tree list object.
getTreeListObjectValue :: TreeListObject a
   -- ^ the concerned tree list object.
   -> a
   -- ^ The given object\'s value.
getTreeListObjectValue obj@(TreeListObject (val, _)) = val

-- | Selector for the type of a tree list object (node or leaf).
getTreeListObjectType :: TreeListObject a
   -- ^ the concerned tree list object.
   -> TreeListObjectType
   -- ^ The object\'s type (node or leaf).
getTreeListObjectType obj@(TreeListObject (_, objtype)) = objtype

-- | True, if the object with the given value is currently opened in the
-- tree list.
isTreeListObjectOpen :: CItem c => TreeList c
   -- ^ the concerned tree list.
   -> c
   -- ^ the concerned object\'s value.
   -> IO Bool
   -- ^ @True@, if the object with the given value
   -- is currently opened in the tree list, otherwise
   -- @False@.
isTreeListObjectOpen tl v =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       let msentry = find (\ (StateEntry obj _ _ _) -> v == val obj) state
       case msentry of
         Just (StateEntry _ b _ _) -> return b
         Nothing -> return False)

-- | (Re-)sets the image of a tree list object.
setImage :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned object\'s value.
   -> Image
   -- ^ the image to set.
   -> IO ()
   -- ^ None.
setImage tl objval img =
  do
    state <- getRef (internal_state tl)
    setImage' state objval img
  where setImage' :: CItem a => [StateEntry a] -> a -> Image -> IO ()
        setImage' ((StateEntry obj _ _ _) : ents) val' img =
          if val obj == val' then obj_img obj # photo img >> done
          else setImage' ents val' img
        setImage' _ _ _ = done

-- | (Re-)sets the name of a tree list object.
setTreeListObjectName :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> a
   -- ^ the concerned object\'s value.
   -> Name
   -- ^ the name to set.
   -> IO ()
   -- ^ None.
setTreeListObjectName tl objval nm =
  do
    state <- getRef (internal_state tl)
    setName state objval
  where setName :: CItem a => [StateEntry a] -> a -> IO ()
        setName ((StateEntry obj _ _ _) : ents) val' =
          if val obj == val' then do
--                                    nm <- getName (val obj)
                                    obj_nm obj # text (full nm) >> done
          else setName ents val'
        setName _ _ = done


-- -----------------------------------------------------------------------
-- internal functionality on tree list objects
-- -----------------------------------------------------------------------

-- shifts a displayed object by dy pixels (vertical)
shiftObject :: CItem c => Int -> StateEntry c -> IO ()
shiftObject dy (StateEntry obj _ _ _) =
  do
    (if (is_node obj) then moveItem (plusminus obj) 0 (Distance dy) >>
                           done
     else done)
    moveItem (selHLine (obj_lines obj)) 0 (Distance dy)
    moveItem (selVLine (obj_lines obj)) 0 (Distance dy)
    coords <- getCoord (selVLine (obj_lines obj))
    hlinelength <- vLineLength obj
    (let (x, y) = selLower coords
     in (selVLine (obj_lines obj)) #
          coord [(x, y), (x, y - hlinelength -
                             if (is_node obj) then 5 else 9)])
    moveItem (embedded_win obj) 0 (Distance dy)

-- selects the lowest position from a list of positions
selLower :: Coord -> Position
selLower coords = selLower' (head coords) (tail coords)
  where selLower' :: Position -> Coord -> Position
        selLower' l@(_, yl) (c@(_, y) : cs) =
          if y > yl then selLower' c cs else selLower' l cs
        selLower' l _ = l

-- updates the scroll region
updScrollRegion :: CItem a => Canvas -> Ref [StateEntry a] -> IO ()
updScrollRegion cnv stateref =
  do
    state <- getRef stateref
    updScrollRegion' cnv 0 0 state
  where updScrollRegion' :: CItem a => Canvas -> Distance -> Distance ->
                                       [StateEntry a] -> IO ()
        updScrollRegion' cnv x y
                         ((StateEntry obj _ _ _) : sentries) =
          do
            Just (_, _, x', y') <- bbox cnv (embedded_win obj)
            updScrollRegion' cnv (max x x') (max y y') sentries
        updScrollRegion' cnv x y _ =
          (cnv # scrollRegion ((0, 0), (x, y))) >> done

-- inserts objects into the treelist
insertObjects :: CItem a => TreeList a -> Position ->
                            [(Int, Bool, TREELISTOBJECT a)] -> IO ()
insertObjects tl (x, y) chobjs =
  do
    state <- getRef (internal_state tl)
    insertObjects' (cnv tl) (x, y + Distance lineheight) chobjs
  where insertObjects' :: CItem a => Canvas -> Position ->
                                     [(Int, Bool, TREELISTOBJECT a)] ->
                                     IO ()
        insertObjects' cnv (x, y) ((i, b, obj) : objs) =
          do
            pho <- getIcon (val obj)
            obj_img obj # photo pho
            packTreeListObject obj False
                               (5 + Distance (i * intendation), y)
            insertObjects' cnv (x, y + Distance lineheight) objs
        insertObjects' _ (x, y) _ = done

-- removes an object from the treelist
removeObject :: CItem a => TREELISTOBJECT a -> IO ()
removeObject obj =
  do
    ubs <- getRef (ub_acts obj)
    mapM id ubs
    destroy (embedded_win obj)
    if (is_node obj) then destroy (plusminus obj) else done
    destroy (selHLine (obj_lines obj))
    destroy (selVLine (obj_lines obj))
    setRef (ub_acts obj) []
    done

-- gets information about a tree list object
-- (intendation, object open or not, ids of previously open subobjects)
getObjInfo :: CItem a => TREELISTOBJECT a -> [StateEntry a] ->
                         IO (Int, Bool, [a])
getObjInfo obj (StateEntry obj' isopen i prevopen : entries) =
  if obj == obj' then return (i, isopen, prevopen)
  else getObjInfo obj entries

-- constructs a state entry
mkEntry :: CItem a => (Int, Bool, TREELISTOBJECT a) -> StateEntry a
mkEntry (i, b, obj) = StateEntry obj b i []

-- gets the displayed children of a tree list object
getChildren :: CItem a => [StateEntry a] -> TREELISTOBJECT a ->
                          IO ([TREELISTOBJECT a], [a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: CItem a => [StateEntry a] -> TREELISTOBJECT a ->
                                   Int -> [TREELISTOBJECT a] -> [a] ->
                                   IO ([TREELISTOBJECT a], [a])
        getChildren' (st@(StateEntry obj' isopen intend _) : es)
                     obj i objs opensubobjvals =
          if (i == -1 && obj /= obj') then
            getChildren' es obj i objs opensubobjvals
          else
            if (obj == obj') then
              getChildren' es obj intend objs opensubobjvals
            else
              if intend > i then
                if isopen then
                  getChildren' es obj i (obj' : objs)
                               (val obj' : opensubobjvals)
                else
                  getChildren' es obj i (obj' : objs) opensubobjvals
              else
                return (objs, opensubobjvals)
        getChildren' _ _ _ objs opensubobjvals =
          return (objs, opensubobjvals)

-- reopens previously open sub-objects of a tree list object
-- (used while opening)
reopenSubObjects :: CItem a => ChildrenFun a -> [a] ->
                               [(Int, TREELISTOBJECT a)] ->
                               IO [(Int, Bool, TREELISTOBJECT a)]
reopenSubObjects c_fun prevopen ((i, tlobj) : objs) =
  if elem (val tlobj) prevopen then
    do
      plusminus tlobj # photo minusImg
      ch <- c_fun (TreeListObject (val tlobj, if (is_node tlobj) then Node
                                              else Leaf))
      thisobjch <- mkTreeListObjects (treelist tlobj) ch (i + 1) prevopen
      chobjs <- reopenSubObjects c_fun prevopen thisobjch
      rest <- reopenSubObjects c_fun prevopen objs
      return (((i, True, tlobj) : chobjs) ++ rest)
  else
    do
      rest <- reopenSubObjects c_fun prevopen objs
      return ((i, False, tlobj) : rest)
reopenSubObjects _ _ _ = return []

-- event handler (buttonpress)
pressed :: CItem c => TREELISTOBJECT c -> IO ()
pressed obj =
  synchronize (treelist obj)
    (do
       state <- getRef (internal_state (treelist obj))
       c <- getCoord (embedded_win obj)
       index <-
         return
           ((fromJust
               (elemIndex obj (map (\ (StateEntry obj _ _ _) -> obj)
                          state))) + 1)
       (i, isopen, prevopen) <- getObjInfo obj state
       (if isopen then
          do                                              -- close
            plusminus obj # photo plusImg
            (children, opensubobjvals) <- getChildren state obj
            mapM removeObject children
            setRef (internal_state (treelist obj))
                   (take (index - 1) state ++
                    [StateEntry obj False i opensubobjvals] ++
                    drop (index + length children) state)
            mapM (shiftObject (-(length children) * lineheight))
                 (drop (index + length children) state)
            done
        else
          do                                               -- open
            plusminus obj # photo minusImg
            ch <- (cfun (treelist obj))
                    (TreeListObject (val obj, if (is_node obj) then Node
                                              else Leaf))
            thisobjch <- mkTreeListObjects (treelist obj) ch (i + 1)
                                           prevopen
            chobjs <- reopenSubObjects (cfun (treelist obj)) prevopen
                                       thisobjch
            setRef (internal_state (treelist obj))
                   (take (index - 1) state ++
                    [StateEntry obj True i []] ++
                    map mkEntry chobjs ++ drop index state)
            mapM (shiftObject ((length chobjs) * lineheight))
                 (drop index state)
            insertObjects (treelist obj) (head c) chobjs
            done)
       updScrollRegion (cnv (treelist obj))
                       (internal_state (treelist obj)))

-- selects objects and send the concerned event
selectObject :: CItem c => TreeList c -> TREELISTOBJECT c -> IO ()
selectObject tl obj =
  do
    unmarkSelectedObject tl
    setRef (selected_object tl) (Just obj)
    obj_nm obj # fg "white"
    obj_nm obj # bg "blue"
    sendEv tl (Selected (Just (TreeListObject (val obj,
                                               if (is_node obj) then Node
                                               else Leaf))))
    done

-- deselects an object
deselect :: CItem c => TreeList c -> IO ()
deselect tl =
  do
    unmarkSelectedObject tl
    setRef (selected_object tl) Nothing
    sendEv tl (Selected Nothing)
--    syncNoWait (send (selection_ch tl) Nothing)
    done

-- unmarks the sekected object
unmarkSelectedObject :: CItem c => TreeList c -> IO ()
unmarkSelectedObject tl =
  do
    sel <- getRef (selected_object tl)
    case sel of
      Just obj -> do
                    obj_nm obj # fg "black"
                    obj_nm obj # bg "white"
                    done
      _ -> done

-- True for a selected object
isSelectedTreeList :: CItem c => TreeList c -> TREELISTOBJECT c -> IO Bool
isSelectedTreeList tl obj =
  do
    sel <- getRef (selected_object tl)
    case sel of
      Just s -> return (s == obj)
      _ -> return False

-- constructs the internal representation of new tree list objects
mkTreeListObjects :: CItem a => TreeList a -> [TreeListObject a] -> Int ->
                                [a] -> IO [(Int, TREELISTOBJECT a)]
mkTreeListObjects tl objs i prevopen =
  mapM (mk tl i prevopen) objs
  where mk :: CItem a => TreeList a -> Int -> [a] -> TreeListObject a ->
                         IO (Int, TREELISTOBJECT a)
        mk tl i prevopen (TreeListObject (val, objtype)) =
          do
            nm <- getName val
            obj <- mkTreeListObject tl val
                     (if objtype == Node then True else False)
                     (elem val prevopen) [name nm]
            return (i, obj)

-- constructs the internal representation of a single tree list object
mkTreeListObject :: CItem a => TreeList a -> a -> Bool -> Bool ->
                               [Config (TREELISTOBJECT a)] ->
                               IO (TREELISTOBJECT a)
mkTreeListObject tl val isnode isopen cnf =
  do
    box <- newHBox (cnv tl) [background "white"]
    drawnstuff <-
      do
        hline <- createLine (cnv tl) [coord [(-200, -200), (-200, -200)]]
        vline <- createLine (cnv tl) [coord [(-200, -200), (-200, -200)]]
        return (hline, vline)
    plusminus <- createImageItem (cnv tl)
                   [coord [(-200, -200)], canvAnchor NorthWest,
                    photo (if isopen then minusImg else plusImg)]
    img <- newLabel box [background "white"]
    pack img [Side AtLeft]
    txt <- newLabel box [background "white", font (Lucida, 12::Int)]
    pack txt [Side AtRight]
    emb <- createEmbeddedCanvasWin (cnv tl) box [coord [(-200, -200)],
                                                 canvAnchor NorthWest]
    unbind_actions <- newRef []
    let obj = TREELISTOBJECT { val = val,
                               treelist = tl,
                               is_node = isnode,
                               plusminus = plusminus,
                               obj_lines = drawnstuff,
                               obj_img = img,
                               obj_nm = txt,
                               embedded_win = emb,
                               ub_acts = unbind_actions }
    foldl (>>=) (return obj) cnf
    (enterTxt, ub) <- bind txt [WishEvent [] Enter]
    addUnbindAction obj ub
    death <- newChannel
    addUnbindAction obj (syncNoWait (send death ()))
    (leaveTxt, ub) <- bind txt [WishEvent [] Leave]
    addUnbindAction obj ub
    (pressTxt, ub) <- bindSimple txt (ButtonPress (Just 1))
    addUnbindAction obj ub
    let listenObject :: Event ()
        listenObject =
             (pressTxt >> always (selectObject tl obj) >> listenObject)
          +> (do
                ev_inf <- leaveTxt
                always (do
                          b <- isSelectedTreeList tl obj
                          if b then done else txt # bg "white" >>
                                              txt # fg "black" >> done
                          sendEv tl (Focused (Nothing, ev_inf)))
                listenObject)
          +> (do
                ev_inf <- enterTxt
                always (do
                          b <- isSelectedTreeList tl obj
                          if b then done else txt # bg "grey" >>
                                              txt # fg "white" >> done
                          sendEv tl (Focused
                                       (Just (TreeListObject
                                                (val,
                                                 if isnode then Node
                                                 else Leaf)), ev_inf)))
                listenObject)
          +> receive death
    _ <- spawnEvent listenObject
    return obj

-- selector for the horizontal line of an (internal) tree list object
selHLine :: (Line, Line) -> Line
selHLine (hline, _) = hline

-- selector for the vertical line of an (internal) tree list object
selVLine :: (Line, Line) -> Line
selVLine (_, vline) = vline


-- -----------------------------------------------------------------------
-- internal tree list object instances
-- -----------------------------------------------------------------------

instance CItem a => Eq (TREELISTOBJECT a) where
  obj1 == obj2 = obj_nm obj1 == obj_nm obj2

instance CItem a => GUIObject (TREELISTOBJECT a) where
  toGUIObject obj = toGUIObject (embedded_win obj)
  cname _ = "TREELISTOBJECT"

instance CItem a => HasPhoto (TREELISTOBJECT a) where
  photo i obj = obj_img obj # photo i >> return obj
  getPhoto obj = getPhoto (obj_img obj)

name :: CItem a => Name -> Config (TREELISTOBJECT a)
name nm obj = obj_nm obj # text (full nm) >> return obj

getTreeListObjectName :: CItem a => TREELISTOBJECT a -> IO Name
getTreeListObjectName obj =
  do
    nm <- getName (val obj)
    return nm


-- -----------------------------------------------------------------------
-- state import / export
-- -----------------------------------------------------------------------

-- | Imports a previously saved tree list state.
importTreeListState :: CItem a => TreeList a
   -- ^ the concerned tree list.
   -> TreeListState a
   -- ^ the state to import.
   -> IO ()
   -- ^ None.
importTreeListState tl st =
  synchronize tl
    (do
       clearTreeList tl
       state <- mkEntries tl st
       setRef (internal_state tl) state
       let StateEntry root _ _ _ = head state
       packTreeListObject root True (5, 5)
       pho <- getIcon (val root)
       obj_img root # photo pho
       insertObjects tl (5 + Distance intendation, 5)
                        (toObjects (tail state))
       updScrollRegion (cnv tl) (internal_state tl))

toObjects :: [StateEntry a] -> [(Int, Bool, TREELISTOBJECT a)]
toObjects (StateEntry obj isopen intend _  : ents) =
  (intend, isopen, obj) : toObjects ents
toObjects _ = []

mkEntries :: CItem a => TreeList a -> TreeListState a -> IO [StateEntry a]
mkEntries tl (i : is) =
  do
    nm <- getName (obj_val i)
    obj <- mkTreeListObject tl (obj_val i)
             (if obj_type i == Node then True else False) (open i)
             [name nm]
    rest <- mkEntries tl is
    return (StateEntry obj (open i) (intend i) [] : rest)
mkEntries _ _ = return []

data TreeListExportItem a =
  TreeListExportItem { obj_val :: a,
                       obj_type :: TreeListObjectType,
                       open :: Bool,                    -- ignored if leaf
                       intend :: Int,
                       selected :: Bool }        -- yet ignored, multiple
                                                 -- selections to come ...

type TreeListState a = [TreeListExportItem a]

-- | Exports the tree list\'s state.
exportTreeListState :: CItem c => TreeList c
   -- ^ the concerned tree list.
   -> IO (TreeListState c)
   -- ^ The tree list\'s state.
exportTreeListState tl =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       exportTreeListState' tl state)
  where exportTreeListState' :: CItem c =>
                                TreeList c -> [StateEntry c] ->
                                IO (TreeListState c)
        exportTreeListState' tl (StateEntry obj open intendation _ :
                                 ents) =
          do
            sel <- isSelectedTreeList tl obj
            rest <- exportTreeListState' tl ents
            return (TreeListExportItem
                      { obj_val = val obj,
                        obj_type = if (is_node obj) then Node else Leaf,
                        open = open,
                        intend = intendation,
                        selected = sel} : rest)
        exportTreeListState' _ _ = return []


-- -----------------------------------------------------------------------
-- images
-- -----------------------------------------------------------------------

plusImg :: Image
plusImg = unsafePerformIO (newImage [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACFJSPiTHdYYIcEopKZax1s35NINcVADs="])
{-# NOINLINE plusImg #-}

minusImg :: Image
minusImg = unsafePerformIO (newImage [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACEZSPiTHdYYKcUNAZb9Vb5ysUADs="])
{-# NOINLINE minusImg #-}
