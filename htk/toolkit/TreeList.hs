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

module TreeList (

  cleanUp,

  newTreeList, {- :: (Container par, Eq a) =>
                     par -> ChildrenFun a -> ImageFun a ->
                     [TreeListObject a] -> [Config (TreeList a)] ->
                     IO (TreeList a)                                    -}
  TreeList,

  bindTreeListEv, {-  :: TreeList c ->
                         IO (Event (TreeListEvent c), IO ())            -}
  TreeListEvent(..),

  updateTreeList,        {- :: Eq a => TreeList a -> IO ()              -}
  addTreeListRootObject, {- :: Eq a => TreeList a ->
                                       TreeListObject a -> IO ()        -}
  addTreeListSubObject,  {- :: Eq a => TreeList a -> a ->
                                       TreeListObject a -> IO ()        -}

  newTreeListObject, {- :: Eq a => a -> String -> TreeListObjectType ->
                                   TreeListObject a -}

  TreeListObject,
  TreeListObjectType(..),

  isLeaf, {- :: Eq a => TreeList a -> a -> IO (Maybe Bool)              -}
  isNode, {- :: Eq a => TreeList a -> a -> IO (Maybe Bool)              -}
  mkLeaf, {- :: Eq a => TreeList a -> a -> IO ()                        -}
  mkNode, {- :: Eq a => TreeList a -> a -> IO ()                        -}
              
  getTreeListObjectValue, {- :: TreeListObject a -> a                   -}
--  getTreeListObjectName,  {- :: TreeListObject a -> String              -}
  getTreeListObjectType,  {- :: TreeListObject a -> TreeListObjectType  -}

  isTreeListObjectOpen,

  ChildrenFun,

--  selectionEvent,
--  focusEvent,

  setImage,
  setTreeListObjectName,

  TreeListExportItem(..),
  TreeListState(..),
  exportTreeListState,
  importTreeListState,
  recoverTreeList,

  module CItem

) where

import HTk
import ScrollBox
import List
import ReferenceVariables
import Core
import Maybe
import CItem
import Name

-- internal options
intendation = 19
lineheight = 20
cwidth = 15



-- ***********************************************************************
-- tree lists
-- ***********************************************************************

-- -----------------------------------------------------------------------
-- basic types
-- -----------------------------------------------------------------------

data CItem a => StateEntry a =
  StateEntry (TREELISTOBJECT a)                                  -- object
             Bool                            -- open: True / closed: False
             Int                                            -- intendation
             [a]           -- ids of previously open subobjects for reopen

type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]

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

---
-- Constructs a new tree list.
newTreeList :: (Container par, CItem a) =>
               par -> ChildrenFun a -> [TreeListObject a] ->
               [Config (TreeList a)] -> IO (TreeList a)
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
    let setImg (TREELISTOBJECT val _ _ _ _ img _ _ _) =
          do
            pho <- getIcon val
            img # photo pho
    mapM setImg rootobjs
    packTreeListObject (head rootobjs) True (5, 5)
    let packObjs :: CItem a => Position -> [TREELISTOBJECT a] -> IO ()
        packObjs (x, y) (obj : objs) =
          packTreeListObject obj False (x, y) >>
          packObjs (x,  y + Distance lineheight) objs
        packObjs _ _ = done
    packObjs (5, 5 + Distance lineheight) (tail rootobjs)
    updScrollRegion cnv stateref
    (press, ub) <- bindSimple cnv (ButtonPress (Just (BNo 1)))
    death <- newChannel
    let listenCnv :: Event ()
        listenCnv =
          (press >> always (deselect treelist) >> listenCnv) +>
          receive death
    spawnEvent listenCnv
    setRef cleanup [ub, syncNoWait (send death ())]
    return treelist

cleanUp :: CItem c => TreeList c -> IO ()
cleanUp tl =
  do
    state <- getRef (internal_state tl)
    mapM cleanUp' state
    cleanup <- getRef (clean_up tl)
    mapM id cleanup
    done
  where cleanUp' :: CItem c => StateEntry c -> IO ()
        cleanUp' (StateEntry (TREELISTOBJECT _ _ _ _ _ _ _ _ ub) _ _ _) =
          do
            ubs <- getRef ub
            mapM id ubs
            done

bindTreeListEv :: CItem c => TreeList c ->
                             IO (Event (TreeListEvent c), IO ())
bindTreeListEv tl =
  do
    ch <- newChannel
    setRef (event_queue tl) (Just ch)
    return (receive ch, setRef (event_queue tl) Nothing)

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


---
-- Constructs a new tree list recovering a previously saved state.
recoverTreeList :: (Container par, CItem a) =>
                   par -> ChildrenFun a -> TreeListState a ->
                   [Config (TreeList a)] ->
                   IO (TreeList a)
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
    let (StateEntry root@(TREELISTOBJECT id _ isnode _ _ img_lab txt_lab _
                                          _)
                    _ _ _) = head state
    pho <- getIcon id
    img_lab # photo pho
    packTreeListObject root True (5, 5)
    let mselexp = find (\ exportitem -> selected exportitem) st
    case mselexp of
      Just selexp ->
        let (Just (StateEntry obj@(TREELISTOBJECT _ _ _ _ _ _ txt _ _) _ _
                              _)) =
              find (\ (StateEntry (TREELISTOBJECT id _ _ _ _ _ _ _ _) _ _
                                  _) -> id == val selexp) state
        in txt # fg "white" >> txt # bg "blue" >> setRef selref (Just obj)
      _ -> done
    insertObjects tl (5 + Distance intendation, 5)
                  (toObjects (tail state))
    updScrollRegion cnv stateref
    (press, ub) <- bindSimple cnv (ButtonPress (Just (BNo 1)))
    death <- newChannel
    let listenCnv :: Event ()
        listenCnv = (press >> always (deselect tl) >> listenCnv) +>
                    receive death
    spawnEvent listenCnv
    setRef cleanup [ub, syncNoWait (send death ())]
    return tl

---
-- Deletes all objects from a tree list.
clearTreeList :: CItem c => TreeList c -> IO ()
clearTreeList tl =
  do
    state <- getRef (internal_state tl)
    putStr "deleting "
    mapM (\ (StateEntry (TREELISTOBJECT _ _ _ img (line1, line2) img_lab
                                        txt_lab emb acts)
                        _ _ _) -> do
                                    putStr "."
                                    acts' <- getRef acts
                                    mapM id acts'
                                    putStr "*"
                                    destroy img
                                    putStr "!"
                                    destroy line1
                                    putStr "!"
                                    destroy line2
                                    putStr "!"
                                    destroy img_lab
                                    putStr "!"
                                    destroy txt_lab
                                    putStr "!"
                                    destroy emb
                                    putStr "!") state
    setRef (internal_state tl) []

getObjectFromTreeList :: CItem a => TreeList a -> a ->
                                    IO (Maybe (TREELISTOBJECT a))
getObjectFromTreeList tl val =
  do
    state <- getRef (internal_state tl)
    let msentry = find (entryEqualsObject val) state
    case msentry of
      Just (StateEntry obj _ _ _) -> return (Just obj)
      _ -> return Nothing
  where entryEqualsObject :: CItem a => a -> StateEntry a -> Bool
        entryEqualsObject val (StateEntry (TREELISTOBJECT val' _ _ _ _ _
                                                          _ _ _) _ _ _) =
          val == val'

isNode :: CItem a => TreeList a -> a -> IO (Maybe Bool)
isNode tl val =
  do
    mobj <- getObjectFromTreeList tl val
    return (case mobj of
              Just (TREELISTOBJECT _ _ isnode _ _ _ _ _ _) ->
                Just isnode
              _ -> Nothing)

isLeaf :: CItem a => TreeList a -> a -> IO (Maybe Bool)
isLeaf tl val =
  do
    mnode <- isNode tl val
    case mnode of
      Just b -> return (Just (not b))
      _ -> return Nothing

mkNode :: CItem a => TreeList a -> a -> IO ()
mkNode tl val =
  do
    mleaf <- isLeaf tl val
    case mleaf of
      Just True ->
        do
          Just obj@(TREELISTOBJECT _ _ _ _ _ _ _ emb _) <-
            getObjectFromTreeList tl val
          nm <- getTreeListObjectName obj
          [(x, y)] <- getCoord emb
          removeObject tl obj
          nuobj@(TREELISTOBJECT _ _ _ _ _ img _ _ _) <-
            mkTreeListObject tl val True False [name nm]
          objectChanged tl obj nuobj
          pho <- getIcon val
          img # photo pho
          packTreeListObject nuobj False (x - 15, y)
      _ -> done

mkLeaf :: CItem a => TreeList a -> a -> IO ()
mkLeaf tl val =
  do
    mnode <- isNode tl val
    case mnode of
      Just True ->
        do
          Just obj@(TREELISTOBJECT _ _ _ _ _ _ _ emb _) <-
            getObjectFromTreeList tl val
          Just (ch, _) <- getChildrenAndUpper tl val
          (if null ch then done
           else error "TreeList (mkLeaf) : node is not empty")
          nm <- getTreeListObjectName obj
          [(x, y)] <- getCoord emb
          removeObject tl obj
          nuobj@(TREELISTOBJECT _ _ _ _ _ img _ _ _) <-
            mkTreeListObject tl val False False [name nm]
          objectChanged tl obj nuobj
--          pho <- ifun (TreeListObject (val, nm, Node))
          pho <- getIcon val
          img # photo pho
          packTreeListObject nuobj False (x - 15, y)
      _ -> done

removeTreeListObject :: CItem a => TreeList a -> a -> IO ()
removeTreeListObject tl val =
  do
    Just obj <- getObjectFromTreeList tl val
    mch <- getChildrenAndUpper tl val
    case mch of
      Just (ch, upper) ->
        do
          mapM (removeObject tl) (obj : ch)
          done
      _ -> done

getChildrenAndUpper :: CItem a => TreeList a -> a ->
                                  IO (Maybe ([TREELISTOBJECT a],
                                             [TREELISTOBJECT a]))
getChildrenAndUpper tl val =
  let getChildrenAndUpper' :: CItem a => a -> [StateEntry a] ->
                                         Maybe ([TREELISTOBJECT a],
                                                [TREELISTOBJECT a])
      getChildrenAndUpper' val ((StateEntry (TREELISTOBJECT val' _ _ _ _
                                                            _ _ _ _)
                                            _ intend _) : ents) =
        if val' == val then Just (getChildrenAndUpper'' ents intend [])
        else getChildrenAndUpper' val ents
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
       return (getChildrenAndUpper' val state)

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

updateTreeList :: CItem a => TreeList a -> IO ()
updateTreeList tl =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       let (StateEntry root isopen _ _) = (head state)
       if isopen then pressed root >> pressed root else done)


-- -----------------------------------------------------------------------
-- adding of objects (while running)
-- -----------------------------------------------------------------------

---
-- Adds a subobject to a tree list object.
addTreeListSubObject :: CItem a => TreeList a -> a ->
                                   TreeListObject a -> IO ()
addTreeListSubObject tl parval obj@(TreeListObject (val, objtype)) =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       (if visibleAndOpen state parval then
          do
            nm <- getName val
            (lowerobj, upperobj, parintend, y) <- sep state parval
            intobj@(TREELISTOBJECT _ _ _ _ drawnstuff img _ emb _) <-
              mkTreeListObject tl val (if objtype == Node then True
                                       else False) False [name nm]
            setRef (internal_state tl)
                   (lowerobj ++
                    [StateEntry intobj False (parintend + 1) []] ++
                    upperobj)
            mapM (shiftObject lineheight) upperobj
            pho <- getIcon val
            img # photo pho
            let (StateEntry obj _ _ _) = last lowerobj
            packTreeListObject intobj False
                               (5 + Distance ((parintend + 1) *
                                              intendation),
                                y + Distance lineheight)
            updScrollRegion (cnv tl) (internal_state tl)
        else done))
  where visibleAndOpen :: CItem a => [StateEntry a] -> a -> Bool
        visibleAndOpen state parval =
          let msentry = find (\ (StateEntry (TREELISTOBJECT val _ _ _ _
                                                            _ _ _ _)
                                            _ _ _) ->
                                val == parval) state
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
        sep1 (ent@(StateEntry (TREELISTOBJECT val _ _ _ _ _ _ emb _) _
                              intend _) : ents) parval lower =
          if val == parval then
            do
              [(_, y)] <- getCoord emb
              sep2 ents intend (lower ++ [ent]) y
          else sep1 ents parval (lower ++ [ent])

        sep2 :: CItem a => [StateEntry a] -> Int -> [StateEntry a] ->
                           Distance ->
                           IO ([StateEntry a], [StateEntry a], Int,
                               Distance)
        sep2 (ent@(StateEntry (TREELISTOBJECT _ _ _ _ drawnstuff _ _
                                              emb _)
                              _ intend' _) : ents) intend lower _ =
          case ents of
            [] -> do
                    [(_, y)] <- getCoord emb
                    return (lower ++ [ent], [], intend, y)
            _ ->
              if intend' > intend  then sep2 ents intend
                                             (lower ++ [ent]) 0
              else do
                     [(_, y)] <- getCoord emb
                     return (lower, ent : ents, intend,
                             y - Distance lineheight)
        sep2 _ intend lower y =
          return (lower, [], intend, y)

---
-- Adds a tree list objects on toplevel.
addTreeListRootObject :: CItem a => TreeList a -> TreeListObject a ->
                                    IO ()
addTreeListRootObject tl obj@(TreeListObject (val, objtype)) =
  synchronize tl
    (do
       nm <- getName val
       tlobj@(TREELISTOBJECT _ _ _ _ _ img _ _ _) <-
         mkTreeListObject tl val (objtype == Node) False [name nm]
       pho <- getIcon val
       img # photo pho
       objs <- getRef (internal_state tl)
       setRef (internal_state tl) (objs ++ [StateEntry tlobj False 0 []])
       packTreeListObject tlobj (length objs == 0)
                          (5, 5 + Distance (length objs * lineheight)))

startObjectInteractor ::  CItem a => TREELISTOBJECT a -> IO ()
startObjectInteractor obj@(TREELISTOBJECT _ _ _ plusminus _ _ _ _
                                          _) =
  do
    (press, ub) <- bindSimple plusminus (ButtonPress (Just (BNo 1)))
    addUnbindAction obj ub
    death <- newChannel
    let listenObject :: Event ()
        listenObject =    (press >> always (pressed obj) >> listenObject)
                       +> (receive death)
    spawnEvent listenObject
    addUnbindAction obj (syncNoWait (send death ()))
    done

addUnbindAction :: CItem a => TREELISTOBJECT a -> IO () -> IO ()
addUnbindAction (TREELISTOBJECT _ _ _ _ _ _ _ _ ubref) ub =
  do
    ubs <- getRef ubref
    setRef ubref (ub : ubs)

vLineLength :: CItem c => TREELISTOBJECT c -> IO Distance
vLineLength obj@(TREELISTOBJECT _ tl _ _ _ _ _ _ _) =
  do
    state <- getRef (internal_state tl)
    return(start obj (reverse state))
  where start :: TREELISTOBJECT a -> [StateEntry a] ->
                 Distance
        start obj (StateEntry obj' _ intend _ : sentries) =
          if obj' == obj then inner intend 0 sentries
          else start obj sentries
        inner :: Int -> Int -> [StateEntry a] ->  Distance
        inner intend n (StateEntry (TREELISTOBJECT _ _ isnode _ _ _ _ _ _)
                                  _ intend' _ : sentries) =
          if intend' <= intend then
            (Distance (n * lineheight) +
             Distance (if isnode then lineheight - 13
                       else lineheight - 9))
          else inner intend (n + 1) sentries
        inner _ _ _ = Distance (lineheight - 13)

-- packs an (internal) tree list object
packTreeListObject :: CItem a => TREELISTOBJECT a -> Bool -> Position ->
                                 IO ()
packTreeListObject obj@(TREELISTOBJECT _ _ isnode plusminus drawnstuff
                                       _ _ emb _)
                   isroot pos@(x, y) =
  let hline = (selHLine drawnstuff)
      vline = (selVLine drawnstuff)
  in do
       emb # coord [(x + 15, y)]
       dist <- vLineLength obj
       (if isnode then
          do
            plusminus # position (x, y + 5)
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
       if isnode then startObjectInteractor obj else done


-- -----------------------------------------------------------------------
-- TreeList instances
-- -----------------------------------------------------------------------

instance CItem c => GUIObject (TreeList c) where
  toGUIObject tl = toGUIObject (scrollbox tl)
  cname _ = "TreeList"

instance CItem c => Destroyable (TreeList c) where
  destroy = destroy . toGUIObject

instance CItem c => Widget (TreeList c)

instance CItem c => Synchronized (TreeList c) where
  synchronize = synchronize . toGUIObject

instance CItem c => HasBorder (TreeList c)

instance CItem c => HasColour (TreeList c) where
  legalColourID tl = hasBackGroundColour (cnv tl)
  setColour tl cid col = setColour (cnv tl) cid col >> return tl
  getColour tl cid = getColour (cnv tl) cid

instance CItem c => HasSize (TreeList c) where
  width s tl = (cnv tl) # width s >> return tl
  getWidth tl = getWidth (cnv tl)
  height s tl = (cnv tl) # height s >> return tl
  getHeight tl = getHeight (cnv tl)


-- -----------------------------------------------------------------------
-- TreeList events
-- -----------------------------------------------------------------------

{-
selectionEvent :: TreeList a -> Channel (Maybe (TreeListObject a))
selectionEvent (TreeList _ _ _ _ _ msgQ _) = msgQ

focusEvent :: TreeList a -> Channel (Maybe (TreeListObject a), EventInfo)
focusEvent (TreeList _ _ _ _ _ _ msgQ) = msgQ
-}


-- ***********************************************************************
-- tree list objects
-- ***********************************************************************

-- -----------------------------------------------------------------------
-- basic types
-- -----------------------------------------------------------------------

data TreeListObjectType = Node | Leaf deriving Eq

-- (value, node/leaf)
newtype TreeListObject a =
  TreeListObject (a, TreeListObjectType)

data CItem a => TREELISTOBJECT a =        -- ** internal representation **
  TREELISTOBJECT a                                                -- value
                 (TreeList a)                                    -- parent
                 Bool                                      -- true if node
                 ImageItem                                    -- plusminus
                 (Line, Line)                           -- lines if pretty
                 (Label Image)                             -- object image
                 (Label String)                             -- object name
                 EmbeddedCanvasWin                           -- main frame
                 (Ref [IO ()])                           -- unbind actions


-- -----------------------------------------------------------------------
-- construction of tree list objects
-- -----------------------------------------------------------------------

---
-- Constructs a new tree list object.
newTreeListObject :: CItem a => a -> TreeListObjectType ->
                                     TreeListObject a
newTreeListObject val objtype = TreeListObject (val, objtype)


-- -----------------------------------------------------------------------
-- exported functionality on tree list objects
-- -----------------------------------------------------------------------

---
-- Selector for the value of a tree list object.
getTreeListObjectValue :: TreeListObject a -> a
getTreeListObjectValue (TreeListObject (val, _)) = val

{-
---
-- Selector for the the name of a tree list object.
getTreeListObjectName :: TreeListObject a -> String
getTreeListObjectName (TreeListObject (_, nm, _)) = nm
-}

---
-- Selector for the type of a tree list object (node or leaf).
getTreeListObjectType :: TreeListObject a -> TreeListObjectType
getTreeListObjectType (TreeListObject (_, objtype)) = objtype

---
-- True, if the object with the given id is currently opened in the tree
-- list.
isTreeListObjectOpen :: CItem c => TreeList c -> c -> IO Bool
isTreeListObjectOpen tl c =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       let msentry =
             find (\ (StateEntry (TREELISTOBJECT c' _ _ _ _ _ _ _ _)
                                 _ _ _) -> c == c') state
       case msentry of
         Just (StateEntry _ b _ _) -> return b
         Nothing -> return False)

---
-- (Re-)sets the image of a tree list object.
setImage :: CItem a => TreeList a -> a -> Image -> IO ()
setImage tl val img =
  do
    state <- getRef (internal_state tl)
    setImage' state val img
  where setImage' :: CItem a => [StateEntry a] -> a -> Image -> IO ()
        setImage' ((StateEntry (TREELISTOBJECT val _ _ _ _ imglab _ _ _)
                               _ _ _) : ents) val' img =
          if val == val' then imglab # photo img >> done
          else setImage' ents val' img
        setImage' _ _ _ = done

---
-- (Re-)sets the name of a tree list object.
setTreeListObjectName :: CItem a => TreeList a -> a -> Name -> IO ()
setTreeListObjectName tl val txt =
  do
    state <- getRef (internal_state tl)
    setName state val
  where setName :: CItem a => [StateEntry a] -> a -> IO ()
        setName ((StateEntry (TREELISTOBJECT val _ _ _ _ _ namelab _ _)
                               _ _ _) : ents) val' =
          if val == val' then do
                                nm <- getName val
                                namelab # text (full nm) >> done
          else setName ents val'
        setName _ _ = done


-- -----------------------------------------------------------------------
-- internal functionality on tree list objects
-- -----------------------------------------------------------------------

-- shifts a displayed object by dy pixels (vertical)
shiftObject :: CItem c => Int -> StateEntry c -> IO ()
shiftObject dy (StateEntry obj@(TREELISTOBJECT _ _ isnode plusminus
                                                     drawnstuff _ _ emb _)
                                 _ _ _) =
  do
    (if isnode then moveItem plusminus 0 (Distance dy) >> done
     else done)
    moveItem (selHLine drawnstuff) 0 (Distance dy)
    moveItem (selVLine drawnstuff) 0 (Distance dy)
    coords <- getCoord (selVLine drawnstuff)
    hlinelength <- vLineLength obj
    (let (x, y) = selLower coords
     in (selVLine drawnstuff) #
          coord [(x, y), (x, y - hlinelength -
                             if isnode then 5 else 9)])
    moveItem emb 0 (Distance dy)

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
                         ((StateEntry obj@(TREELISTOBJECT _ _ _ _ _ _ _
                                                          emb _) _ _ _) :
                          sentries) =
          do
            [(cx, cy)] <- getCoord emb
            nm <- getTreeListObjectName obj
            updScrollRegion' cnv
                             (max x (cx + Distance
                                            (cwidth * length (full nm))))
                             (max y (cy + Distance lineheight)) sentries
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
        insertObjects' cnv (x, y)
                       ((i, b, obj@(TREELISTOBJECT val _ isnode _
                                                   drawnstuff img _ emb
                                                   _))
                        : objs) =
          do
            pho <- getIcon val
            img # photo pho
            packTreeListObject obj False
                               (5 + Distance (i * intendation), y)
            insertObjects' cnv (x, y + Distance lineheight) objs
        insertObjects' _ (x, y) _ = done

-- removes an object from the treelist
removeObject :: TreeList a -> TREELISTOBJECT a -> IO ()
removeObject _ (TREELISTOBJECT _ _ isnode plusminus drawnstuff _ _ emb
                               ubref) =
  do
    destroy emb
    if isnode then destroy plusminus else done
    destroy (selHLine drawnstuff)
    destroy (selVLine drawnstuff)
    ubs <- getRef ubref
    mapM id ubs
    setRef ubref []
    done

-- gets information about a tree list object
-- (intendation, object open or not, ids of previously open subobjects)
getObjInfo :: TREELISTOBJECT a -> [StateEntry a] -> IO (Int, Bool, [a])
getObjInfo obj (StateEntry obj' isopen i prevopen : entries) =
  if obj == obj' then return (i, isopen, prevopen)
  else getObjInfo obj entries

-- constructs a state entry
mkEntry :: CItem a => (Int, Bool, TREELISTOBJECT a) -> StateEntry a
mkEntry (i, b, obj) = StateEntry obj b i []

-- gets the displayed children of a tree list object
getChildren :: [StateEntry a] -> TREELISTOBJECT a ->
               IO ([TREELISTOBJECT a], [a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: [StateEntry a] -> TREELISTOBJECT a -> Int ->
                        [TREELISTOBJECT a] -> [a] ->
                        IO ([TREELISTOBJECT a], [a])
        getChildren' (st@(StateEntry obj'@(TREELISTOBJECT val _ _ _ _ _ _ 
                                                          _ _)
                                     isopen intend _) : es)
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
                               (val : opensubobjvals)
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
reopenSubObjects c_fun prevopen
                 ((i, tlobj@(TREELISTOBJECT val tl isnode plusminus _ _
                                             _ _ _)) :
                  objs) =
  if elem val prevopen then
    do
      minus <- minusImg
      plusminus # photo minus
      ch <- c_fun (TreeListObject (val, if isnode then Node else Leaf))
      thisobjch <- mkTreeListObjects tl ch (i + 1) prevopen
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
pressed obj@(TREELISTOBJECT val tl isnode plusminus drawnstuff _ _ emb
                            _) =
  synchronize tl
    (do
       state <- getRef (internal_state tl)
       c <- getCoord emb
       index <-
         return
           ((fromJust
               (elemIndex obj (map (\ (StateEntry obj _ _ _) -> obj)
                          state))) + 1)
       (i, isopen, prevopen) <- getObjInfo obj state
       (if isopen then
          do                                              -- *** close ***
            pho <- plusImg
            plusminus # photo pho
            (children, opensubobjvals) <- getChildren state obj
            mapM (removeObject tl) children
            setRef (internal_state tl)
                   (take (index - 1) state ++
                    [StateEntry obj False i opensubobjvals] ++
                    drop (index + length children) state)
            mapM (shiftObject (-(length children) * lineheight))
                 (drop (index + length children) state)
            done
        else
          do                                               -- *** open ***
            pho <- minusImg
            plusminus # photo pho
            ch <- (cfun tl) (TreeListObject (val, if isnode then Node
                                                  else Leaf))
            thisobjch <- mkTreeListObjects tl ch (i + 1) prevopen
            chobjs <- reopenSubObjects (cfun tl) prevopen thisobjch
            setRef (internal_state tl)
                   (take (index - 1) state ++
                    [StateEntry obj True i []] ++
                    map mkEntry chobjs ++ drop index state)
            mapM (shiftObject ((length chobjs) * lineheight))
                 (drop index state)
            insertObjects tl (head c) chobjs
            done)
       updScrollRegion (cnv tl) (internal_state tl))

-- selects objects and send the concerned event
selectObject :: CItem c => TreeList c -> TREELISTOBJECT c -> IO ()
selectObject tl obj@(TREELISTOBJECT val _ isnode _ _ _ txt _ _) =
  do
    unmarkSelectedObject tl
    setRef (selected_object tl) (Just obj)
    txt # fg "white"
    txt # bg "blue"
    sendEv tl (Selected (Just (TreeListObject (val, if isnode then Node
                                                    else Leaf))))
{-
    syncNoWait (send (selection_ch tl)
                  (Just (TreeListObject (val, if isnode then Node
                                              else Leaf))))
-}
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
      Just (TREELISTOBJECT _ _ _ _ _ _ txt _ _) -> do
                                                     txt # fg "black"
                                                     txt # bg "white"
                                                     done
      _ -> done

-- True for a selected object
isSelected :: CItem c => TreeList c -> TREELISTOBJECT c -> IO Bool
isSelected tl obj =
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
        hline <- createLine (cnv tl) [coord [(-50, -50), (-50, -50)]]
        vline <- createLine (cnv tl) [coord [(-50, -50), (-50, -50)]]
        return (hline, vline)
    pho <- if isopen then minusImg else plusImg
    plusminus <- createImageItem (cnv tl) [coord [(-50, -50)], photo pho,
                                           canvAnchor NorthWest]
    img <- newLabel box [background "white"]
    pack img [Side AtLeft]
    txt <- newLabel box [background "white", font (Lucida, 12::Int)]
    pack txt [Side AtRight]
    emb <- createEmbeddedCanvasWin (cnv tl) box [canvAnchor NorthWest,
                                                 coord [(-50, -50)]]
    unbind_actions <- newRef []
    let obj = TREELISTOBJECT val tl isnode plusminus drawnstuff img txt
                             emb unbind_actions
    foldl (>>=) (return obj) cnf
    (enterTxt, ub) <- bind txt [WishEvent [] Enter]
    addUnbindAction obj ub
    death <- newChannel
    addUnbindAction obj (syncNoWait (send death ()))
    (leaveTxt, ub) <- bind txt [WishEvent [] Leave]
    addUnbindAction obj ub
    (pressTxt, ub) <- bindSimple txt (ButtonPress (Just (BNo 1)))
    addUnbindAction obj ub
    let listenObject :: Event ()
        listenObject =
             (pressTxt >> always (selectObject tl obj) >> listenObject)
          +> (do
                ev_inf <- leaveTxt
                always (do
                          b <- TreeList.isSelected tl obj
                          if b then done else txt # bg "white" >>
                                              txt # fg "black" >> done
                          sendEv tl (Focused (Nothing, ev_inf)))
--                noWait (send (focus_ch tl) (Nothing, ev_inf))
                listenObject)
          +> (do
                ev_inf <- enterTxt
                always (do
                          b <- TreeList.isSelected tl obj
                          if b then done else txt # bg "grey" >>
                                              txt # fg "white" >> done
                          sendEv tl (Focused
                                       (Just (TreeListObject
                                                (val,
                                                 if isnode then Node
                                                 else Leaf)), ev_inf)))
{-
                noWait (send (focus_ch tl)
                             (Just (TreeListObject
                                      (val, if isnode then Node
                                            else Leaf)), ev_inf))
-}
                listenObject)
          +> receive death
    spawnEvent listenObject
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

instance Eq (TREELISTOBJECT a) where
  (TREELISTOBJECT _ _ _ _ _ _ txt1 _ _) ==
    (TREELISTOBJECT _ _ _ _ _ _ txt2 _ _) = txt1 == txt2

instance GUIObject (TREELISTOBJECT a) where
  toGUIObject (TREELISTOBJECT _ _ _ _ _ _ _ emb _) = toGUIObject emb
  cname _ = "TREELISTOBJECT"

instance HasPhoto (TREELISTOBJECT a) where
  photo i obj@(TREELISTOBJECT _ _ _ _ _ img _ _ _) =
    img # photo i >> return obj
  getPhoto (TREELISTOBJECT _ _ _ _ _ img _ _ _) = getPhoto img

name :: Name -> Config (TREELISTOBJECT a)
name nm obj@(TREELISTOBJECT _ _ _ _ _ _ txt _ _) =
  txt # text (full nm) >> return obj

getTreeListObjectName :: CItem a => TREELISTOBJECT a -> IO Name
getTreeListObjectName (TREELISTOBJECT val _ _ _ _ _ txt _ _) =
  do
    nm <- getName val
    return nm


-- -----------------------------------------------------------------------
-- state import / export
-- -----------------------------------------------------------------------

importTreeListState :: CItem a => TreeList a -> TreeListState a -> IO ()
importTreeListState tl st =
  do
    putStrLn "clearing treelist"
    clearTreeList tl
    putStrLn "making entries"
    state <- mkEntries tl st
    setRef (internal_state tl) state
    let StateEntry root _ _ _ = head state
    putStrLn "packing root"
    packTreeListObject root True (5, 5)
    putStrLn "inserting objects"
    insertObjects tl (5, 5 + Distance lineheight) (toObjects (tail state))

toObjects :: [StateEntry a] -> [(Int, Bool, TREELISTOBJECT a)]
toObjects (StateEntry obj isopen intend _  : ents) =
  (intend, isopen, obj) : toObjects ents
toObjects _ = []

mkEntries :: CItem a => TreeList a -> TreeListState a -> IO [StateEntry a]
mkEntries tl (i : is) =
  do
    nm <- getName (val i)
    obj <- mkTreeListObject tl (val i)
             (if obj_type i == Node then True else False) (open i)
             [name nm]
    rest <- mkEntries tl is
    return (StateEntry obj (open i) (intend i) [] : rest)
mkEntries _ _ = return []

data TreeListExportItem a =
  TreeListExportItem { val :: a,
                       obj_type :: TreeListObjectType,
                       open :: Bool,                    -- ignored if leaf
                       intend :: Int,
                       selected :: Bool }        -- yet ignored, multiple
                                                 -- selections to come ...

type TreeListState a = [TreeListExportItem a]

exportTreeListState :: CItem c => TreeList c -> IO (TreeListState c)
exportTreeListState tl =
  do
    state <- getRef (internal_state tl)
    exportTreeListState' tl state
  where exportTreeListState' :: CItem c =>
                                TreeList c -> [StateEntry c] ->
                                IO (TreeListState c)
        exportTreeListState' tl
          (StateEntry obj@(TREELISTOBJECT id _ isnode _ _ img nm _ _)
                      open intendation _ : ents) =
          do
            sel <- TreeList.isSelected tl obj
            rest <- exportTreeListState' tl ents
            return (TreeListExportItem
                      { val = id,
                        obj_type = if isnode then Node else Leaf,
                        open = open,
                        intend = intendation,
                        selected = sel} : rest)
        exportTreeListState' _ _ = return []


-- -----------------------------------------------------------------------
-- images
-- -----------------------------------------------------------------------

plusImg = newImage NONE [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACFJSPiTHdYYIcEopKZax1s35NINcV
ADs="]

minusImg = newImage NONE [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACEZSPiTHdYYKcUNAZb9Vb5ysUADs="]
