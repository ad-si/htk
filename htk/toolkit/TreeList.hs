{- --------------------------------------------------------------------
 -
 - Module TreeList
 -
 - HTk tree lists
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}

module TreeList (

  newTreeList,
  newTreeListObject,

  updateTreeList,
  addTreeListObject,

  TreeList,
  TreeListObject,
  Style(..),
  TreeListObjectType(..),
  TreeListObjectName,

  isLeaf,
  isNode,
  mkLeaf,
  mkNode,

  getObjectValue,
  getTreeListObjectName,
  getObjectType,

  ChildrenFun,
  ImageFun,

  selectionEvent,
  focusEvent,

  setImage,
  setName

) where

import HTk
import ScrollBox
import List
import ReferenceVariables
import Core
import Maybe

tldebug = True
debugMsg str = if tldebug then putStr(">>> " ++ str ++ "\n\n") else done

debugPrintState ((StateEntry obj open intend _) : ents) =
  if tldebug then
    do
      objname <- getName obj
      putStrLn (objname ++ "   Open: " ++ show open ++
                "   Intendation: " ++ show intend)
      debugPrintState ents
  else done
debugPrintState [] = putStr "\n\n"

intendation = 19
lineheight = 20
cwidth = 15


-- tree lists --

data Eq a => StateEntry a =
  StateEntry (TREELISTOBJECT a)                                  -- object
             Bool                            -- open: True / closed: False
             Int                                            -- intendation
             [a]           -- ids of previously open subobjects for reopen

data Style = Fast | Pretty deriving Eq

type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]
type ImageFun a = TreeListObject a -> IO Image

data Eq a => TreeList a =
  TreeList Canvas
           (ScrollBox Canvas)
           Style                                         -- treelist style
           (Ref [StateEntry a])                          -- treelist state
           (ChildrenFun a)                       -- node children function
           (ImageFun a)                           -- object image function
           (Ref (Maybe (TREELISTOBJECT a)))             -- selected object
           (Channel (Maybe (TreeListObject a)))      -- selection notifier
           (Channel (Maybe (TreeListObject a)))          -- focus notifier

newTreeList :: (Container par, Eq a) =>
               par -> Style -> ChildrenFun a -> ImageFun a ->
               TreeListObject a -> [Config (TreeList a)] ->
               IO (TreeList a)
newTreeList par style cfun ifun rt@(TreeListObject (val, nm, objtype))
            cnf =
  do
    (scr, cnv) <- newScrollBox par (\p -> newCanvas p []) []
    stateref <- newRef []
    selref <- newRef Nothing
    selectionMsgQ <- newChannel
    focusMsgQ <- newChannel
    let treelist = TreeList cnv scr style stateref cfun ifun selref
                            selectionMsgQ focusMsgQ
    foldl (>>=) (return treelist) cnf
    root@(TREELISTOBJECT _ _ _ _ _ drawnstuff img _ emb _) <-
      mkTreeListObject treelist val
                       (if objtype == Node then True else False)
                       False [name nm]
    setRef stateref [StateEntry root False 0 []]
    pho <- ifun rt
    img # photo pho
    packTreeListObject root True (5, 5)
    updScrollRegion cnv stateref
    (press, _) <- bindSimple cnv (ButtonPress (Just (BNo 1)))
    spawnEvent (forever (press >> always (deselect treelist)))
    pressed root
    return treelist

getObjectFromTreeList :: Eq a => TreeList a -> a ->
                                 IO (Maybe (TREELISTOBJECT a))
getObjectFromTreeList (TreeList _ _ _ stateref _ _ _ _ _) val =
  do
    state <- getRef stateref
    let msentry = find (entryEqualsObject val) state
    case msentry of
      Just (StateEntry obj _ _ _) -> return (Just obj)
      _ -> return Nothing
  where entryEqualsObject :: Eq a => a -> StateEntry a -> Bool
        entryEqualsObject val (StateEntry (TREELISTOBJECT val' _ _ _ _ _ _
                                                          _ _ _) _ _ _) =
          val == val'

isNode :: Eq a => TreeList a -> a -> IO (Maybe Bool)
isNode tl val =
  do
    mobj <- getObjectFromTreeList tl val
    return (case mobj of
              Just (TREELISTOBJECT _ _ isnode _ _ _ _ _ _ _) ->
                Just isnode
              _ -> Nothing)

isLeaf :: Eq a => TreeList a -> a -> IO (Maybe Bool)
isLeaf tl val =
  do
    mnode <- isNode tl val
    case mnode of
      Just b -> return (Just (not b))
      _ -> return Nothing

mkNode :: Eq a => TreeList a -> a -> IO ()
mkNode tl@(TreeList _ _ style _ _ ifun _ _ _) val =
  do
    mleaf <- isLeaf tl val
    case mleaf of
      Just True ->
        do
          Just obj@(TREELISTOBJECT _ _ _ _ _ _ _ _ emb _) <-
            getObjectFromTreeList tl val
          nm <- getName obj
          [(x, y)] <- getCoord emb
          removeObject tl obj
          nuobj@(TREELISTOBJECT _ _ _ _ _ _ img _ _ _) <-
            mkTreeListObject tl val True False [name nm]
          objectChanged tl obj nuobj
          pho <- ifun (TreeListObject (val, nm, Node))
          img # photo pho
          packTreeListObject nuobj False (if style == Pretty then x - 15
                                          else x , y)
      _ -> done

mkLeaf :: Eq a => TreeList a -> a -> IO ()
mkLeaf tl@(TreeList _ _ style _ _ ifun _ _ _) val =
  do
    mnode <- isNode tl val
    case mnode of
      Just True ->
        do
          Just obj@(TREELISTOBJECT _ _ _ _ _ _ _ _ emb _) <-
            getObjectFromTreeList tl val
          Just (ch, _) <- getChildrenAndUpper tl val
          (if null ch then done
           else error "TreeList (mkLeaf) : node is not empty")
          nm <- getName obj
          [(x, y)] <- getCoord emb
          removeObject tl obj
          nuobj@(TREELISTOBJECT _ _ _ _ _ _ img _ _ _) <-
            mkTreeListObject tl val False False [name nm]
          objectChanged tl obj nuobj
          pho <- ifun (TreeListObject (val, nm, Node))
          img # photo pho
          packTreeListObject nuobj False (if style == Pretty then x - 15
                                          else x , y)
      _ -> done

removeTreeListObject :: Eq a => TreeList a -> a -> IO ()
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

getChildrenAndUpper :: Eq a => TreeList a -> a ->
                               IO (Maybe ([TREELISTOBJECT a],
                                          [TREELISTOBJECT a]))
getChildrenAndUpper (TreeList _ _ _ stateref _ _ _ _ _) val =
  let getChildrenAndUpper' :: Eq a => a -> [StateEntry a] ->
                                      Maybe ([TREELISTOBJECT a],
                                             [TREELISTOBJECT a])
      getChildrenAndUpper' val ((StateEntry (TREELISTOBJECT val' _ _ _ _ _
                                                            _ _ _ _)
                                            _ intend _) : ents) =
        if val' == val then Just (getChildrenAndUpper'' ents intend [])
        else getChildrenAndUpper' val ents
      getChildrenAndUpper' _ _ = Nothing

      getChildrenAndUpper'' :: Eq a => [StateEntry a] -> Int ->
                                       [TREELISTOBJECT a] ->
                                       ([TREELISTOBJECT a],
                                        [TREELISTOBJECT a])
      getChildrenAndUpper'' ((StateEntry obj _ intend' _) : ents) intend
                            ch =
        if intend' > intend then getChildrenAndUpper'' ents intend
                                                       (ch ++ [obj])
        else (ch, map (\ (StateEntry obj _ _ _) -> obj) ents)
  in do
       state <- getRef stateref
       return (getChildrenAndUpper' val state)

objectChanged :: Eq a => TreeList a -> TREELISTOBJECT a ->
                         TREELISTOBJECT a -> IO ()
objectChanged (TreeList _ _ _ stateref _ _ _ _ _) obj nuobj =
  let objectChanged' (ent@(StateEntry obj' isopen intend sub) : ents) =
        if obj == obj' then
          StateEntry nuobj isopen intend sub : objectChanged' ents
        else ent : objectChanged' ents
      objectChanged' _ = []
  in do
       state <- getRef stateref
       setRef stateref (objectChanged' state)

updateTreeList :: Eq a => TreeList a -> IO ()
updateTreeList tl@(TreeList _ _ _ stateref _ _ _ _ _) =
  synchronize tl
    (do
       state <- getRef stateref
       let (StateEntry root isopen _ _) = (head state)
       if isopen then pressed root >> pressed root else done)

addTreeListObject :: Eq a => TreeList a -> a ->
                             TreeListObject a -> IO ()
addTreeListObject tl@(TreeList cnv _ style stateref _ ifun _ _ _) parval
                  obj@(TreeListObject (val, nm, objtype)) =
  synchronize tl
    (do
       state <- getRef stateref
       (if visibleAndOpen state parval then
          do
            (lowerobj, upperobj, parintend, y) <- sep state parval
            intobj@(TREELISTOBJECT _ _ _ _ _ drawnstuff img _ emb _) <-
              mkTreeListObject tl val (if objtype == Node then True
                                       else False) False [name nm]
            setRef stateref (lowerobj ++
                             [StateEntry intobj False (parintend + 1)
                                         []] ++
                             upperobj)
            mapM (shiftObject style lineheight) upperobj
            pho <- ifun obj
            img # photo pho
            let (StateEntry obj _ _ _) = last lowerobj
            packTreeListObject intobj False
                               (5 + Distance ((parintend + 1) *
                                              intendation),
                                y + Distance lineheight)
            updScrollRegion cnv stateref
        else done))
  where visibleAndOpen :: Eq a => [StateEntry a] -> a -> Bool
        visibleAndOpen state parval =
          let msentry = find (\ (StateEntry (TREELISTOBJECT val _ _ _ _ _
                                                            _ _ _ _)
                                            _ _ _) ->
                                val == parval) state
          in case msentry of
               Just (StateEntry obj isopen _ _) -> isopen
               _ -> False

        sep :: Eq a => [StateEntry a] -> a ->
                       IO ([StateEntry a], [StateEntry a], Int, Distance)
        sep sentries parval = sep1 sentries parval []

        sep1 :: Eq a => [StateEntry a] -> a -> [StateEntry a] ->
                        IO ([StateEntry a], [StateEntry a], Int, Distance)
        sep1 (ent@(StateEntry (TREELISTOBJECT val _ _ _ _ _ _ _ emb _) _
                              intend _) : ents) parval lower =
          if val == parval then
            do
              [(_, y)] <- getCoord emb
              sep2 ents intend (lower ++ [ent]) y
          else sep1 ents parval (lower ++ [ent])

        sep2 :: Eq a => [StateEntry a] -> Int -> [StateEntry a] ->
                        Distance ->
                        IO ([StateEntry a], [StateEntry a], Int, Distance)
        sep2 (ent@(StateEntry (TREELISTOBJECT _ _ _ _ _ drawnstuff _ _
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

startObjectInteractor ::  Eq a => Style -> TREELISTOBJECT a -> IO ()
startObjectInteractor style obj@(TREELISTOBJECT _ _ _ arrow plusminus _ _
                                                _ _ _) =
  do
    (if style == Fast then
       do
         (press, ub) <- bindSimple (fromJust arrow)
                                   (ButtonPress (Just (BNo 1)))
         addUnbindAction obj ub
         spawnEvent (forever (press >> always(pressed obj)))
     else
       do
         (press, ub) <- bindSimple (fromJust plusminus)
                                   (ButtonPress (Just (BNo 1)))
         addUnbindAction obj ub
         spawnEvent (forever (press >> always(pressed obj))))
    done

addUnbindAction :: Eq a => TREELISTOBJECT a -> IO () -> IO ()
addUnbindAction (TREELISTOBJECT _ _ _ _ _ _ _ _ _ ubref) ub =
  do
    ubs <- getRef ubref
    setRef ubref (ub : ubs)

vLineLength :: TREELISTOBJECT a -> IO Distance
vLineLength obj@(TREELISTOBJECT _ (TreeList _ _ _ stateref _ _ _ _ _) _ _ 
                                _ _ _ _ _ _) =
  do
    state <- getRef stateref
    return(start obj (reverse state))
  where start :: TREELISTOBJECT a -> [StateEntry a] ->
                 Distance
        start obj (StateEntry obj' _ intend _ : sentries) =
          if obj' == obj then inner intend 0 sentries
          else start obj sentries
        inner :: Int -> Int -> [StateEntry a] ->  Distance
        inner intend n(StateEntry (TREELISTOBJECT _ _ isnode _ _ _ _ _ _
                                                  _)
                                  _ intend' _ : sentries) =
          if intend' <= intend then
            (Distance (n * lineheight) +
             Distance (if isnode then lineheight - 13
                       else lineheight - 9))
          else inner intend (n + 1) sentries
        inner _ _ _ = Distance (lineheight - 13)

packTreeListObject :: Eq a => TREELISTOBJECT a -> Bool -> Position ->
                              IO ()
packTreeListObject obj@(TREELISTOBJECT _ (TreeList cnv _ style _ _ _ _ _
                                                         _)
                                       isnode _ plusminus drawnstuff img _
                                       emb _)
                   isroot pos@(x, y) =
  do
    (if style == Fast then
       emb # coord [(x, y)] >> done
     else
       let hline = (selHLine drawnstuff)
           vline = (selVLine drawnstuff)
       in
         do
           emb # coord [(x + 15, y)]
           dist <- vLineLength obj
           (if isnode then
              do
                fromJust plusminus # position (x, y + 5)
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
           done)
    if isnode then startObjectInteractor style obj else done
    done

instance GUIObject (TreeList a) where
  toGUIObject (TreeList _ scr _ _ _ _ _ _ _) = toGUIObject scr
  cname _ = "TreeList"

instance Destroyable (TreeList a) where
  destroy = destroy . toGUIObject

instance Widget (TreeList a)

instance Synchronized (TreeList a) where
  synchronize = synchronize . toGUIObject

instance HasBorder (TreeList a)

instance HasColour (TreeList a) where
  legalColourID (TreeList cnv _ _ _ _ _ _ _ _) = hasBackGroundColour cnv
  setColour treelist@(TreeList cnv _ _ _ _ _ _ _ _) cid col =
    setColour cnv cid col >> return treelist
  getColour (TreeList cnv _ _ _ _ _ _ _ _) cid = getColour cnv cid

instance HasSize (TreeList a) where
  width s treelist@(TreeList cnv _ _ _ _ _ _ _ _) =
    cnv # width s >> return treelist
  getWidth (TreeList cnv _ _ _ _ _ _ _ _) = getWidth cnv
  height s treelist@(TreeList cnv _ _ _ _ _ _ _ _) =
    cnv # height s >> return treelist
  getHeight (TreeList cnv _ _ _ _ _ _ _ _) = getHeight cnv

selectionEvent :: TreeList a -> Channel (Maybe (TreeListObject a))
selectionEvent (TreeList _ _ _ _ _ _ _ msgQ _) = msgQ

focusEvent :: TreeList a -> Channel (Maybe (TreeListObject a))
focusEvent (TreeList _ _ _ _ _ _ _ _ msgQ) = msgQ

--selectionEvent :: TreeList a -> IA (Maybe (TreeListObject a))
--selectionEvent (TreeList _ _ _ _ _ _ _ msgQ _) = lift (receive msgQ)

--focusEvent :: TreeList a -> IA (Maybe (TreeListObject a))
--focusEvent (TreeList _ _ _ _ _ _ _ _ msgQ) = lift (receive msgQ)


-- tree list objects --

data TreeListObjectType = Node | Leaf deriving Eq

type TreeListObjectName = String

newtype TreeListObject a = TreeListObject (a, TreeListObjectName, TreeListObjectType)

getObjectValue :: TreeListObject a -> a
getObjectValue (TreeListObject (val, _, _)) = val

getTreeListObjectName :: TreeListObject a -> TreeListObjectName
getTreeListObjectName (TreeListObject (_, nm, _)) = nm

getObjectType :: TreeListObject a -> TreeListObjectType
getObjectType (TreeListObject (_, _, objtype)) = objtype

newTreeListObject :: Eq a => a -> String -> TreeListObjectType ->
                             TreeListObject a
newTreeListObject val nm objtype = TreeListObject (val, nm, objtype)

setImage :: Eq a => TreeList a -> a -> Image -> IO ()
setImage (TreeList _ _ _ stateref _ _ _ _ _) val img =
  do
    state <- getRef stateref
    setImage' state val img
  where setImage' :: Eq a => [StateEntry a] -> a -> Image -> IO ()
        setImage' ((StateEntry (TREELISTOBJECT val _ _ _ _ _ imglab _ _ _)
                               _ _ _) : ents) val' img =
          if val == val' then imglab # photo img >> done
          else setImage' ents val' img
        setImage' _ _ _ = done

setName :: Eq a => TreeList a -> a -> String -> IO ()
setName (TreeList _ _ _ stateref _ _ _ _ _) val txt =
  do
    state <- getRef stateref
    setName' state val txt
  where setName' :: Eq a => [StateEntry a] -> a -> String -> IO ()
        setName' ((StateEntry (TREELISTOBJECT val _ _ _ _ _ _ namelab _ _)
                               _ _ _) : ents) val' txt =
          if val == val' then namelab # text txt >> done
          else setName' ents val' txt
        setName' _ _ _ = done

data Eq a => TREELISTOBJECT a =           -- ** internal representation **
  TREELISTOBJECT a                                                -- value
                 (TreeList a)                                    -- parent
                 Bool                                      -- true if node
                 (Maybe (Label Image))           -- arrow if style is fast
                 (Maybe ImageItem)                  -- plusminus if pretty
                 (Maybe (Line, Line))                   -- lines if pretty
                 (Label Image)                             -- object image
                 (Label String)                             -- object name
                 EmbeddedCanvasWin                           -- main frame
                 (Ref [IO ()])                           -- unbind actions

shiftObject :: Style -> Int -> StateEntry a -> IO ()
shiftObject style dy (StateEntry obj@(TREELISTOBJECT _ _ isnode _
                                                     plusminus drawnstuff
                                                     _ _ emb _)
                                 _ _ _) =
  (if style == Pretty then
     do
       if isnode then
         moveItem (fromJust plusminus) 0 (Distance dy) >> done else done
       moveItem (selHLine drawnstuff) 0 (Distance dy)
       moveItem (selVLine drawnstuff) 0 (Distance dy)
       coords <- getCoord (selVLine drawnstuff)
       hlinelength <- vLineLength obj
       (let (x, y) = selLower coords
        in (selVLine drawnstuff) #
             coord [(x, y), (x, y - hlinelength -
                                if isnode then 5 else 9)])
       done
   else done) >> moveItem emb 0 (Distance dy)

selLower :: Coord -> Position
selLower coords = selLower' (head coords) (tail coords)
  where selLower' :: Position -> Coord -> Position
        selLower' l@(_, yl) (c@(_, y) : cs) =
          if y > yl then selLower' c cs else selLower' l cs
        selLower' l _ = l

updScrollRegion :: Canvas -> Ref [StateEntry a] -> IO ()
updScrollRegion cnv stateref =
  do
    state <- getRef stateref
    updScrollRegion' cnv 0 0 state
  where updScrollRegion' :: Canvas -> Distance -> Distance ->
                            [StateEntry a] -> IO ()
        updScrollRegion' cnv x y
                         ((StateEntry obj@(TREELISTOBJECT _ _ _ _ _ _ _ _
                                                          emb _) _ _ _) :
                          sentries) =
          do
            [(cx, cy)] <- getCoord emb
            nm <- getName obj
            updScrollRegion' cnv
                             (max x (cx + Distance (cwidth * length nm)))
                             (max y (cy + Distance lineheight)) sentries
        updScrollRegion' cnv x y _ =
          (cnv # scrollRegion ((0, 0), (x, y))) >> done

insertObjects :: Eq a => TreeList a -> Position ->
                 [(Int, Bool, TREELISTOBJECT a)] -> IO ()
insertObjects treelist@(TreeList cnv _ style stateref _ ifun _ _ _) (x, y)
              chobjs =
  do
    state <- getRef stateref
    insertObjects' cnv ifun (x, y + Distance lineheight) chobjs
  where insertObjects' :: Eq a => Canvas -> ImageFun a -> Position ->
                                  [(Int, Bool, TREELISTOBJECT a)] -> IO ()
        insertObjects' cnv ifun (x, y)
                       ((i, _, obj@(TREELISTOBJECT val _ isnode _ _
                                                   drawnstuff img _ emb
                                                   _))
                        : objs) =
          do
            nm <- getName obj
            pho <- ifun (TreeListObject (val, nm, if isnode then Node
                                                  else Leaf))
            img # photo pho
            packTreeListObject obj False
                               (5 + Distance (i * intendation), y)
            insertObjects' cnv ifun (x, y + Distance lineheight) objs
        insertObjects' _ _ (x, y) _ = done

removeObject :: TreeList a -> TREELISTOBJECT a -> IO ()
removeObject (TreeList _ _ style _ _ _ _ _ _)
             (TREELISTOBJECT _ _ isnode _ plusminus drawnstuff _ _ emb
                             ubref) =
  destroy emb >>
  if style == Pretty then
    (if isnode then
       destroy (fromJust plusminus)
     else done) >>
    destroy (selHLine drawnstuff) >>
    destroy (selVLine drawnstuff)
  else done >>
  do
    ubs <- getRef ubref
    mapM (\act -> act) ubs >> done

getObjInfo :: TREELISTOBJECT a -> [StateEntry a] -> IO (Int, Bool, [a])
getObjInfo obj (StateEntry obj' isopen i prevopen : entries) =
  if obj == obj' then return (i, isopen, prevopen)
  else getObjInfo obj entries

mkEntry :: Eq a => (Int, Bool, TREELISTOBJECT a) -> StateEntry a
mkEntry (i, b, obj) = StateEntry obj b i []

getChildren :: [StateEntry a] -> TREELISTOBJECT a ->
               IO ([TREELISTOBJECT a], [a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: [StateEntry a] -> TREELISTOBJECT a -> Int ->
                        [TREELISTOBJECT a] -> [a] ->
                        IO ([TREELISTOBJECT a], [a])
        getChildren' (st@(StateEntry obj'@(TREELISTOBJECT val _ _ _ _ _ _ 
                                                          _ _ _)
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

reopenSubObjects :: Eq a => ChildrenFun a -> [a] ->
                            [(Int, TREELISTOBJECT a)] ->
                            IO [(Int, Bool, TREELISTOBJECT a)]
reopenSubObjects cfun prevopen
                 ((i, tlobj@(TREELISTOBJECT val tl isnode _ plusminus _ _
                                             _ _ _)) :
                  objs) =
  if elem val prevopen then
    do
      minus <- minusImg
      fromJust plusminus # photo minus
      nm <- getName tlobj
      ch <- cfun (TreeListObject (val, nm, if isnode then Node else Leaf))
      thisobjch <- mkTreeListObjects tl ch (i + 1) prevopen
      chobjs <- reopenSubObjects cfun prevopen thisobjch
      rest <- reopenSubObjects cfun prevopen objs
      return (((i, True, tlobj) : chobjs) ++ rest)
  else
    do
      rest <- reopenSubObjects cfun prevopen objs
      return ((i, False, tlobj) : rest)
reopenSubObjects _ _ _ = return []

pressed :: Eq a => TREELISTOBJECT a -> IO ()
pressed obj@(TREELISTOBJECT val (treelist@(TreeList cnv _ style stateref
                                                    cfun _ _ _ _))
                            isnode arrow plusminus drawnstuff _ _ emb _) =
  do
    state <- getRef stateref
    c <- getCoord emb
    index <-
      return
        ((fromJust
            (elemIndex obj (map (\ (StateEntry obj _ _ _) -> obj)
                       state))) + 1)
    (i, isopen, prevopen) <- getObjInfo obj state
    (if isopen then
       do                                                 -- *** close ***
         (if style == Fast then
            do
              pho <- closedImg
              fromJust arrow # photo pho >> done
          else
            do
              pho <- plusImg
              fromJust plusminus # photo pho >> done)
         (children, opensubobjvals) <- getChildren state obj
         mapM (removeObject treelist) children
         setRef stateref (take (index - 1) state ++
                          [StateEntry obj False i opensubobjvals] ++
                          drop (index + length children) state)
         mapM (shiftObject style (-(length children) * lineheight))
              (drop (index + length children) state)
         done
     else
       do                                                  -- *** open ***
         (if style == Fast then
            do
              pho <- openImg
              fromJust arrow # photo pho
              done
          else
            do
              pho <- minusImg
              fromJust plusminus # photo pho >> done)
         nm <- getName obj
         ch <- cfun (TreeListObject (val, nm, if isnode then Node
                                              else Leaf))
         thisobjch <- mkTreeListObjects treelist ch (i + 1) prevopen
         chobjs <- reopenSubObjects cfun prevopen thisobjch
         setRef stateref (take (index - 1) state ++
                          [StateEntry obj True i []] ++
                          map mkEntry chobjs ++
                          drop index state)
         mapM (shiftObject style ((length chobjs) * lineheight))
              (drop index state)
         insertObjects treelist (head c) chobjs
         done)
    updScrollRegion cnv stateref

unmarkSelectedObject :: TreeList a -> IO ()
unmarkSelectedObject (TreeList _ _ _ _ _ _ selref _ _) =
  do
    sel <- getRef selref
    case sel of
      Just (TREELISTOBJECT _ _ _ _ _ _ _ txt _ _) -> do
                                                       txt # fg "black"
                                                       txt # bg "white"
                                                       done
      _ -> done

selectObject :: TreeList a -> TREELISTOBJECT a -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ _ selref msgQ _)
             obj@(TREELISTOBJECT val _ isnode _ _ _ _ txt _ _) =
  do
    unmarkSelectedObject treelist
    setRef selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    nm <- getName obj
    syncNoWait (send msgQ (Just (TreeListObject
                                   (val, nm, if isnode then Node
                                             else Leaf))))
    done

deselect :: TreeList a -> IO ()
deselect treelist@(TreeList _ _ _ _ _ _ selref msgQ _) =
  do
    unmarkSelectedObject treelist
    setRef selref Nothing
    syncNoWait (send msgQ Nothing)
    done

isSelected :: TreeList a -> TREELISTOBJECT a -> IO Bool
isSelected (TreeList _ _ _ _ _ _ selref _ _) obj =
  do
    sel <- getRef selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

mkTreeListObjects :: Eq a => TreeList a -> [TreeListObject a] -> Int ->
                             [a] -> IO [(Int, TREELISTOBJECT a)]
mkTreeListObjects tl objs i prevopen =
  mapM (mk tl i prevopen) objs
  where mk :: Eq a => TreeList a -> Int -> [a] -> TreeListObject a ->
                      IO (Int, TREELISTOBJECT a)
        mk tl i prevopen (TreeListObject (val, nm, objtype)) =
          do
            obj <- mkTreeListObject tl val
                     (if objtype == Node then True else False)
                     (elem val prevopen) [name nm]
            return (i, obj)

mkTreeListObject :: Eq a => TreeList a -> a -> Bool -> Bool ->
                            [Config (TREELISTOBJECT a)] ->
                            IO (TREELISTOBJECT a)
mkTreeListObject treelist@(TreeList cnv _ style _ cfun _ _ _ msgQ) val
                 isnode isopen cnf =
  do
    box <- newHBox cnv [background "white"]
    arrow <- if style == Fast then
               do
                 pho <- if not isnode then leafImg
                        else if isopen then openImg else closedImg
                 arrow' <- newLabel box [background "white", photo pho]
                 pack arrow' [Side AtLeft]
                 return (Just arrow')
             else return Nothing
    drawnstuff <-
      if style == Fast then return Nothing
      else
        do
          hline <- createLine cnv [coord [(-50, -50), (-50, -50)]]
          vline <- createLine cnv [coord [(-50, -50), (-50, -50)]]
          return (Just (hline, vline))
    pho <- if isopen then minusImg else plusImg
    plusminus <- if style == Fast then return Nothing
                 else do
                        plus <- plusImg
                        i <- createImageItem cnv [coord [(-50, -50)],
                                                  photo plus,
                                                  canvAnchor NorthWest]
                        return (Just i)
    img <- newLabel box [background "white"]
    pack img [Side AtLeft]
    txt <- newLabel box [background "white", font (Lucida, 12::Int)]
    pack txt [Side AtRight]
    emb <- createEmbeddedCanvasWin cnv box [canvAnchor NorthWest,
                                            coord [(-50, -50)]]
    unbind_actions <- newRef []
    let obj = TREELISTOBJECT val treelist isnode arrow plusminus
                             drawnstuff img txt emb unbind_actions
    foldl (>>=) (return obj) cnf
    nm <- getName obj
    (enterTxt, ub) <- bindSimple txt Enter
    addUnbindAction obj ub
    death1 <- newChannel
    addUnbindAction obj (syncNoWait (send death1 ()))

    let enterEv :: Event ()
        enterEv =
          do
            enterTxt
            always (do
                      b <- TreeList.isSelected treelist obj
                      if b then done else txt # bg "grey" >>
                                          txt # fg "white" >> done)
            noWait(send msgQ (Just (TreeListObject
                                      (val, nm, if isnode then Node
                                                else Leaf))))
            enterEv
    spawnEvent (enterEv +> receive death1)

    (leaveTxt, ub) <- bindSimple txt Leave
    addUnbindAction obj ub
    death2 <- newChannel
    addUnbindAction obj (syncNoWait (send death2 ()))

    let leaveEv :: Event ()
        leaveEv =
          do
            leaveTxt
            always (do
                      b <- TreeList.isSelected treelist obj
                      if b then done else txt # bg "white" >>
                                          txt # fg "black" >> done)
            noWait (send msgQ Nothing)
            leaveEv
    spawnEvent (leaveEv +> receive death2)

    (pressTxt, ub) <- bindSimple txt (ButtonPress (Just (BNo 1)))
    addUnbindAction obj ub
    death3 <- newChannel
    addUnbindAction obj (syncNoWait (send death3 ()))

    let pressEv :: Event ()
        pressEv =
          pressTxt >> always (selectObject treelist obj) >> pressEv
    spawnEvent (pressEv +> receive death3)

    return obj

selHLine :: Maybe (Line, Line) -> Line
selHLine (Just (hline, _)) = hline

selVLine :: Maybe (Line, Line) -> Line
selVLine (Just (_, vline)) = vline

instance Eq (TREELISTOBJECT a) where
  (TREELISTOBJECT _ _ _ _ _ _ _ txt1 _ _) ==
    (TREELISTOBJECT _ _ _ _ _ _ _ txt2 _ _) = txt1 == txt2

instance GUIObject (TREELISTOBJECT a) where
  toGUIObject (TREELISTOBJECT _ _ _ _ _ _ _ _ emb _) = toGUIObject emb
  cname _ = "TREELISTOBJECT"

instance HasPhoto (TREELISTOBJECT a) where
  photo i obj@(TREELISTOBJECT _ _ _ _ _ _ img _ _ _) =
    img # photo i >> return obj
  getPhoto (TREELISTOBJECT _ _ _ _ _ _ img _ _ _) = getPhoto img

name :: TreeListObjectName -> Config (TREELISTOBJECT a)
name nm obj@(TREELISTOBJECT _ _ _ _ _ _ _ txt _ _) =
  txt # text nm >> return obj

getName :: TREELISTOBJECT a -> IO String
getName (TREELISTOBJECT _ _ _ _ _ _ _ txt _ _) = getText txt

closedImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMfAwMD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
JwADCBxIsCCAgwUJHkSYMADDhg4XAmg4EaLAihYXZpRIUWNHiyADAgA7"]

openImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMfAwMD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
IgADCBxIsKDBggASKkx4sOHAhQ4jRlxIEQDBig0ZStxYMCAAOw=="]

leafImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMf///8AAAD///8AAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAI
FgABCBxIsKDBgwgTKlzIsKHDhxARBgQAOw=="]

plusImg = newImage NONE [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACFJSPiTHdYYIcEopKZax1s35NINcV
ADs="]

minusImg = newImage NONE [imgData GIF "R0lGODlhCQAJAJEAAP///9Dc4H6LjwAAACwAAAAACQAJAEACEZSPiTHdYYKcUNAZb9Vb5ysUADs="]
