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

  TreeListObject,
  Style(..),
  ObjectType(..),
  ObjectName,

  getObjectID,
  getObjectName,
  getObjectType,

  ChildrenFun,
  ImageFun,

  selectionEvent,
  focusEvent,

  setImage,
  setName

) where

import HTk
import Image
import Canvas
import EmbeddedCanvasWin
import Line
import Rectangle
import Label
import Mouse
import ScrollBox
import Concurrency
import Channels
import RVar
import List
import Maybe

debug = False	
debugMsg str = if debug then putStr(">>> " ++ str ++ "\n\n") else done

debugPrintState ((StateEntry obj open intend _) : ents) =
  if debug then
    do
      objname <- getName obj
      putStrLn (objname ++ "   Open: " ++ show open ++
                "   Intendation: " ++ show intend)
      debugPrintState ents
  else done
debugPrintState [] = putStr "\n\n"

white = "#f6f6f6"
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
           (RVar [StateEntry a])                         -- treelist state
           (ChildrenFun a)                       -- node children function
           (ImageFun a)                           -- object image function
           (RVar (Maybe (TREELISTOBJECT a)))            -- selected object
           (MsgQueue (Maybe (TreeListObject a)))     -- selection notifier
           (MsgQueue (TreeListObject a))                 -- focus notifier

newTreeList :: Eq a => Style -> ChildrenFun a -> ImageFun a ->
                       TreeListObject a -> [Config (TreeList a)] ->
                       IO (TreeList a)
newTreeList style cfun ifun rt@(TreeListObject (val, nm, objtype)) cnf =
  do
    cnv <- newCanvas []
    scr <- newScrollBox cnv []
    stateref <- newRVar []
    selref <- newRVar Nothing
    selectionMsgQ <- newMsgQueue
    focusMsgQ <- newMsgQueue
    treelist <- return(TreeList cnv scr style stateref cfun ifun selref
                                selectionMsgQ focusMsgQ)
    foldl (>>=) (return treelist) cnf
    root@(TREELISTOBJECT _ _ _ _ drawnstuff img _ emb) <-
      mkTreeListObject treelist val
                       (if objtype == Node then True else False)
                       False [name nm]
    setVar stateref [StateEntry root False 0 []]
    pho  <- ifun rt
    img # photo pho
    packTreeListObject root True (5, 5)
    updScrollRegion cnv stateref
    pressed root
    return treelist

startObjectInteractor ::  Eq a => Style -> TREELISTOBJECT a -> IO ()
startObjectInteractor style obj@(TREELISTOBJECT _ treelist _ arrow
                                                drawnstuff _ _ _) =
  do
    (if style == Fast then
       interactor (\i -> mouseButtonPress (fromJust arrow) 1 >>>
                         pressed obj)
     else
       let plusminus = selPlusMinus drawnstuff
       in interactor (\i -> (mouseButtonPress
                               (selRect drawnstuff) 1 >>>
                             pressed obj) +>
                            (mouseButtonPress (head(tail plusminus)) 1 >>>
                             pressed obj) +>
                            (mouseButtonPress (head plusminus) 1 >>>
                             pressed obj)))
    done

vLineLength :: TREELISTOBJECT a -> IO Distance
vLineLength obj@(TREELISTOBJECT _ (TreeList _ _ _ stateref _ _ _ _ _) _ _ 
                                _ _ _ _) =
  do
    state <- getVar stateref
    return(start obj (reverse state))
  where start :: TREELISTOBJECT a -> [StateEntry a] ->
                 Distance
        start obj (StateEntry obj' _ intend _ : sentries) =
          if obj' == obj then inner intend 0 sentries
          else start obj sentries
        inner :: Int -> Int -> [StateEntry a] ->  Distance
        inner intend n(StateEntry (TREELISTOBJECT _ _ isnode _ _ _ _ _) _
                                  intend' _ : sentries) =
          if intend' <= intend then
            (Distance (n * lineheight) +
             Distance (if isnode then lineheight - 13
                       else lineheight - 9))
          else inner intend (n + 1) sentries
        inner _ _ _ = Distance (lineheight - 13)

packTreeListObject :: Eq a => TREELISTOBJECT a -> Bool -> Position -> IO ()
packTreeListObject obj@(TREELISTOBJECT _ (TreeList cnv _ style _ _ _ _ _
                                                         _)
                                       isnode _ drawnstuff img _ emb)
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
              let rect = (selRect drawnstuff)
                  plusminus = (selPlusMinus drawnstuff)
              in do
                   rect # position (x, y + 5)
                   rect # parent cnv
                   head plusminus # coord [(x + 4, y + 7),
                                           (x + 4, y + 12)]
                   head (tail plusminus) #
                     coord [(x + 2, y + 9), (x + 7, y + 9)]
                   mapM (\l -> l # parent cnv) plusminus
                   hline # coord [(x + 9, y + 9), (x + 13, y + 9)]
                   (if not isroot then
                      vline # coord [(x + 4, y + 5),
                                     (x + 4, y - dist)] >> done
                    else done)
            else
              do
                hline # coord [(x + 4, y + 9), (x + 13, y + 9)]
                (if not isroot then
                   vline # coord [(x + 4, y + 9), (x + 4, y - dist)] >>
                   done
                 else done)
                done)
           hline # parent cnv
           vline # parent cnv
           done)
    emb # parent cnv
    if isnode then startObjectInteractor style obj else done
    done

instance GUIObject (TreeList a) where
  toGUIObject (TreeList _ scr _ _ _ _ _ _ _) = toGUIObject scr
  cname _ = "TreeList"

instance Destructible (TreeList a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Widget (TreeList a)

instance ChildWidget (TreeList a)

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

selectionEvent :: TreeList a -> IA (Maybe (TreeListObject a))
selectionEvent (TreeList _ _ _ _ _ _ _ msgQ _) = lift (receive msgQ)

focusEvent :: TreeList a -> IA (TreeListObject a)
focusEvent (TreeList _ _ _ _ _ _ _ _ msgQ) = lift (receive msgQ)


-- tree list objects --

data ObjectType = Node | Leaf deriving Eq

type ObjectName = String

newtype TreeListObject a = TreeListObject (a, ObjectName, ObjectType)

getObjectID :: TreeListObject a -> a
getObjectID (TreeListObject (val, _, _)) = val

getObjectName :: TreeListObject a -> ObjectName
getObjectName (TreeListObject (_, nm, _)) = nm

getObjectType :: TreeListObject a -> ObjectType
getObjectType (TreeListObject (_, _, objtype)) = objtype

newTreeListObject :: Eq a => a -> String -> ObjectType -> TreeListObject a
newTreeListObject val nm objtype = TreeListObject (val, nm, objtype)

setImage :: Eq a => TreeList a -> a -> Image -> IO ()
setImage (TreeList _ _ _ stateref _ _ _ _ _) val img =
  do
    state <- getVar stateref
    setImage' state val img
  where setImage' :: Eq a => [StateEntry a] -> a -> Image -> IO ()
        setImage' ((StateEntry (TREELISTOBJECT val _ _ _ _ imglab _ _) _ _
                               _) : ents) val' img =
          if val == val' then imglab # photo img >> done
          else setImage' ents val' img
        setImage' _ _ _ = done

setName :: Eq a => TreeList a -> a -> String -> IO ()
setName (TreeList _ _ _ stateref _ _ _ _ _) val txt =
  do
    state <- getVar stateref
    setName' state val txt
  where setName' :: Eq a => [StateEntry a] -> a -> String -> IO ()
        setName' ((StateEntry (TREELISTOBJECT val _ _ _ _ _ namelab _) _ _
                              _) : ents) val' txt =
          if val == val' then namelab # value txt >> done
          else setName' ents val' txt
        setName' _ _ _ = done

data Eq a => TREELISTOBJECT a =           -- ** internal representation **
  TREELISTOBJECT a                                                -- value
                 (TreeList a)                                    -- parent
                 Bool                                      -- true if node
                 (Maybe (Label Image))           -- arrow if style is fast
                 (Maybe (Maybe Rectangle, [Line],
                         Line, Line))             -- drawn stuff if pretty
                 (Label Image)                             -- object image
                 (Label String)                             -- object name
                 EmbeddedCanvasWin                           -- main frame

shiftObject :: Style -> Int -> StateEntry a -> IO ()
shiftObject style dy (StateEntry obj@(TREELISTOBJECT _ _ isnode _
                                                     drawnstuff _ _ emb)
                                 _ _ _) =
  do
    (if style == Pretty then
       do
         (if isnode then
            let plusminus = selPlusMinus drawnstuff
                rect = selRect drawnstuff
            in mapM (\it -> moveItem it 0 (Distance dy)) plusminus >>
               moveItem rect 0 (Distance dy) >> done
          else done)
         moveItem (selHLine drawnstuff) 0 (Distance dy)
         moveItem (selVLine drawnstuff) 0 (Distance dy) >> done
         coords <- getCoord (selVLine drawnstuff)
         hlinelength <- vLineLength obj
         (let (x, y) = selLower (head coords) (tail coords)
          in (selVLine drawnstuff) #
               coord [(x, y), (x, y - hlinelength -
                                  if isnode then 5 else 9)])
         done
     else done)
    moveItem emb 0 (Distance dy)
    where selLower :: Position -> Coord -> Position
          selLower l@(_, yl) (c@(_, y) : cs) =
            if y > yl then selLower c cs else selLower l cs
          selLower l _ = l

updScrollRegion :: Canvas -> RVar [StateEntry a] -> IO ()
updScrollRegion cnv stateref =
  do
    state <- getVar stateref
    updScrollRegion' cnv 0 0 state
  where updScrollRegion' :: Canvas -> Distance -> Distance ->
                            [StateEntry a] -> IO ()
        updScrollRegion' cnv x y
                         ((StateEntry obj@(TREELISTOBJECT _ _ _ _ _ _ _
                                                          emb) _ _ _) :
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
    state <- getVar stateref
    insertObjects' cnv ifun (x, y + Distance lineheight) chobjs
  where insertObjects' :: Eq a => Canvas -> ImageFun a -> Position ->
                                  [(Int, Bool, TREELISTOBJECT a)] -> IO ()
        insertObjects' cnv ifun (x, y)
                       ((i, _, obj@(TREELISTOBJECT val _ isnode _
                                                   drawnstuff img _ emb))
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

removeObjects :: TreeList a -> Int -> [TREELISTOBJECT a] -> IO ()
removeObjects treelist@(TreeList _ _ style _ _ _ _ _ _) index children =
  mapM (\ (TREELISTOBJECT _ _ isnode _ drawnstuff _ _ emb) ->
          destroy emb >>
          (if style == Pretty then
             do
               (if isnode then
                  destroy (selRect drawnstuff) >>
                  mapM destroy (selPlusMinus drawnstuff) >>
                  done
                else done)
               destroy (selHLine drawnstuff)
               destroy (selVLine drawnstuff)
               done
           else done)) children >> done

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
                                                          _)
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
                 ((i, tlobj@(TREELISTOBJECT val tl isnode _ _ _ _ _)) :
                  objs) =
  if elem val prevopen then
    do
      nm <- getName tlobj
      ch <- cfun (TreeListObject (val, nm, if isnode then Node else Leaf))
      thisobjch <- mkTreeListObjects tl ch (i + 1)
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
                            isnode arrow drawnstuff _ _ emb) =
  synchronize treelist
    (do
       state <- getVar stateref
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
                 fromJust arrow # photo pho
                 done
             else head (selPlusMinus drawnstuff) #
                    filling "black" >> done)
            (children, opensubobjvals) <- getChildren state obj
            removeObjects treelist index children
            setVar stateref (take (index - 1) state ++
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
             else head (selPlusMinus drawnstuff) #
                    filling white >> done)
            nm <- getName obj
            ch <- cfun (TreeListObject (val, nm, if isnode then Node
                                                 else Leaf))
            thisobjch <- mkTreeListObjects treelist ch (i + 1)
            chobjs <- reopenSubObjects cfun prevopen thisobjch
            setVar stateref (take (index - 1) state ++
                             [StateEntry obj True i []] ++
                             map mkEntry chobjs ++
                             drop index state)
            mapM (shiftObject style ((length chobjs) * lineheight))
                 (drop index state)
            insertObjects treelist (head c) chobjs
            done)
       updScrollRegion cnv stateref)

unmarkSelectedObject :: TreeList a -> IO ()
unmarkSelectedObject (TreeList _ _ _ _ _ _ selref _ _) =
  do
    sel <- getVar selref
    case sel of
      Just (TREELISTOBJECT _ _ _ _ _ _ txt _) -> do
                                                   txt # fg "black"
                                                   txt # bg "white"
                                                   done
      _ -> done

selectObject :: TreeList a -> TREELISTOBJECT a -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ _ selref msgQ _)
             obj@(TREELISTOBJECT val _ isnode _ _ _ txt _) =
  do
    unmarkSelectedObject treelist
    setVar selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    nm <- getName obj
    sendIO msgQ (Just (TreeListObject (val, nm, if isnode then Node
                                                else Leaf)))

deselect :: TreeList a -> IO ()
deselect treelist@(TreeList _ _ _ _ _ _ selref msgQ _) =
  do
    unmarkSelectedObject treelist
    setVar selref Nothing
    sendIO msgQ Nothing

isSelected :: TreeList a -> TREELISTOBJECT a -> IO Bool
isSelected (TreeList _ _ _ _ _ _ selref _ _) obj =
  do
    sel <- getVar selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

mkTreeListObjects :: Eq a => TreeList a -> [TreeListObject a] -> Int ->
                             IO [(Int, TREELISTOBJECT a)]
mkTreeListObjects tl objs i =
  mapM (mk tl i) objs
  where mk :: Eq a => TreeList a -> Int -> TreeListObject a ->
                      IO (Int, TREELISTOBJECT a)
        mk tl i (TreeListObject (val, nm, objtype)) =
          do
            obj <- mkTreeListObject tl val (if objtype == Node then True
                                            else False) False [name nm]
            return (i, obj)

mkTreeListObject :: Eq a => TreeList a -> a -> Bool -> Bool ->
                            [Config (TREELISTOBJECT a)] ->
                            IO (TREELISTOBJECT a)
mkTreeListObject treelist@(TreeList cnv _ style _ cfun _ _ _ msgQ) val
                 isnode isopen cnf =
  do
    box <- newHBox [background "white"]
    arrow <- if style == Fast then
               do
                 pho <- if not isnode then leafImg
                        else if isopen then openImg else closedImg
                 arrow' <- newLabel [background "white", photo pho,
                                     parent box]
                 return (Just arrow')
             else return Nothing
    drawnstuff <-
      if style == Fast then return Nothing
      else
        if isnode  then
          do
            rect <- newRectangle [size(8, 8), outline "black",
                                  filling white]
            plusmin1 <- newLine []
            plusmin2 <- newLine []
            hline <- newLine []
            vline <- newLine []
            if isopen then plusmin1 # filling white >> done else done
            return (Just (Just rect, [plusmin1, plusmin2], hline, vline))
        else
          do
            rect <- return Nothing
            hline <- newLine []
            vline <- newLine []
            return (Just (rect, [], hline, vline))
    img <- newLabel [background "white", parent box]
    txt <- newLabel [background "white", parent box,
                     font (Lucida, 12::Int)]
    emb <- newEmbeddedCanvasWin box [anchor NorthWest]
    obj <- return(TREELISTOBJECT val treelist isnode arrow drawnstuff img
                                 txt emb)
    foldl (>>=) (return obj) cnf
    nm <- getName obj
    interactor (\i -> (mouseEnter txt >>>
                         do
                           b <- isSelected treelist obj
                           (if b then done
                            else
                              txt # bg "grey" >>
                              txt # fg "white" >> 
                              sendIO msgQ
                                     (TreeListObject (val, nm,
                                                      if isnode then Node
                                                      else Leaf)) >>
                              done)) +>
                      (mouseLeave txt >>>
                         do
                           b <- isSelected treelist obj
                           (if b then done
                            else
                              txt # bg "white" >>
                              txt # fg "black" >> done)) +>
                      (mouseButtonPress txt 1 >>>
                         selectObject treelist obj) +>
                      (mouseButtonPress cnv 1 >>>
                         deselect treelist))
    return obj

selRect :: Maybe (Maybe Rectangle, [Line], Line, Line) -> Rectangle
selRect (Just (Just rect, _, _, _)) = rect

selPlusMinus :: Maybe (Maybe Rectangle, [Line], Line, Line) -> [Line]
selPlusMinus (Just (_, plusminus, _, _)) = plusminus

selHLine :: Maybe (Maybe Rectangle, [Line], Line, Line) -> Line
selHLine (Just (_, _, hline, _)) = hline

selVLine :: Maybe (Maybe Rectangle, [Line], Line, Line) -> Line
selVLine (Just (_, _, _, vline)) = vline

instance Eq (TREELISTOBJECT a) where
  (TREELISTOBJECT _ _ _ _ _ _ txt1 _) ==
    (TREELISTOBJECT _ _ _ _ _ _ txt2 _) = txt1 == txt2

instance GUIObject (TREELISTOBJECT a) where
  toGUIObject (TREELISTOBJECT _ _ _ _ _ _ _ emb) = toGUIObject emb
  cname _ = "TREELISTOBJECT"

instance HasPhoto (TREELISTOBJECT a) where
  photo i obj@(TREELISTOBJECT _ _ _ _ _ img _ _) =
    img # photo i >> return obj
  getPhoto (TREELISTOBJECT _ _ _ _ _ img _ _) = getPhoto img

name :: ObjectName -> Config (TREELISTOBJECT a)
name nm obj@(TREELISTOBJECT _ _ _ _ _ _ txt _) =
  txt # value nm >> return obj

getName :: TREELISTOBJECT a -> IO String
getName (TREELISTOBJECT _ _ _ _ _ _ txt _) = getValue txt

closedImg = newImage  [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMfAwMD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
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

openImg = newImage [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMfAwMD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
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

leafImg = newImage [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMf///8AAAD///8AAAAAAAAAAAAAAAAAAAAAAAAA
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
