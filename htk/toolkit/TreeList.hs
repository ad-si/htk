{- ------------------------------------------------------------------------
 -
 - Module TreeList
 -
 - HTk tree lists
 -
 - Author: ludi
 - $Revision$ from $Date$
 -
 - ------------------------------------------------------------------------ -}

module TreeList (

  TreeList(..),
  TreeListObject,
  newTreeList,
  setRoot,
  ChildrenFun,
  ImageFun,
  selectionEvent

) where

import HTk
import Image
import Canvas
import EmbeddedCanvasWin
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
      putStr (objname ++ "   Open: " ++ show open ++ "   Intendation: " ++
              show intend ++ "\n")
      debugPrintState ents
  else done
debugPrintState [] = putStr "\n\n"

intendation = 15
lineheight = 20


-- tree lists --

data StateEntry a =
  StateEntry (TREELISTOBJECT a)                                       -- object
             Bool                                 -- open: True / closed: False
             Int                                                 -- intendation
             [OldEntry a]                                          -- -> reopen

data OldEntry a =
  OldEntry Image
           String                                                       -- name
           Int                                                   -- intendation
           Bool                                                -- open / closed
           a                                                           -- value

type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]
type ImageFun a = TreeListObject a -> IO Image

data TreeList a =
  TreeList Canvas (ScrollBox Canvas) (RVar [StateEntry a]) (ChildrenFun a)
           (ImageFun a)
           (RVar (Maybe (TREELISTOBJECT a)))     -- selected object
           (MsgQueue (Maybe (TreeListObject a)))

newTreeList :: ChildrenFun a -> ImageFun a -> [Config (TreeList a)] ->
               IO (TreeList a)
newTreeList cfun ifun cnf =
  do
    cnv <- newCanvas []
    scr <- newScrollBox cnv []
    stateref <- newRVar []
    selref <- newRVar Nothing
    msgQ <- newMsgQueue
    treelist <- return(TreeList cnv scr stateref cfun ifun selref msgQ)
    foldl (>>=) (return treelist) cnf
    return treelist

setRoot :: TreeListObject a -> IO ()
setRoot rt@(tl@(TreeList cnv _ stateref _ ifun _ _), val, nm) =
  do
    root@(TREELISTOBJECT _ _ _ img _ emb) <- mkTreeListObject tl val [name nm]
    setVar stateref [StateEntry root False 0 []]
    pho  <- ifun rt
    img # photo pho
    emb # coord [(5, 5)]
    emb # parent cnv
    done

instance GUIObject (TreeList a) where
  toGUIObject (TreeList _ scr _ _ _ _ _) = toGUIObject scr
  cname _ = "TreeList"

instance Destructible (TreeList a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Interactive (TreeList a)

instance Widget (TreeList a)

instance ChildWidget (TreeList a)

instance Synchronized (TreeList a) where
  synchronize = synchronize . toGUIObject

instance HasBorder (TreeList a)

instance HasColour (TreeList a) where
  legalColourID (TreeList cnv _ _ _ _ _ _) = hasBackGroundColour cnv
  setColour treelist@(TreeList cnv _ _ _ _ _ _) cid col =
    setColour cnv cid col >> return treelist
  getColour (TreeList cnv _ _ _ _ _ _) cid = getColour cnv cid

instance HasSize (TreeList a) where
  width s treelist@(TreeList cnv _ _ _ _ _ _) =
    cnv # width s >> return treelist
  getWidth (TreeList cnv _ _ _ _ _ _) = getWidth cnv
  height s treelist@(TreeList cnv _ _ _ _ _ _) =
    cnv # height s >> return treelist
  getHeight (TreeList cnv _ _ _ _ _ _) = getHeight cnv

selectionEvent :: TreeList a -> IA (Maybe (TreeListObject a))
selectionEvent tl@(TreeList _ _ _ _ _ _ msgQ) = lift(receive msgQ)

-- tree list objects --

type TreeListObject a = (TreeList a, a, String)

data TREELISTOBJECT a =
  TREELISTOBJECT a                                                     -- value
                 (TreeList a)                                         -- parent
                 (Label Image)                                         -- arrow
                 (Label Image)                                  -- object image
                 (Label String)                                  -- object name
                 EmbeddedCanvasWin                                      -- main

shiftObject :: Int -> StateEntry a -> IO ()
shiftObject dy (StateEntry (TREELISTOBJECT _ _ _ _ _ emb) _ _ _) =
  moveItem emb 0 (Distance dy)

insertObjects :: TreeList a -> Int -> Position -> Int -> [TREELISTOBJECT a] ->
                 IO ()
insertObjects treelist@(TreeList cnv _ stateref _ ifun _ _) index (x, y) intd
              children =
  do
    state <- getVar stateref
    mapM (shiftObject ((length children) * lineheight)) (drop index state)
    insertObjects' cnv ifun (x, y + Distance lineheight) intd children
  where insertObjects' :: Canvas -> ImageFun a -> Position -> Int ->
                          [TREELISTOBJECT a] -> IO ()
        insertObjects' cnv ifun p@(x, y) i
                       (obj@(TREELISTOBJECT val tl _ img _ emb) : objs) =
          do
            nm <- getName obj
            pho <- ifun (tl, val, nm)
            img # photo pho
            emb # coord [(5 + Distance (i * intendation), y)]
            emb # parent cnv
            insertObjects' cnv ifun (x, y + Distance lineheight) i objs
        insertObjects' _ _ (x, y) _ _ = done
--          cnv # scrollRegion ((0, 0), (0, 0), (x, y), (x, y)) >> done

insertOldObjects :: TreeList a -> Int -> TREELISTOBJECT a -> [OldEntry a] ->
                    IO [StateEntry a]
insertOldObjects treelist@(TreeList _ _ stateref _ _ _ _) index
                 (TREELISTOBJECT _ _ _ _ _ emb) osts =
  do
    state <- getVar stateref
    [(_, y)] <- getCoord emb
    mapM (shiftObject ((length osts) * lineheight)) (drop index state)
    insertOldObjects' treelist (y + Distance lineheight) osts []
  where insertOldObjects' :: TreeList a -> Distance -> [OldEntry a] ->
                             [StateEntry a] -> IO [StateEntry a]
        insertOldObjects' treelist@(TreeList cnv _ _ _ _ _ _) y
                          ((OldEntry img nm i b val) : osts) sts =
          do
            obj@(TREELISTOBJECT _ _ _ _ _ emb) <- mkTreeListObject treelist 
                                                    val [photo img, name nm]
            emb # coord [(Distance (i * intendation), y)]
            emb # parent cnv
            insertOldObjects' treelist (y + Distance lineheight) osts
                              (StateEntry obj b i [] : sts)
        insertOldObjects' _ _ _ sts = return (reverse sts)

removeObjects :: TreeList a -> Int -> [TREELISTOBJECT a] -> IO ()
removeObjects treelist@(TreeList _ _ stateref _ _ _ _) index children = 
  do
    state <- getVar stateref
    mapM (\ (TREELISTOBJECT _ _ _ _ _ emb) -> destroy emb) children
    mapM (shiftObject (-(length children) * lineheight))
         (drop (index + length children) state)
    done

getIntendAndOpen :: TREELISTOBJECT a -> [StateEntry a] -> IO (Int, Bool)
getIntendAndOpen obj (StateEntry obj' isopen i _ : entries) =
  if obj == obj' then (if isopen then return (i, True) else return (i, False))
    else getIntendAndOpen obj entries

mkEntry :: Int -> TREELISTOBJECT a -> StateEntry a
mkEntry i obj = StateEntry obj False i []

getOldEntries :: [StateEntry a] -> TREELISTOBJECT a -> [OldEntry a]
getOldEntries ((StateEntry obj' _ _ oldentries) : sts) obj =
  if obj == obj' then oldentries else getOldEntries sts obj

toOldEntry :: StateEntry a -> IO (OldEntry a)
toOldEntry (StateEntry (obj@(TREELISTOBJECT val _ _ _ _ _)) b i _) =
  do
    nm <- getName obj
    Just img <- getPhoto obj
    return (OldEntry img nm i b val)

getChildren :: [StateEntry a] -> TREELISTOBJECT a ->
               IO ([TREELISTOBJECT a], [OldEntry a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: [StateEntry a] -> TREELISTOBJECT a -> Int ->
                        [TREELISTOBJECT a] -> [OldEntry a] ->
                        IO ([TREELISTOBJECT a], [OldEntry a])
        getChildren' (st@(StateEntry obj' _ intend _) : es) obj i objs osts =
          if (i == -1 && obj /= obj') then getChildren' es obj i objs osts else
            if (obj == obj') then getChildren' es obj intend objs osts else
              if (intend > i) then do
                                     ost <- toOldEntry st
                                     getChildren' es obj i (obj' : objs)
                                                  (ost : osts)
                else return (reverse objs, reverse osts)
        getChildren' _ _ _ objs osts = return (reverse objs, reverse osts)

pressed :: TreeList a -> TREELISTOBJECT a -> IO ()
pressed treelist@(TreeList _ _ stateref cfun _ _ _)
        obj@(TREELISTOBJECT val _ arrow _ _ emb) =
  do
    state <- getVar stateref
    c <- getCoord emb
    index <-
      return
        ((fromJust
          (elemIndex obj (map (\ (StateEntry obj _ _ _) -> obj) state))) + 1)
    (i, b) <- getIntendAndOpen obj state
    (if b then
       do
         (children, sentries) <- getChildren state obj
         removeObjects treelist index children
         setVar stateref (take (index - 1) state ++
                          [StateEntry obj False i sentries] ++
                          drop (index + length children) state)
         (if null children then done
          else do
                 pho <- closedImg
                 arrow # photo pho
                 done)
         nustate <- getVar stateref
         debugPrintState nustate
     else
       do
         oldents <- return (getOldEntries state obj)
         (case oldents of
            [] ->
              do
                nm <- getName obj
                ch <- cfun (treelist, val, nm)
                children <- mkTreeListObjects ch
                insertObjects treelist index (head c) (i + 1) children
                setVar stateref (take (index - 1) state ++
                                 [StateEntry obj True i []] ++
                                 map (mkEntry (i + 1)) children ++
                                 drop index state)
                (if null children then done else
                   do
                     pho <- openImg
                     arrow # photo pho
                     done)
            _ ->
              do
                oldobjs <- insertOldObjects treelist index obj oldents
                setVar stateref (take (index - 1) state ++
                                 [StateEntry obj True i []] ++ oldobjs ++
                                 drop index state)
                pho <- openImg
                arrow # photo pho
                done)
         nustate <- getVar stateref
         debugPrintState nustate)

unmarkSelectedObject :: TreeList a -> IO ()
unmarkSelectedObject (TreeList _ _ _ _ _ selref _) =
  do
    sel <- getVar selref
    case sel of
      Just (TREELISTOBJECT _ _ _ _ txt' _) -> do
                                                txt' # fg "black"
                                                txt' # bg "white"
                                                done
      _ -> done

selectObject :: TreeList a -> TREELISTOBJECT a -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ selref msgQ)
             obj@(TREELISTOBJECT val tl _ _ txt _) =
  do
    unmarkSelectedObject treelist
    setVar selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    nm <- getName obj
    sendIO msgQ (Just (tl, val, nm))

deselect :: TreeList a -> IO ()
deselect treelist@(TreeList _ _ _ _ _ selref msgQ) =
  do
    unmarkSelectedObject treelist
    setVar selref Nothing
    sendIO msgQ Nothing

isSelected :: TreeList a -> TREELISTOBJECT a -> IO Bool
isSelected (TreeList _ _ _ _ _ selref _) obj =
  do
    sel <- getVar selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

mkTreeListObjects :: [TreeListObject a] -> IO [TREELISTOBJECT a]
mkTreeListObjects objs =
  mapM mk objs
  where mk :: TreeListObject a -> IO (TREELISTOBJECT a)
        mk (tl, val, nm) = mkTreeListObject tl val [name nm]

mkTreeListObject :: TreeList a -> a -> [Config (TREELISTOBJECT a)] ->
                     IO (TREELISTOBJECT a)
mkTreeListObject treelist@(TreeList cnv _ _ cfun _ _ _) val cnf =
  do
    box <- newHBox [background "white"]
    pho <- closedImg
    arrow <- newLabel [background "white", photo pho, parent box]
    img <- newLabel [background "white", parent box]
    txt <- newLabel [background "white", parent box]
    emb <- newEmbeddedCanvasWin box [anchor NorthWest]
    obj <- return(TREELISTOBJECT val treelist arrow img txt emb)
    foldl (>>=) (return obj) cnf
    nm <- getName obj
    ch <- (cfun (treelist, val, nm))
    (if null ch then
       do
         pho <- leafImg
         arrow # photo pho
         done
     else done)
    interactor (\i -> (mouseEnter txt >>> do
                                            b <- isSelected treelist obj
                                            (if b then
                                               done
                                             else
                                               txt # bg "grey" >>
                                               txt # fg "white" >> done)) +>
                      (mouseLeave txt >>> do
                                            b <- isSelected treelist obj
                                            (if b then
                                               done
                                             else
                                               txt # bg "white" >>
                                               txt # fg "black" >> done)) +>
                      (mouseButtonPress txt 1 >>> selectObject treelist obj) +>
                      (mouseButtonPress arrow 1 >>> pressed treelist obj) +>
                      (mouseButtonPress cnv 1 >>> deselect treelist))

    return obj

instance Eq (TREELISTOBJECT a) where
  (TREELISTOBJECT _ _ _ txt1 _ _) == (TREELISTOBJECT _ _ _ txt2 _ _) =
    txt1 == txt2

instance GUIObject (TREELISTOBJECT a) where
  toGUIObject (TREELISTOBJECT _ _ _ _ _ emb) = toGUIObject emb
  cname _ = "TreeListObject"

instance Destructible (TREELISTOBJECT a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance HasPhoto (TREELISTOBJECT a) where
  photo i obj@(TREELISTOBJECT _ _ _ img _ _) = img # photo i >> return obj
  getPhoto (TREELISTOBJECT _ _ _ img _ _) = getPhoto img

instance Interactive (TREELISTOBJECT a)

type ObjectName = String

name :: ObjectName -> Config (TREELISTOBJECT a)
name nm obj@(TREELISTOBJECT _ _ _ _ txt _) = txt # value nm >> return obj

getName :: TREELISTOBJECT a -> IO String
getName (TREELISTOBJECT _ _ _ _ txt _) = getValue txt

getObjValue :: TREELISTOBJECT a -> a
getObjValue (TREELISTOBJECT val _ _ _ _ _) = val

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
