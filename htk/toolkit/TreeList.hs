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

  newTreeList,
  setRoot,
  newTreeListObject,
  name,
  getName,
  ObjectName,
  ChildrenFun,
  ImageFun,
  TreeList(..),
  TreeListObject(..),
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

debug = True
debugMsg str = if debug then putStr(">>> " ++ str ++ "\n\n") else done

debugPrintState ((StateEntry obj open intend) : ents) =
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

data StateEntry =
  StateEntry TreeListObject                                           -- object
             Bool                                 -- open: True / closed: False
             Int                                                 -- intendation

type ChildrenFun = TreeList -> TreeListObject -> IO [TreeListObject]
type ImageFun = TreeListObject -> IO Image

data TreeList =
  TreeList Canvas (ScrollBox Canvas) (RVar [StateEntry]) ChildrenFun ImageFun
           (RVar (Maybe TreeListObject))     -- selected object
           (MsgQueue (Maybe TreeListObject))

newTreeList :: ChildrenFun -> ImageFun -> [Config TreeList] -> IO TreeList
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

setRoot :: TreeList -> TreeListObject -> IO ()
setRoot (TreeList cnv _ stateref _ ifun _ _)
        root@(TreeListObject _ _ img _ emb) =
  do
    setVar stateref [StateEntry root False 0]
    pho  <- ifun root
    img # photo pho
    emb # coord [(5, 5)]
    emb # parent cnv
    done

instance GUIObject TreeList where
  toGUIObject (TreeList _ scr _ _ _ _ _) = toGUIObject scr
  cname _ = "TreeList"

instance Destructible TreeList where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Interactive TreeList

instance Widget TreeList

instance ChildWidget TreeList

instance Synchronized TreeList where
  synchronize = synchronize . toGUIObject

instance HasBorder TreeList

instance HasColour TreeList where
  legalColourID (TreeList cnv _ _ _ _ _ _) = hasBackGroundColour cnv
  setColour treelist@(TreeList cnv _ _ _ _ _ _) cid col =
    setColour cnv cid col >> return treelist
  getColour (TreeList cnv _ _ _ _ _ _) cid = getColour cnv cid

instance HasSize TreeList where
  width s treelist@(TreeList cnv _ _ _ _ _ _) =
    cnv # width s >> return treelist
  getWidth (TreeList cnv _ _ _ _ _ _) = getWidth cnv
  height s treelist@(TreeList cnv _ _ _ _ _ _) =
    cnv # height s >> return treelist
  getHeight (TreeList cnv _ _ _ _ _ _) = getHeight cnv

selectionEvent :: TreeList -> IA (Maybe TreeListObject)
selectionEvent (TreeList _ _ _ _ _ _ msgQ) = lift(receive msgQ)


-- tree list objects --

data TreeListObject =
  TreeListObject TreeList                                             -- parent
                 (Label Image)                                         -- arrow
                 (Label Image)                                  -- object image
                 (Label String)                                  -- object name
                 EmbeddedCanvasWin                                      -- main

shiftObject :: Int -> StateEntry -> IO ()
shiftObject dy (StateEntry (TreeListObject _ _ _ _ emb) _ _) =
  moveItem emb 0 (Distance dy)

insertObjects' :: Canvas -> ImageFun -> Position -> Int -> [TreeListObject] -> 
                  IO ()
insertObjects' cnv ifun p@(x, y) i
              (obj@(TreeListObject _ _ img _ emb) : objs) =
  do
    pho <- ifun obj
    img # photo pho
    emb # coord [(5 + Distance (i * intendation), y)]
    emb # parent cnv
    insertObjects' cnv ifun (x, y + Distance lineheight) i objs
insertObjects' _ _ _ _ _ = done

insertObjects :: TreeList -> Int -> Position -> Int -> [TreeListObject] ->
                 IO ()
insertObjects treelist@(TreeList cnv _ stateref _ ifun _ _) index (x, y) intd
              children =
  do
    state <- getVar stateref
    mapM (shiftObject ((length children) * lineheight)) (drop index state)
    insertObjects' cnv ifun (x, y + Distance lineheight) intd children

removeObjects :: TreeList -> Int -> [TreeListObject] -> IO ()
removeObjects treelist@(TreeList _ _ stateref _ _ _ _) index children = 
  do
    state <- getVar stateref
    mapM (\ (TreeListObject _ _ _ _ emb) -> destroy emb) children
    mapM (shiftObject (-(length children) * lineheight))
         (drop (index + length children) state)
    done

getIntendAndOpen :: TreeListObject -> [StateEntry] -> IO (Int, Bool)
getIntendAndOpen obj (StateEntry obj' isopen i : entries) =
  if obj == obj' then (if isopen then return (i, True) else return (i, False))
    else getIntendAndOpen obj entries

mkEntry :: Int -> TreeListObject -> StateEntry
mkEntry i obj = StateEntry obj False i

pressed :: TreeList -> TreeListObject -> IO ()
pressed treelist@(TreeList _ _ stateref cfun _ _ _)
        obj@(TreeListObject _ arrow _ _ emb) =
  do
    children <- cfun treelist obj
    state <- getVar stateref
    c <- getCoord emb
    index <-
      return
        ((fromJust
          (elemIndex obj (map (\ (StateEntry obj _ _) -> obj) state))) + 1)
    (i,b) <- getIntendAndOpen obj state
    (if b then
       do
         removeObjects treelist index children
         setVar stateref (take (index - 1) state ++
                          [StateEntry obj False i] ++
                          drop (index + length children) state)
         pho <- newImage [filename "./images/closed.gif"]
         arrow # photo pho
         nustate <- getVar stateref
         debugPrintState nustate
     else
       do
         insertObjects treelist index (head c) (i + 1) children
         setVar stateref (take (index - 1) state ++ [StateEntry obj True i] ++
                          map (mkEntry (i + 1)) children ++ drop index state)
         pho <- newImage [filename "./images/open.gif"]
         arrow # photo pho
         nustate <- getVar stateref
         debugPrintState nustate)

unmarkSelectedObject :: TreeList -> IO ()
unmarkSelectedObject (TreeList _ _ _ _ _ selref _) =
  do
    sel <- getVar selref
    case sel of
      Just (TreeListObject _ _ _ txt' _) -> do
                                                txt' # fg "black"
                                                txt' # bg "white"
                                                done
      _ -> done

selectObject :: TreeList -> TreeListObject -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ selref msgQ)
             obj@(TreeListObject _ _ _ txt _) =
  do
    unmarkSelectedObject treelist
    setVar selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    sendIO msgQ (Just obj)

deselect :: TreeList -> IO ()
deselect treelist@(TreeList _ _ _ _ _ selref msgQ) =
  do
    unmarkSelectedObject treelist
    setVar selref Nothing
    sendIO msgQ Nothing

isSelected :: TreeList -> TreeListObject -> IO Bool
isSelected (TreeList _ _ _ _ _ selref _) obj =
  do
    sel <- getVar selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

newTreeListObject :: TreeList -> [Config TreeListObject] -> IO TreeListObject
newTreeListObject treelist@(TreeList cnv _ _ _ _ _ _) cnf =
  do
    box <- newHBox [background "white"]
    pho <- newImage [filename "./images/closed.gif"]
    arrow <- newLabel [background "white", photo pho, parent box]
    img <- newLabel [background "white", parent box]
    txt <- newLabel [background "white", parent box]
    emb <- newEmbeddedCanvasWin box [anchor NorthWest]
    obj <- return(TreeListObject treelist arrow img txt emb)
    foldl (>>=) (return obj) cnf
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

instance Eq TreeListObject where
  (TreeListObject _ _ txt1 _ _) == (TreeListObject _ _ txt2 _ _) =
    txt1 == txt2

instance GUIObject TreeListObject where
  toGUIObject (TreeListObject _ _ _ _ emb) = toGUIObject emb
  cname _ = "TreeListObject"

instance Destructible TreeListObject where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance HasPhoto TreeListObject where
  photo i obj@(TreeListObject _ _ img _ _) = img # photo i >> return obj
  getPhoto (TreeListObject _ _ img _ _) = getPhoto img

instance Interactive TreeListObject

type ObjectName = String

name :: ObjectName -> Config TreeListObject
name nm obj@(TreeListObject _ _ _ txt _) = txt # value nm >> return obj

getName :: TreeListObject -> IO String
getName (TreeListObject _ _ _ txt _) = getValue txt
