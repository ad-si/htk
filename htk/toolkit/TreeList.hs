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
  getObjValue,
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
  StateEntry (TreeListObject a)                                       -- object
             Bool                                 -- open: True / closed: False
             Int                                                 -- intendation
             [OldEntry a]                                          -- -> reopen

data OldEntry a =
  OldEntry Image
           String                                                       -- name
           Int                                                   -- intendation
           Bool                                                -- open / closed
           a                                                           -- value

type ChildrenFun a = TreeList a -> TreeListObject a -> IO [TreeListObject a]
type ImageFun a = TreeListObject a -> IO Image

data TreeList a =
  TreeList Canvas (ScrollBox Canvas) (RVar [StateEntry a]) (ChildrenFun a)
           (ImageFun a)
           (RVar (Maybe (TreeListObject a)))     -- selected object
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

setRoot :: TreeList a -> TreeListObject a -> IO ()
setRoot (TreeList cnv _ stateref _ ifun _ _)
        root@(TreeListObject _ _ _ img _ emb) =
  do
    setVar stateref [StateEntry root False 0 []]
    pho  <- ifun root
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
selectionEvent (TreeList _ _ _ _ _ _ msgQ) = lift(receive msgQ)


-- tree list objects --

data TreeListObject a =
  TreeListObject a                                                     -- value
                 (TreeList a)                                         -- parent
                 (Label Image)                                         -- arrow
                 (Label Image)                                  -- object image
                 (Label String)                                  -- object name
                 EmbeddedCanvasWin                                      -- main

shiftObject :: Int -> StateEntry a -> IO ()
shiftObject dy (StateEntry (TreeListObject _ _ _ _ _ emb) _ _ _) =
  moveItem emb 0 (Distance dy)

insertObjects :: TreeList a -> Int -> Position -> Int -> [TreeListObject a] ->
                 IO ()
insertObjects treelist@(TreeList cnv _ stateref _ ifun _ _) index (x, y) intd
              children =
  do
    state <- getVar stateref
    mapM (shiftObject ((length children) * lineheight)) (drop index state)
    insertObjects' cnv ifun (x, y + Distance lineheight) intd children
  where insertObjects' :: Canvas -> ImageFun a -> Position -> Int ->
                          [TreeListObject a] -> IO ()
        insertObjects' cnv ifun p@(x, y) i
                       (obj@(TreeListObject _ _ _ img _ emb) : objs) =
          do
            pho <- ifun obj
            img # photo pho
            emb # coord [(5 + Distance (i * intendation), y)]
            emb # parent cnv
            insertObjects' cnv ifun (x, y + Distance lineheight) i objs
        insertObjects' _ _ (x, y) _ _ = done
--          cnv # scrollRegion ((0, 0), (0, 0), (x, y), (x, y)) >> done

insertOldObjects :: TreeList a -> Int -> TreeListObject a -> [OldEntry a] ->
                    IO [StateEntry a]
insertOldObjects treelist@(TreeList _ _ stateref _ _ _ _) index
                 (TreeListObject _ _ _ _ _ emb) osts =
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
            obj@(TreeListObject _ _ _ _ _ emb) <- newTreeListObject treelist 
                                                    val [photo img, name nm]
            emb # coord [(Distance (i * intendation), y)]
            emb # parent cnv
            insertOldObjects' treelist (y + Distance lineheight) osts
                              (StateEntry obj b i [] : sts)
        insertOldObjects' _ _ _ sts = return (reverse sts)

removeObjects :: TreeList a -> Int -> [TreeListObject a] -> IO ()
removeObjects treelist@(TreeList _ _ stateref _ _ _ _) index children = 
  do
    state <- getVar stateref
    mapM (\ (TreeListObject _ _ _ _ _ emb) -> destroy emb) children
    mapM (shiftObject (-(length children) * lineheight))
         (drop (index + length children) state)
    done

getIntendAndOpen :: TreeListObject a -> [StateEntry a] -> IO (Int, Bool)
getIntendAndOpen obj (StateEntry obj' isopen i _ : entries) =
  if obj == obj' then (if isopen then return (i, True) else return (i, False))
    else getIntendAndOpen obj entries

mkEntry :: Int -> TreeListObject a -> StateEntry a
mkEntry i obj = StateEntry obj False i []

getOldEntries :: [StateEntry a] -> TreeListObject a -> [OldEntry a]
getOldEntries ((StateEntry obj' _ _ oldentries) : sts) obj =
  if obj == obj' then oldentries else getOldEntries sts obj

toOldEntry :: StateEntry a -> IO (OldEntry a)
toOldEntry (StateEntry (obj@(TreeListObject val _ _ _ _ _)) b i _) =
  do
    nm <- getName obj
    Just img <- getPhoto obj
    return (OldEntry img nm i b val)

getChildren :: [StateEntry a] -> TreeListObject a ->
               IO ([TreeListObject a], [OldEntry a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: [StateEntry a] -> TreeListObject a -> Int ->
                        [TreeListObject a] -> [OldEntry a] ->
                        IO ([TreeListObject a], [OldEntry a])
        getChildren' (st@(StateEntry obj' _ intend _) : es) obj i objs osts =
          if (i == -1 && obj /= obj') then getChildren' es obj i objs osts else
            if (obj == obj') then getChildren' es obj intend objs osts else
              if (intend > i) then do
                                     ost <- toOldEntry st
                                     getChildren' es obj i (obj' : objs)
                                                  (ost : osts)
                else return (reverse objs, reverse osts)
        getChildren' _ _ _ objs osts = return (reverse objs, reverse osts)

pressed :: TreeList a -> TreeListObject a -> IO ()
pressed treelist@(TreeList _ _ stateref cfun _ _ _)
        obj@(TreeListObject _ _ arrow _ _ emb) =
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
                children <- cfun treelist obj
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
      Just (TreeListObject _ _ _ _ txt' _) -> do
                                                txt' # fg "black"
                                                txt' # bg "white"
                                                done
      _ -> done

selectObject :: TreeList a -> TreeListObject a -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ selref msgQ)
             obj@(TreeListObject _ _ _ _ txt _) =
  do
    unmarkSelectedObject treelist
    setVar selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    sendIO msgQ (Just obj)

deselect :: TreeList a -> IO ()
deselect treelist@(TreeList _ _ _ _ _ selref msgQ) =
  do
    unmarkSelectedObject treelist
    setVar selref Nothing
    sendIO msgQ Nothing

isSelected :: TreeList a -> TreeListObject a -> IO Bool
isSelected (TreeList _ _ _ _ _ selref _) obj =
  do
    sel <- getVar selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

newTreeListObject :: TreeList a -> a -> [Config (TreeListObject a)] ->
                     IO (TreeListObject a)
newTreeListObject treelist@(TreeList cnv _ _ cfun _ _ _) val cnf =
  do
    box <- newHBox [background "white"]
    pho <- closedImg
    arrow <- newLabel [background "white", photo pho, parent box]
    img <- newLabel [background "white", parent box]
    txt <- newLabel [background "white", parent box]
    emb <- newEmbeddedCanvasWin box [anchor NorthWest]
    obj <- return(TreeListObject val treelist arrow img txt emb)
    foldl (>>=) (return obj) cnf
{-    ch <- (cfun treelist obj)
    (if null ch then
       do
         pho <- leafImg
         arrow # photo pho
         done
     else done) -}
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

instance Eq (TreeListObject a) where
  (TreeListObject _ _ _ txt1 _ _) == (TreeListObject _ _ _ txt2 _ _) =
    txt1 == txt2

instance GUIObject (TreeListObject a) where
  toGUIObject (TreeListObject _ _ _ _ _ emb) = toGUIObject emb
  cname _ = "TreeListObject"

instance Destructible (TreeListObject a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance HasPhoto (TreeListObject a) where
  photo i obj@(TreeListObject _ _ _ img _ _) = img # photo i >> return obj
  getPhoto (TreeListObject _ _ _ img _ _) = getPhoto img

instance Interactive (TreeListObject a)

type ObjectName = String

name :: ObjectName -> Config (TreeListObject a)
name nm obj@(TreeListObject _ _ _ _ txt _) = txt # value nm >> return obj

getName :: TreeListObject a -> IO String
getName (TreeListObject _ _ _ _ txt _) = getValue txt

getObjValue :: TreeListObject a -> a
getObjValue (TreeListObject val _ _ _ _ _) = val

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

leafImg = newImage [imgData GIF "R0lGODdhDAAMAPAAAAAA/wD//ywAAAAADAAMAMfAwMD///8AAAD///8AAAAAAAAAAAAAAAAAAAAA
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
IQADCBxIsKBBgwASKjzIcKDChQ0jSnx48GHCihAlahwYEAA7"]
