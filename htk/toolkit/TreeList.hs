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

  TreeList(..),
  Style(..),
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

debug = True
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
intendation = 20
lineheight = 20


-- tree lists --

data StateEntry a =
  StateEntry (TREELISTOBJECT a)                                  -- object
             Bool                            -- open: True / closed: False
             Int                                            -- intendation
             [OldEntry a]                                     -- -> reopen

data OldEntry a =
  OldEntry Image
           String                                                  -- name
           Int                                              -- intendation
           Bool                                           -- open / closed
           Bool                                                  -- isnode
           a                                                      -- value

data Style = Fast | Pretty deriving Eq

type ChildrenFun a = TreeListObject a -> IO [TreeListObject a]
type ImageFun a = TreeListObject a -> IO Image

data TreeList a =
  TreeList Canvas
           (ScrollBox Canvas)
           Style                                         -- treelist style
           (RVar [StateEntry a])                         -- treelist state
           (ChildrenFun a)                       -- node children function
           (ImageFun a)                           -- object image function
           (RVar (Maybe (TREELISTOBJECT a)))            -- selected object
           (MsgQueue (Maybe (TreeListObject a)))     -- selection notifier

newTreeList :: Style -> ChildrenFun a -> ImageFun a ->
               [Config (TreeList a)] -> IO (TreeList a)
newTreeList style cfun ifun cnf =
  do
    cnv <- newCanvas []
    scr <- newScrollBox cnv []
    stateref <- newRVar []
    selref <- newRVar Nothing
    msgQ <- newMsgQueue
    treelist <- return(TreeList cnv scr style stateref cfun ifun selref
                                msgQ)
    foldl (>>=) (return treelist) cnf
    return treelist

setRoot :: TreeListObject a -> IO ()
setRoot rt@(tl@(TreeList cnv _ style stateref _ ifun _ _), val, nm,
            isnode) =
  do
    root@(TREELISTOBJECT _ _ _ _ drawnstuff img _ emb) <-
      mkTreeListObject tl val isnode False [name nm]
    setVar stateref [StateEntry root False 0 []]
    pho  <- ifun rt
    img # photo pho
    (if style == Fast then
       emb # coord [(5, 5)] >> done
     else
       do
         emb # coord [(20, 5)]
         selRect (fromJust drawnstuff) # position (5, 5 + 6)
         selRect (fromJust drawnstuff) # parent cnv
         done)
    emb # parent cnv
    done

instance GUIObject (TreeList a) where
  toGUIObject (TreeList _ scr _ _ _ _ _ _) = toGUIObject scr
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
  legalColourID (TreeList cnv _ _ _ _ _ _ _) = hasBackGroundColour cnv
  setColour treelist@(TreeList cnv _ _ _ _ _ _ _) cid col =
    setColour cnv cid col >> return treelist
  getColour (TreeList cnv _ _ _ _ _ _ _) cid = getColour cnv cid

instance HasSize (TreeList a) where
  width s treelist@(TreeList cnv _ _ _ _ _ _ _) =
    cnv # width s >> return treelist
  getWidth (TreeList cnv _ _ _ _ _ _ _) = getWidth cnv
  height s treelist@(TreeList cnv _ _ _ _ _ _ _) =
    cnv # height s >> return treelist
  getHeight (TreeList cnv _ _ _ _ _ _ _) = getHeight cnv

selectionEvent :: TreeList a -> IA (Maybe (TreeListObject a))
selectionEvent tl@(TreeList _ _ _ _ _ _ _ msgQ) = lift(receive msgQ)

-- tree list objects --

type TreeListObject a = (TreeList a, a, String, Bool)

data TREELISTOBJECT a =
  TREELISTOBJECT a                                                -- value
                 (TreeList a)                                    -- parent
                 Bool                                      -- true if node
                 (Maybe (Label Image))           -- arrow if style is fast
                 (Maybe (Rectangle, [Line]))      -- drawn stuff if pretty
                 (Label Image)                             -- object image
                 (Label String)                             -- object name
                 EmbeddedCanvasWin                           -- main frame

shiftObject :: Style -> Int -> StateEntry a -> IO ()
shiftObject style dy (StateEntry (TREELISTOBJECT _ _ _ _ drawnstuff _ _
                                                 emb) _ _ _) =
  do
    (if style == Pretty then
       moveItem (selRect (fromJust drawnstuff)) 0 (Distance dy) >> done
     else done)
    moveItem emb 0 (Distance dy)

insertObjects :: TreeList a -> Position -> Int ->
                 [TREELISTOBJECT a] -> IO ()
insertObjects treelist@(TreeList cnv _ style stateref _ ifun _ _) (x, y)
              intd children =
  do
    state <- getVar stateref
    insertObjects' cnv ifun (x, y + Distance lineheight) intd children
  where insertObjects' :: Canvas -> ImageFun a -> Position -> Int ->
                          [TREELISTOBJECT a] -> IO ()
        insertObjects' cnv ifun p@(x, y) i
                       (obj@(TREELISTOBJECT val tl isnode _ drawnstuff img
                                            _ emb) :
                        objs) =
          do
            nm <- getName obj
            pho <- ifun (tl, val, nm, isnode)
            img # photo pho

            (if style == Fast then
               emb # coord [(5 + Distance (i * intendation), y)] >> done
             else
               do
                 emb # coord [(20 + Distance (i * intendation), y)]
                 selRect (fromJust drawnstuff) #
                   position (5 + Distance (i * intendation), y + 6)
                 selRect (fromJust drawnstuff) # parent cnv
                 done)
            emb # parent cnv
            insertObjects' cnv ifun (x, y + Distance lineheight) i objs
        insertObjects' _ _ (x, y) _ _ = done
--          cnv # scrollRegion ((0, 0), (0, 0), (x, y), (x, y)) >> done

insertOldObjects :: TreeList a -> Int -> TREELISTOBJECT a ->
                    [OldEntry a] -> IO [StateEntry a]
insertOldObjects treelist@(TreeList _ _ style stateref _ _ _ _) index
                 (TREELISTOBJECT _ _ _ _ drawnstuff _ _ emb) osts =
  do
    state <- getVar stateref
    [(_, y)] <- getCoord emb
    mapM (shiftObject style ((length osts) * lineheight))
         (drop index state)
    insertOldObjects' treelist (y + Distance lineheight) osts []
  where insertOldObjects' :: TreeList a -> Distance -> [OldEntry a] ->
                             [StateEntry a] -> IO [StateEntry a]
        insertOldObjects' treelist@(TreeList cnv _ _ _ _ _ _ _) y
                          ((OldEntry img nm i isopen isnode val) : osts)
                          sts =
          do
            obj@(TREELISTOBJECT _ _ _ _ _ _ _ emb) <-
              mkTreeListObject treelist val isnode isopen
                               [photo img, name nm]
            (if style == Fast then
               emb # coord [(5 + Distance (i * intendation), y)] >> done
             else
               do
                 emb # coord [(20 + Distance (i * intendation), y)]
                 selRect (fromJust drawnstuff) #
                   position (5 + Distance (i * intendation), y + 6)
                 selRect (fromJust drawnstuff) # parent cnv
                 done)
            emb # parent cnv
            insertOldObjects' treelist (y + Distance lineheight) osts
                              (StateEntry obj isopen i [] : sts)
        insertOldObjects' _ _ _ sts = return (reverse sts)

removeObjects :: TreeList a -> Int -> [TREELISTOBJECT a] -> IO ()
removeObjects treelist@(TreeList _ _ style stateref _ _ _ _) index
              children = 
  do
    state <- getVar stateref
    mapM (\ (TREELISTOBJECT _ _ _ _ _ _ _ emb) -> destroy emb) children
    mapM (shiftObject style (-(length children) * lineheight))
         (drop (index + length children) state)
    done

getIntendAndOpen :: TREELISTOBJECT a -> [StateEntry a] -> IO (Int, Bool)
getIntendAndOpen obj (StateEntry obj' isopen i _ : entries) =
  if obj == obj' then (if isopen then
                         return (i, True)
                       else return (i, False))
  else getIntendAndOpen obj entries

mkEntry :: Int -> TREELISTOBJECT a -> StateEntry a
mkEntry i obj = StateEntry obj False i []

getOldEntries :: [StateEntry a] -> TREELISTOBJECT a -> [OldEntry a]
getOldEntries ((StateEntry obj' _ _ oldentries) : sts) obj =
  if obj == obj' then oldentries else getOldEntries sts obj

toOldEntry :: StateEntry a -> IO (OldEntry a)
toOldEntry (StateEntry (obj@(TREELISTOBJECT val _ isnode _ _ _ _ _))
                       isopen i _) =
  do
    nm <- getName obj
    Just img <- getPhoto obj
    return (OldEntry img nm i isopen isnode val)

getChildren :: [StateEntry a] -> TREELISTOBJECT a ->
               IO ([TREELISTOBJECT a], [OldEntry a])
getChildren state obj = getChildren' state obj (-1) [] []
  where getChildren' :: [StateEntry a] -> TREELISTOBJECT a -> Int ->
                        [TREELISTOBJECT a] -> [OldEntry a] ->
                        IO ([TREELISTOBJECT a], [OldEntry a])
        getChildren' (st@(StateEntry obj' _ intend _) : es) obj i objs
                     osts =
          if (i == -1 && obj /= obj') then
            getChildren' es obj i objs osts
          else
            if (obj == obj') then
              getChildren' es obj intend objs osts
            else
              if (intend > i) then
                do
                  ost <- toOldEntry st
                  getChildren' es obj i (obj' : objs) (ost : osts)
              else
                return (reverse objs, reverse osts)
        getChildren' _ _ _ objs osts = return (reverse objs, reverse osts)

pressed :: TreeList a -> TREELISTOBJECT a -> IO ()
pressed treelist@(TreeList _ _ style stateref cfun _ _ _)
        obj@(TREELISTOBJECT val _ isnode arrow _ _ _ emb) =
  do
    state <- getVar stateref
    c <- getCoord emb
    index <-
      return
        ((fromJust
          (elemIndex obj (map (\ (StateEntry obj _ _ _) -> obj)
                     state))) + 1)
    (i, b) <- getIntendAndOpen obj state
    (if b then
       do                                                 -- *** Close ***
         (if style == Fast then
            do
              pho <- closedImg
              fromJust arrow # photo pho
              done
          else done)
         (children, sentries) <- getChildren state obj
         removeObjects treelist index children
         setVar stateref (take (index - 1) state ++
                          [StateEntry obj False i sentries] ++
                          drop (index + length children) state)
         nustate <- getVar stateref
         debugPrintState nustate
     else
       do                                                  -- *** Open ***
         (if style == Fast then
            do
              pho <- openImg
              fromJust arrow # photo pho
              done
          else done)
         oldents <- return (getOldEntries state obj)
         (case oldents of
            [] ->
              do
                debugMsg "inserting new entries"
                nm <- getName obj
                ch <- cfun (treelist, val, nm, isnode)
                mapM (shiftObject style ((length ch) * lineheight))
                     (drop index state)
                children <- mkTreeListObjects ch
                insertObjects treelist (head c) (i + 1) children
                setVar stateref (take (index - 1) state ++
                                 [StateEntry obj True i []] ++
                                 map (mkEntry (i + 1)) children ++
                                 drop index state)
            _ ->
              do
                oldobjs <- insertOldObjects treelist index obj oldents
                setVar stateref (take (index - 1) state ++
                                 [StateEntry obj True i []] ++ oldobjs ++
                                 drop index state))
         nustate <- getVar stateref
         debugPrintState nustate)

unmarkSelectedObject :: TreeList a -> IO ()
unmarkSelectedObject (TreeList _ _ _ _ _ _ selref _) =
  do
    sel <- getVar selref
    case sel of
      Just (TREELISTOBJECT _ _ _ _ _ _ txt _) -> do
                                                   txt # fg "black"
                                                   txt # bg "white"
                                                   done
      _ -> done

selectObject :: TreeList a -> TREELISTOBJECT a -> IO ()
selectObject treelist@(TreeList _ _ _ _ _ _ selref msgQ)
             obj@(TREELISTOBJECT val tl isnode _ _ _ txt _) =
  do
    unmarkSelectedObject treelist
    setVar selref (Just obj)
    txt # fg "white"
    txt # bg "blue"
    nm <- getName obj
    sendIO msgQ (Just (tl, val, nm, isnode))

deselect :: TreeList a -> IO ()
deselect treelist@(TreeList _ _ _ _ _ _ selref msgQ) =
  do
    unmarkSelectedObject treelist
    setVar selref Nothing
    sendIO msgQ Nothing

isSelected :: TreeList a -> TREELISTOBJECT a -> IO Bool
isSelected (TreeList _ _ _ _ _ _ selref _) obj =
  do
    sel <- getVar selref
    case sel of
      Just s -> return (s == obj)
      _ -> return False

mkTreeListObjects :: [TreeListObject a] -> IO [TREELISTOBJECT a]
mkTreeListObjects objs =
  mapM mk objs
  where mk :: TreeListObject a -> IO (TREELISTOBJECT a)
        mk (tl, val, nm, b) = mkTreeListObject tl val b False [name nm]

mkTreeListObject :: TreeList a -> a -> Bool -> Bool ->
                    [Config (TREELISTOBJECT a)] -> IO (TREELISTOBJECT a)
mkTreeListObject treelist@(TreeList cnv _ style _ cfun _ _ _) val isnode
                 isopen cnf =
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
        do
          rect <- newRectangle [size(7, 7), outline "black"]
          return (Just (rect, []))
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
                              txt # fg "white" >> done)) +>
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
    (if style == Fast then
       interactor (\i -> mouseButtonPress (fromJust arrow) 1 >>>
                         pressed treelist obj)
     else
       interactor (\i -> mouseButtonPress
                           (selRect (fromJust drawnstuff)) 1 >>>
                         pressed treelist obj))
    return obj

selRect :: (Rectangle, [Line]) -> Rectangle
selRect (rect, _) = rect

instance Eq (TREELISTOBJECT a) where
  (TREELISTOBJECT _ _ _ _ _ _ txt1 _) ==
    (TREELISTOBJECT _ _ _ _ _ _ txt2 _) = txt1 == txt2

instance GUIObject (TREELISTOBJECT a) where
  toGUIObject (TREELISTOBJECT _ _ _ _ _ _ _ emb) = toGUIObject emb
  cname _ = "TreeListObject"

instance Destructible (TREELISTOBJECT a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance HasPhoto (TREELISTOBJECT a) where
  photo i obj@(TREELISTOBJECT _ _ _ _ _ img _ _) =
    img # photo i >> return obj
  getPhoto (TREELISTOBJECT _ _ _ _ _ img _ _) = getPhoto img

instance Interactive (TREELISTOBJECT a)

type ObjectName = String

name :: ObjectName -> Config (TREELISTOBJECT a)
name nm obj@(TREELISTOBJECT _ _ _ _ _ _ txt _) =
  txt # value nm >> return obj

getName :: TREELISTOBJECT a -> IO String
getName (TREELISTOBJECT _ _ _ _ _ _ txt _) = getValue txt

getObjValue :: TREELISTOBJECT a -> a
getObjValue (TREELISTOBJECT val _ _ _ _ _ _ _) = val

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
