{- ------------------------------------------------------------------------
 -
 - Module DragAndDrop
 -
 - Scrolled canvases with drag and drop support.
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - ------------------------------------------------------------------------ -}


module DragAndDrop (

Notepad,
NotepadItem,
ItemName(..),

name,              {- ItemName -> Config (NotepadItem a) -}
getName,           {- NotepadItem a -> (IO ItemName) -}

newNotepad,        {- Bool -> [Config (Notepad a)] -> IO (Notepad a) -}
newNotepadItem,    {- a -> Notepad a -> [Config (NotepadItem a)] ->
                      IO (NotepadItem a) -}

selectAll,         {- Notepad a -> IO () -}
deselectAll,       {- Notepad a -> IO () -}
selectItem,        {- Notepad a -> NotepadItem a -> IO () -}
selectAnotherItem, {- Notepad a -> NotepadItem a -> IO () -}
selectItemsWithin, {- Position -> Position -> Notepad a -> IO () -}
deselectItem,      {- Notepad a -> NotepadItem a -> IO () -}

deleteItem,        {- Notepad a -> NotepadItem a -> IO () -}
getSelectedItems,  {- Notepad a -> NotepadItem a -> IO () -}

--dropEvent,          {- NotepadItem a -> IA [(NotepadItem a)] -}
selectionEvent,    {- NotepadItem a -> IA () -}
) where

import Concurrency
import Channels
import GUICore
import HTk
import EmbeddedCanvasWin
import Frame
import Image
import Canvas
import CanvasItemAux
import CanvasItem
import ImageItem
import TextItem
import Rectangle
import Mouse
import CanvasTag
import ScrollBox
import Printer
import RVar

debug = True

debugMsg str = if debug then putStr(">>> " ++ str ++ "\n\n") else done


-------------------
-- Notepad items --
-------------------

-- type --

data NotepadItem a =
  NotepadItem ImageItem (TextItem String) (RVar a) (RVar ItemName)
    (RVar (Maybe (Rectangle, Rectangle))) (MsgQueue (Bool))


-- constructor --

newNotepadItem :: a -> Notepad a -> [Config (NotepadItem a)] ->
                  IO (NotepadItem a)
newNotepadItem val notepad@(Notepad cnv _ _ _ _) cnf =
  do
    img <- newImageItem [parent cnv]
    txt <- newTextItem [parent cnv]
    itemval <- newRVar val
    itemname <- newRVar (ItemName { short = \_ -> "", full = "" })
    itemsel <- newRVar Nothing
    msgQ <- newMsgQueue :: IO (MsgQueue Bool)
    item <- return(NotepadItem img txt itemval itemname itemsel msgQ)
    foldl (>>=) (return item) cnf
    interactor(inside img notepad item)
    addItemToState notepad item
    return item
  where inside :: ImageItem -> Notepad a -> NotepadItem a ->
                  InterActor -> IA ()
        inside img notepad@(Notepad _ _ _ _ entereditemref) item iact =
             (mouseEnter img >>> (debugMsg "entered item" >>
                                  setVar entereditemref (Just item)))
          +> (mouseLeave img >>> (debugMsg "left item" >>
                                  setVar entereditemref Nothing))


-- instances --

instance Eq (NotepadItem a) where
  (NotepadItem img1 _ _ _ _ _) == (NotepadItem img2 _ _ _ _ _) = img1 == img2

instance GUIObject (NotepadItem a) where
  toGUIObject (NotepadItem img _ _ _ _ _) = toGUIObject img
  cname _ = "NotepadItem"

instance HasPosition (NotepadItem a) where
  position p@(x, y) n@(NotepadItem img txt _ _ _ _) =
    itemPositionD2 p img >> itemPositionD2 (x, y + 35) txt >> return n
  getPosition (NotepadItem img _ _ _ _ _) = getItemPositionD2 img

instance HasPhoto(NotepadItem a) where
  photo i item@(NotepadItem img _ _ _ _ _) = img # photo i >> return item
  getPhoto (NotepadItem img _ _ _ _ _) = getPhoto img

instance Interactive (NotepadItem a)

instance Destructible (NotepadItem a) where
  destroy   = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

data ItemName = ItemName { short :: Int -> String,
	                   full  :: String }

name :: ItemName -> Config (NotepadItem a)
name itname w@(NotepadItem _ txt _ nm _ _) =
  do
    setVar nm itname
    txt # value (full itname)
    return w

getName :: NotepadItem a -> (IO ItemName)
getName (NotepadItem _ _ _ nm _ _) =
  do
    itemname <- getVar nm
    return itemname


-- events --

-- dropEvent :: NotepadItem a -> IA [(NotepadItem a)]

selectionEvent :: NotepadItem a -> IA (Bool)
selectionEvent (NotepadItem _ _ _ _ _ msgQ) = lift(receive msgQ)

{-
selectionEvent (NotepadItem img _ _ _ _ _) =
  mouseButtonPress img 1 >>>= \ _ -> return ()
-}


-------------
-- Notepad --
-------------

-- type --

data Notepad a =
  Notepad Canvas (Maybe (ScrollBox Canvas)) (RVar ([NotepadItem a]))
          (RVar ([NotepadItem a])) (RVar (Maybe (NotepadItem a))) deriving Eq


-- state --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad@(Notepad _ _ notepaditemsref _ _) item =
  do
    notepaditems <- getVar notepaditemsref
    setVar notepaditemsref (item : notepaditems)

highlight :: Canvas -> NotepadItem a -> IO ()
highlight cnv item@(NotepadItem img txt _ _ sel _) =
  do
    txt # filling "white"
    s <- getVar sel
    case s of
      Nothing -> do
                   (x,y) <- getPosition item
                   rect1 <- newRectangle [parent cnv, filling "grey",
                                          outline "grey",
                                          coord [(x - 27, y - 30),
                                                 (x + 27, y + 25)]]
                   putItemAtBottom rect1
                   rect2 <- newRectangle [parent cnv, filling "grey",
                                          outline "grey",
                                          coord [(x - 40, y + 25),
                                                 (x + 40, y + 45)]]
                   putItemAtBottom rect2
                   setVar sel (Just(rect1, rect2))
      Just _  -> done

deHighlight :: NotepadItem a -> IO ()
deHighlight (NotepadItem img txt _ _ sel _) =
  do
    txt # filling "black"
    s <- getVar sel
    case s of
      Nothing             -> done
      Just (rect1, rect2) ->
        destroy rect1 >> destroy rect2 >> setVar sel Nothing

selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem notepad@(Notepad cnv _ _ selecteditemsref _)
           item@(NotepadItem _ _ _ _ _ msgQ) =
  do
    deselectAll notepad
    highlight cnv item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (item : selecteditems)
    sendIO msgQ True

selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem (Notepad cnv _ _ selecteditemsref msgQ) item =
  do
    highlight cnv item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (item : selecteditems)

deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem (Notepad _ _ _ selecteditemsref msgQ) item =
  do
    deHighlight item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (filter ((==) item) selecteditems)

selectAll :: Notepad a -> IO ()
selectAll (Notepad cnv _ notepaditemsref selecteditemsref _) =
  do
    notepaditems <- getVar notepaditemsref
    mapM (highlight cnv) notepaditems
    setVar selecteditemsref notepaditems

deselectAll :: Notepad a -> IO ()
deselectAll notepad@(Notepad _ _ _ selecteditemsref _) =
  do
    selecteditems <- getVar selecteditemsref
    mapM deHighlight selecteditems
    setVar selecteditemsref []

deleteItem :: Notepad a -> NotepadItem a -> IO ()
deleteItem (Notepad _ _ notepaditemsref selecteditemsref _) item =
  do
    notepaditems <- getVar notepaditemsref
    selecteditems <- getVar selecteditemsref
    setVar notepaditemsref (filter ((==) item) notepaditems)
    setVar selecteditemsref (filter ((==) item) selecteditems)
    destroy item

isSelected :: Notepad a -> NotepadItem a -> IO Bool
isSelected (Notepad _ _ _ selecteditemsref _) item =
  do
    selecteditems <- getVar selecteditemsref
    return (any ((==) item) selecteditems)

selectItemsWithin :: Position -> Position -> Notepad a -> IO ()
selectItemsWithin (x0, y0) (x1, y1)
                  notepad@(Notepad _ _ notepaditemsref selecteditemsref _) =
  do
    notepaditems <- getVar notepaditemsref
    (let within :: Position -> Bool
         within (x, y)  =
           ((x0 <= x && x <= x1) || (x1 <= x && x <= x0)) &&
           ((y0 <= y && y <= y1) || (y1 <= y && y <= y0))
     in mapM (\ item -> do
                          pos <- getPosition item
                          (if within pos then
                             selectAnotherItem notepad item
                           else
                             done))
             notepaditems
        >> done)

getSelectedItems :: Notepad a -> IO [NotepadItem a]
getSelectedItems notepad@(Notepad _ _ _ selecteditemsref _) =
  do
    selecteditems <- getVar selecteditemsref
    return selecteditems


-- constructor --

newNotepad :: Bool -> [Config (Notepad a)] -> IO (Notepad a)
newNotepad scrolled cnf =
  do
    notepaditemsref <- newRVar []
    selecteditemsref <- newRVar []
    entereditemref <- newRVar Nothing
    cnv <- newCanvas []
    notepad <- if scrolled then
                 do
                   scrollbox <- newScrollBox cnv []
                   return (Notepad cnv (Just scrollbox) notepaditemsref
                                   selecteditemsref entereditemref)
               else
                 return (Notepad cnv Nothing notepaditemsref selecteditemsref
                                 entereditemref)
    interactor (click notepad)
    foldl (>>=) (return notepad) cnf
    return notepad
  where click :: Notepad a -> InterActor -> IA ()
        click notepad@(Notepad cnv _ _ _ entereditemref) iact =
            (mouseButtonPress cnv 1 >>>=
             \ (x, y) -> do
                           entereditem <- getVar entereditemref
                           case entereditem of
                             Nothing -> do
                                          deselectAll notepad
                                          rect <- newRectangle [coord [(x, y),
                                                                       (x, y)],
                                                                parent cnv]
                                          debugMsg "rectangle created"
                                          become iact (selecting notepad rect
                                                                 x y x y iact)
                             Just item@(NotepadItem img _ _ _ _ _) ->
                               do
                                 b <- isSelected notepad item
                                 (if b then
                                    do
                                      tag <- createTagFromSelection notepad
                                      become iact (moving notepad tag x y iact)
                                  else
                                    selectItem notepad item >>
                                    debugMsg "item selected"))
         +> (mouseEvent cnv (Shift, ButtonPress (Just 1)) >>>=
             \ (x, y) -> do
                           entereditem <- getVar entereditemref
                           case entereditem of
                             Nothing -> done
                             Just item -> do
                                            b <- isSelected notepad item
                                            (if b then
                                               do
                                                 deselectItem notepad item
                                                 debugMsg "item deselected"
                                             else
                                               do
                                                 selectAnotherItem notepad item
                                                 debugMsg
                                                   "item added to selection"))

        selecting :: Notepad a -> Rectangle -> Distance -> Distance ->
                     Distance -> Distance -> InterActor -> IA ()
        selecting notepad@(Notepad cnv _ _ _ _) rect x0 y0 x1 y1 iact =
             (mouseEvent cnv (Button1, Motion) >>>=
              \ ((x, y), _)-> do
                                rect # coord [(x0,y0),(x,y)]
                                debugMsg "resize"
-- leider zu langsam            selectItemsWithin (x0, y0) (x1, y1) notepad
                                become iact (selecting notepad rect x0 y0 x y
                                                       iact))
          +> (mouseEvent cnv (ButtonRelease Nothing) >>>
                do
                  destroy rect
                  selectItemsWithin (x0, y0) (x1, y1) notepad
                  become iact (click notepad iact))

        addToTag :: CanvasTag -> NotepadItem a -> IO ()
        addToTag tag (NotepadItem img txt _ _ rectref _) =
          do
            img # tags [tag]
            txt # tags [tag]
            rects <- getVar rectref
            case rects of
              Nothing            -> done
              Just(rect1, rect2) -> do
                                      rect1 # tags [tag]
                                      rect2 # tags [tag]
                                      done

        createTagFromSelection :: Notepad a -> IO CanvasTag
        createTagFromSelection notepad@(Notepad cnv _ notepaditemsref
                                                selecteditemsref _) =
          do
            notepaditems <- getVar notepaditemsref
            selecteditems <- getVar selecteditemsref
            tag <- newCanvasTag [parent cnv]
            mapM (addToTag tag) selecteditems >> done
            debugMsg "CanvasTag created"
            return tag

        moving :: Notepad a -> CanvasTag -> Distance -> Distance ->
                  InterActor -> IA ()
        moving notepad@(Notepad cnv _ _ _ _) tag x0 y0 iact =
             (mouseEvent cnv (Button1, Motion) >>>=
              \ ((x, y), _) -> do
                                 debugMsg ("moving:  dx = " ++ show (x - x0) ++
                                           "   dy = " ++ show (y - y0))
                                 moveItem tag (x - x0) (y - y0)
                                 debugMsg "moved"
                                 become iact (moving notepad tag x y iact))
          +> (mouseEvent cnv (ButtonRelease Nothing) >>>
              ( {- destroy tag >>     was stattdessen ??? -}
               become iact (click notepad iact)))


-- instances --

instance GUIObject (Notepad a) where
  toGUIObject (Notepad cnv scr _ _ _) = case scr of
                                          Nothing  -> toGUIObject cnv
                                          Just box -> toGUIObject box
  cname _ = "Notepad"

instance Destructible (Notepad a) where
  destroy   = destroy . toGUIObject
  destroyed = destroyed . toGUIObject

instance Interactive (Notepad a)

instance Widget (Notepad a)

instance ChildWidget (Notepad a)

instance Synchronized (Notepad a) where
  synchronize w = synchronize (toGUIObject w)

instance HasBorder (Notepad a)

instance HasColour (Notepad a) where
  legalColourID (Notepad cnv _ _ _ _) = hasBackGroundColour cnv
  setColour notepad@(Notepad cnv _ _ _ _) cid col =
    setColour cnv cid col >> return notepad
  getColour (Notepad cnv _ _ _ _) cid = getColour cnv cid

instance HasSize (Notepad a) where
  width s notepad@(Notepad cnv _ _ _ _)  = cnv # width s >> return notepad
  getWidth (Notepad cnv _ _ _ _) = getWidth cnv
  height s notepad@(Notepad cnv _ _ _ _) = cnv # height s >> return notepad
  getHeight (Notepad cnv _ _ _ _) = getHeight cnv

instance HasScroller (Notepad a)

instance ParentWidget (Notepad a) (NotepadItem a)


-- state export --

--getItems :: Notepad a -> IO [Config (NotepadItem a)]   ???
