{- --------------------------------------------------------------------
 -
 - Module DragAndDrop
 -
 - Scrolled canvases with drag and drop support.
 -
 - Author: cxl/ludi
 - $Revision$ from $Date$
 -
 - -------------------------------------------------------------------- -}


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

  dropEvent,         {- NotepadItem a -> IA [(NotepadItem a)] -}
  selectionEvent,    {- NotepadItem a -> IA () -}

) where

import HTk
import Image
import Canvas
import CanvasItemAux
import ImageItem
import TextItem
import Rectangle
import Mouse
import CanvasTag
import ScrollBox
import Concurrency
import Channels
import RVar

debug = True
debugMsg str = if debug then putStrLn(">>> " ++ str) else done


-------------------
-- Notepad items --
-------------------

-- type --

data NotepadItem a =
  NotepadItem ImageItem                                           -- image
              (TextItem String)                          -- displayed name
              (RVar a)                                            -- value
              (RVar ItemName)                                      -- name
              (RVar (Maybe (Rectangle, Rectangle)))      -- bg if selected
              (MsgQueue (Bool))                   -- selection event queue
              (MsgQueue [NotepadItem a])                -- dnd event queue


-- constructor --

newNotepadItem :: a -> Notepad a -> [Config (NotepadItem a)] ->
                  IO (NotepadItem a)
newNotepadItem val notepad@(Notepad cnv _ _ _ _ _) cnf =
  do
    img <- newImageItem [parent cnv]
    txt <- newTextItem [parent cnv]
    itemval <- newRVar val
    itemname <- newRVar (ItemName { short = \_ -> "", full = "" })
    itemsel <- newRVar Nothing
    smsgQ <- newMsgQueue :: IO (MsgQueue Bool)
    dmsgQ <- newMsgQueue :: IO (MsgQueue [NotepadItem a])
    item <- return(NotepadItem img txt itemval itemname itemsel smsgQ
                               dmsgQ)
    foldl (>>=) (return item) cnf
    interactor (inside img notepad item)
    addItemToState notepad item
    return item
  where inside :: ImageItem -> Notepad a -> NotepadItem a ->
                  InterActor -> IA ()
        inside img notepad@(Notepad _ _ _ _ entereditemref _) item iact =
             (mouseEnter img >>> setVar entereditemref (Just item))
          +> (mouseLeave img >>> setVar entereditemref Nothing)


-- instances --

instance Eq (NotepadItem a) where
  (NotepadItem img1 _ _ _ _ _ _) == (NotepadItem img2 _ _ _ _ _ _) =
    img1 == img2

instance GUIObject (NotepadItem a) where
  toGUIObject (NotepadItem img _ _ _ _ _ _) = toGUIObject img
  cname _ = "NotepadItem"
{- Probleme: Nicht einziges Objekt, was hängt alles davon ab ?
             Events sicherlich ein Problem, Destructible könnte man
             irgendwie hinbiegen -}

instance HasPosition (NotepadItem a) where
  position p@(x, y) n@(NotepadItem img txt _ _ _ _ _) =
    itemPositionD2 p img >> itemPositionD2 (x, y + 35) txt >> return n
  getPosition (NotepadItem img _ _ _ _ _ _) = getItemPositionD2 img

instance HasPhoto(NotepadItem a) where
  photo i item@(NotepadItem img _ _ _ _ _ _) =
    img # photo i >> return item
  getPhoto (NotepadItem img _ _ _ _ _ _) = getPhoto img

instance Interactive (NotepadItem a)
{- Probleme: Events (z.B. Enter) hängen nur am Image,
             Lösung wäre evtl. allgemeine Interactive Instanz für
             GUIObjects -}

instance Destructible (NotepadItem a) where
  destroy = destroy . toGUIObject
  destroyed = destroyed . toGUIObject
{- Probleme: Nur Image würde zerstört / destroyed wäre so o.k., wenn
             GUIObject Instanz so ginge,
             items müssten zusätzlich aus dem State gelöscht werden -}

data ItemName = ItemName { short :: Int -> String,
	                   full  :: String }

name :: ItemName -> Config (NotepadItem a)
name itname w@(NotepadItem _ txt _ nm _ _ _) =
  do
    setVar nm itname
    txt # value (full itname)
    return w

getName :: NotepadItem a -> (IO ItemName)
getName (NotepadItem _ _ _ nm _ _ _) =
  do
    itemname <- getVar nm
    return itemname


-- events --

dropEvent :: NotepadItem a -> IA [(NotepadItem a)]
dropEvent (NotepadItem _ _ _ _ _ _ dmsgQ) = lift (receive dmsgQ)

selectionEvent :: NotepadItem a -> IA (Bool)
selectionEvent (NotepadItem _ _ _ _ _ smsgQ _) = lift (receive smsgQ)


-------------
-- Notepad --
-------------

-- type --

data Notepad a =
  Notepad Canvas                                            -- main canvas
          (Maybe (ScrollBox Canvas))                          -- scrollbox
          (RVar ([NotepadItem a]))                      -- contained items
          (RVar ([NotepadItem a]))                       -- selected items
          (RVar (Maybe (NotepadItem a)))                   -- entered item
          (RVar (Maybe (NotepadItem a, Rectangle, Rectangle)))
                             -- entered item when other items dragged / bg
  deriving Eq


-- state --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad@(Notepad _ _ notepaditemsref _ _ _) item =
  do
    notepaditems <- getVar notepaditemsref
    setVar notepaditemsref (item : notepaditems)

highlight :: Canvas -> NotepadItem a -> IO ()
highlight cnv item@(NotepadItem img txt _ _ sel _ _) =
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
deHighlight (NotepadItem img txt _ _ sel _ _) =
  do
    txt # filling "black"
    s <- getVar sel
    case s of
      Nothing             -> done
      Just (rect1, rect2) ->
        destroy rect1 >> destroy rect2 >> setVar sel Nothing

selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem notepad@(Notepad cnv _ _ selecteditemsref _ _)
           item@(NotepadItem _ _ _ _ _ smsgQ _) =
  do
    deselectAll notepad
    highlight cnv item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (item : selecteditems)
    sendIO smsgQ True

selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem (Notepad cnv _ _ selecteditemsref _ _)
                  item@(NotepadItem _ _ _ _ _ smsgQ _) =
  do
    highlight cnv item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (item : selecteditems)
    sendIO smsgQ True

deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem (Notepad _ _ _ selecteditemsref _ _)
             item@(NotepadItem _ _ _ _ _ smsgQ _) =
  do
    deHighlight item
    selecteditems <- getVar selecteditemsref
    setVar selecteditemsref (filter ((==) item) selecteditems)
    sendIO smsgQ False

selectAll :: Notepad a -> IO ()
selectAll notepad@(Notepad cnv _ notepaditemsref selecteditemsref _ _) =
  do
    notepaditems <- getVar notepaditemsref
    mapM (highlight cnv) notepaditems
    mapM (\ item@(NotepadItem _ _ _ _ _ smsgQ _) ->
            do
              b <- isSelected notepad item
              if b then done else sendIO smsgQ True) notepaditems
    setVar selecteditemsref notepaditems

deselectAll :: Notepad a -> IO ()
deselectAll notepad@(Notepad _ _ notepaditemsref selecteditemsref _ _) =
  do
    notepaditems <- getVar notepaditemsref
    selecteditems <- getVar selecteditemsref
    mapM deHighlight selecteditems
    mapM (\ item@(NotepadItem _ _ _ _ _ smsgQ _) ->
            do
              b <- isSelected notepad item
              if b then sendIO smsgQ False else done) notepaditems
    setVar selecteditemsref []
    done

deleteItem :: Notepad a -> NotepadItem a -> IO ()
deleteItem (Notepad _ _ notepaditemsref selecteditemsref _ _) item =
  do
    notepaditems <- getVar notepaditemsref
    selecteditems <- getVar selecteditemsref
    setVar notepaditemsref (filter ((==) item) notepaditems)
    setVar selecteditemsref (filter ((==) item) selecteditems)
    destroy item

isSelected :: Notepad a -> NotepadItem a -> IO Bool
isSelected (Notepad _ _ _ selecteditemsref _ _) item =
  do
    selecteditems <- getVar selecteditemsref
    return (any ((==) item) selecteditems)

selectItemsWithin :: Position -> Position -> Notepad a -> IO ()
selectItemsWithin (x0, y0) (x1, y1)
                  notepad@(Notepad _ _ notepaditemsref selecteditemsref _ 
                                   _) =
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
getSelectedItems notepad@(Notepad _ _ _ selecteditemsref _ _) =
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
    dropref <- newRVar Nothing
    cnv <- newCanvas []
    notepad <- if scrolled then
                 do
                   scrollbox <- newScrollBox cnv []
                   return (Notepad cnv (Just scrollbox) notepaditemsref
                                   selecteditemsref entereditemref
                                   dropref)
               else
                 return (Notepad cnv Nothing notepaditemsref
                                 selecteditemsref entereditemref dropref)
    interactor (click notepad)
    foldl (>>=) (return notepad) cnf
    return notepad
  where click :: Notepad a -> InterActor -> IA ()
        click notepad@(Notepad cnv _ _ _ entereditemref _) iact =
            (mouseButtonPress cnv 1 >>>=
             \ (x, y) -> do
                           entereditem <- getVar entereditemref
                           case entereditem of
                             Nothing -> do
                                          deselectAll notepad
                                          rect <- newRectangle
                                                    [coord [(x, y),
                                                            (x, y)],
                                                     parent cnv]
                                          become iact
                                            (selectingByRectangle
                                               notepad rect x y x y iact)
                             Just item@(NotepadItem img _ _ _ _ _ _) ->
                               do
                                 b <- isSelected notepad item
                                 (if b then done
                                  else selectItem notepad item)
                                 tag <- createTagFromSelection notepad
                                 become iact (moving notepad tag x y x y
                                                     iact))
         +> (mouseEvent cnv (Shift, ButtonPress (Just 1)) >>>=
             \ (x, y) -> do
                           entereditem <- getVar entereditemref
                           case entereditem of
                             Nothing -> done
                             Just item -> do
                                            b <- isSelected notepad item
                                            (if b then
                                               deselectItem notepad item
                                             else
                                               selectAnotherItem notepad
                                                                 item))

        selectingByRectangle :: Notepad a -> Rectangle -> Distance ->
                                Distance -> Distance -> Distance ->
                                InterActor -> IA ()
        selectingByRectangle notepad@(Notepad cnv _ _ _ _ _) rect x0 y0 x1
                             y1 iact =
             (mouseEvent cnv (Button1, Motion) >>>=
              \ ((x, y), _)-> do
                                rect # coord [(x0, y0), (x, y)]
-- leider zu langsam            selectItemsWithin (x0,y0) (x1,y1) notepad
                                become iact (selectingByRectangle notepad
                                               rect x0 y0 x y iact))
          +> (mouseEvent cnv (ButtonRelease Nothing) >>>
                do
                  destroy rect
                  selectItemsWithin (x0, y0) (x1, y1) notepad
                  become iact (click notepad iact))

        addToTag :: CanvasTag -> NotepadItem a -> IO ()
        addToTag tag (NotepadItem img txt _ _ rectref _ _) =
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
                                                selecteditemsref _ _) =
          do
            notepaditems <- getVar notepaditemsref
            selecteditems <- getVar selecteditemsref
            tag <- newCanvasTag [parent cnv]
            mapM (addToTag tag) selecteditems >> done
            return tag

        checkDropZones :: Notepad a -> Distance -> Distance -> IO ()
        checkDropZones notepad@(Notepad cnv _ notepaditemsref
                                        selecteditemsref _ dropref)
                       x y =
          let doSet item =
                do
                  (x,y) <- getPosition item
                  rect1 <- newRectangle [parent cnv, filling "yellow",
                                         outline "yellow",
                                         coord [(x - 27, y - 30),
                                                (x + 27, y + 25)]]
                  putItemAtBottom rect1
                  rect2 <- newRectangle [parent cnv, filling "yellow",
                                         outline "yellow",
                                         coord [(x - 40, y + 25),
                                                (x + 40, y + 45)]]
                  putItemAtBottom rect2
                  setVar dropref (Just (item, rect1, rect2))

              setDropRef item =
                do
                   drop <- getVar dropref
                   (case drop of
                      Nothing -> doSet item
                      Just (ditem, _, _) ->
                        if item == ditem then done else doSet item)

              inDropZone (NotepadItem img _ _ _ _ _ _) =
                do
                  (x_it, y_it) <- getPosition img
                  return (if x_it - 30 < x && x_it + 30 > x &&
                             y_it - 30 < y && y_it + 30 > y then True
                          else False)

              checkDropZones' (item : items) =
                do
                  b <- inDropZone item
                  (if b then setDropRef item else checkDropZones' items)
              checkDropZones' [] =
                do
                  maybeitem <- getVar dropref
                  case maybeitem of
                    Just (_, rect1, rect2) ->
                      destroy rect1 >> destroy rect2 >>
                      setVar dropref Nothing
                    Nothing -> done
          in do
               notepaditems <- getVar notepaditemsref
               debugMsg (show (length notepaditems) ++ " selected items")
               selecteditems <- getVar selecteditemsref
               debugMsg (show (length selecteditems) ++ " selected items")
               (let nonselecteditems =
                      filter (\item -> not(any ((==) item) selecteditems))
                             notepaditems
                in debugMsg (show (length nonselecteditems) ++ " nonselected items") >> checkDropZones' nonselecteditems)

        moving :: Notepad a -> CanvasTag -> Distance -> Distance ->
                  Distance -> Distance -> InterActor -> IA ()
        moving notepad@(Notepad cnv _ _ selecteditemsref _ dropref) tag
               rootx rooty x0 y0 iact =
          (mouseEvent cnv (Button1, Motion) >>>=
             \ ((x, y), _) -> do
                                checkDropZones notepad x y
                                moveItem tag (x - x0) (y - y0)
                                become iact (moving notepad tag rootx
                                                    rooty x y iact))
         +> (mouseEvent cnv (ButtonRelease Nothing) >>>
             (do
                drop <- getVar dropref
                (case drop of
                   Nothing -> done
                   Just (NotepadItem _ _ _ _ _ _ dmsgQ, rect1, rect2) ->
                     do
                       selecteditems <- getVar selecteditemsref
                       moveItem tag (rootx - x0) (rooty - y0)
                       setVar dropref Nothing
                       destroy rect1 
                       destroy rect2
                       sendIO dmsgQ selecteditems)
                {- destroy tag  --   was stattdessen ??? -}
                become iact (click notepad iact)))


-- instances --

instance GUIObject (Notepad a) where
  toGUIObject (Notepad cnv scr _ _ _ _) =
    case scr of Nothing  -> toGUIObject cnv
                Just box -> toGUIObject box
  cname _ = "Notepad"
{- Mögl. Probleme: destroy, Events und was sonst noch von GUIObject
                   Instanz abhängen mag -}

instance Destructible (Notepad a) where
  destroy   = destroy . toGUIObject
  destroyed = destroyed . toGUIObject
{- Probleme: Werden Kinder automatisch mit zerstört ? 
             (z.B. der Canvas und auch die Canvasitems, wenn die Scrollbox
              zerstört wird) -}

instance Interactive (Notepad a)
{- Wäre so o.k., wenn wenn Events nach unten durchgereicht würden.
   Hoffentlich gibt es kein Enter/Leave, wenn man den Cursor über den
   Scrollbar in den Canvas bewegt. Testen! -}

instance Widget (Notepad a)

instance ChildWidget (Notepad a)

instance Synchronized (Notepad a) where
  synchronize w = synchronize (toGUIObject w)

instance HasBorder (Notepad a)

instance HasColour (Notepad a) where
  legalColourID (Notepad cnv _ _ _ _ _) = hasBackGroundColour cnv
  setColour notepad@(Notepad cnv _ _ _ _ _) cid col =
    setColour cnv cid col >> return notepad
  getColour (Notepad cnv _ _ _ _ _) cid = getColour cnv cid

instance HasSize (Notepad a) where
  width s notepad@(Notepad cnv _ _ _ _ _)  =
    cnv # width s >> return notepad
  getWidth (Notepad cnv _ _ _ _ _) = getWidth cnv
  height s notepad@(Notepad cnv _ _ _ _ _) =
    cnv # height s >> return notepad
  getHeight (Notepad cnv _ _ _ _ _) = getHeight cnv

instance ParentWidget (Notepad a) (NotepadItem a)


-- state export --

--getItems :: Notepad a -> IO [Config (NotepadItem a)]               -- TD
