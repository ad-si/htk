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

module Notepad (

  Notepad,
  NotepadItem,

  newNotepad,
  createNotepadItem,
  getItemValue,

  ScrollType(..),

  module Name,
  setName,

  selectAll,
  deselectAll,
  selectItem,
  selectAnotherItem,
  selectItemsWithin,
  deselectItem,
  getSelectedItems,
  deleteItem,
  clearNotepad,
  undoLastMotion,

  npScrollRegion,

  bindNotepadEv, {- :: Notepad a -> IO (Event (NotepadEvent a), IO ())  -}
  NotepadEvent(..),

  NotepadExportItem(..),
  NotepadState,
  exportNotepadState,
  importNotepadState,

  module CItem

) where

import HTk
import CanvasItemAux
import ScrollBox
import ReferenceVariables
import Name
import Examples(watch)
import Core
import Maybe
import CItem

getCoords :: EventInfo -> IO (Distance, Distance)
getCoords eventInfo = return (x eventInfo, y eventInfo)


-------------------
-- Notepad items --
-------------------

-- type --

data NotepadItem a =
  NotepadItem ImageItem                                           -- image
              Size                                        -- size of image
              TextItem                                   -- displayed name
              (Ref a)                                             -- value
              (Ref (Maybe (Rectangle, Rectangle)))       -- bg if selected


-- constructor --

createNotepadItem :: CItem c => c -> Notepad c ->
                                [Config (NotepadItem c)] ->
                                IO (NotepadItem c)
createNotepadItem val notepad cnf =
  do
    pho <- getIcon val
    img <- createImageItem (canvas notepad) [photo pho]
    let (Distance dx, _) = img_size notepad
        len = div (dx + 80) 5
    nm <- getName val
    txt <- createTextItem (canvas notepad) [font (Helvetica, 10 :: Int),
                                            text (short nm len)]
    itemval <- newRef val
    itemsel <- newRef Nothing
    let item = NotepadItem img (img_size notepad) txt itemval itemsel
    foldl (>>=) (return item) cnf

    (enter1, _) <- bindSimple img Enter
    (leave1, _) <- bindSimple img Leave
    (enter2, _) <- bindSimple txt Enter
    (leave2, _) <- bindSimple txt Leave

    let listenItem :: Event ()
        listenItem =
             (enter1 >> always
                          (setRef (entered_item notepad) (Just item)))
          +> (leave1 >> always (setRef (entered_item notepad) Nothing))
          +> (enter2 >> always
                          (setRef (entered_item notepad) (Just item)))
          +> (leave2 >> always (setRef (entered_item notepad) Nothing))

    spawnEvent (forever listenItem)          -- TD: stop threads !!!

    addItemToState notepad item
    return item

getItemValue :: NotepadItem a -> IO a
getItemValue (NotepadItem _ _ _ valref _) = getRef valref


-- instances --

instance Eq (NotepadItem a) where
  (NotepadItem img1 _ _ _ _) == (NotepadItem img2 _ _ _ _) =
    img1 == img2

instance GUIObject (NotepadItem a) where
  toGUIObject (NotepadItem img _ _ _ _) = toGUIObject img
  cname _ = "NotepadItem"

instance Synchronized (NotepadItem a) where
  synchronize = synchronize . toGUIObject

instance HasPosition (NotepadItem a) where
  position p@(x, y) n@(NotepadItem img imgsize txt _ _) =
    itemPositionD2 p img >>
    let (Distance iwidth, Distance iheight) = imgsize
    in itemPositionD2 (x, y + Distance (div iheight 2 + 7)) txt >>
       return n
  getPosition (NotepadItem img _ _ _ _) = getItemPositionD2 img

{-
instance HasPhoto (NotepadItem a) where
  photo i item@(NotepadItem img _ _ _ _) =
    img # photo i >> return item
  getPhoto (NotepadItem img _ _ _ _) = getPhoto img
-}

instance Destroyable (NotepadItem a) where
  destroy item@(NotepadItem img _ txt _ itemsel) =
    do
      destroy img
      destroy txt
      mrects <- getRef itemsel
      case mrects of
        Just (rect1, rect2) -> destroy rect1 >> destroy rect2
        _ -> done

setName :: CItem c => NotepadItem c -> Name -> IO ()
setName item@(NotepadItem _ (Distance dx, _) txt _ _) nm =
  do
    let len = div (dx + 80) 5
    txt # text (short nm len)
    done

{-
name :: Name -> Config (NotepadItem a)
name itname w@(NotepadItem _ (Distance dx, _) txt _ nm _) =
  do
    setRef nm itname
    let len = div (dx + 80) 5
    txt # text {- (full itname) -} (short itname len)
    return w
-}

{-
getName :: NotepadItem a -> (IO Name)
getName (NotepadItem _ _ _ _ nm _) =
  do
    itemname <- getRef nm
    return itemname
-}

--------------------------------------------------------------------------
-- notepad events
--------------------------------------------------------------------------

bindNotepadEv :: Notepad a -> IO (Event (NotepadEvent a), IO ())
bindNotepadEv np =
  do
    ch <- newChannel
    setRef (event_queue np) (Just ch)
    return (receive ch, setRef (event_queue np) Nothing)

data NotepadEvent a =
    Dropped (NotepadItem a, [NotepadItem a])
  | Selected (NotepadItem a)
  | Deselected (NotepadItem a)
  | Doubleclick (NotepadItem a)
  | Rightclick [NotepadItem a]
  | Release EventInfo                -- needed for use with GenGUI

sendEv :: Notepad a -> NotepadEvent a -> IO ()
sendEv np ev =
  do
    mch <- getRef (event_queue np)
    case mch of
      Just ch -> syncNoWait (send ch ev)
      _ -> done


--------------------------------------------------------------------------
-- Notepad type
--------------------------------------------------------------------------

data Notepad a =
  Notepad { -- main canvas widget
            canvas :: Canvas,

            -- scrollbox if scrolled
            scrollbox :: Maybe (ScrollBox Canvas),

            -- size of item images
            img_size :: Size,

            -- contained items
            items :: Ref ([NotepadItem a]),

            -- selected items
            selected_items :: Ref ([NotepadItem a]),

            -- entered item (mouse over item)
            entered_item :: Ref (Maybe (NotepadItem a)),

            -- undo last motion action (needed for drag and drop with
            --                          other widgets)
            undo_last_motion :: Ref UndoMotion,

            -- entered item when other items dragged /
            -- rectangles (highlight)
            drop_item :: (Ref (Maybe (NotepadItem a, Rectangle,
                                      Rectangle))),

            -- event queue
            event_queue :: Ref (Maybe (Channel (NotepadEvent a))) }

data ScrollType = Scrolled | NotScrolled deriving Eq

data UndoMotion = ToPerform (IO ()) | Performed


-- state --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad item =
  do
    notepaditems <- getRef (items notepad)
    setRef (items notepad) (item : notepaditems)

highlight :: Canvas -> NotepadItem a -> IO ()
highlight cnv item@(NotepadItem img imgsize txt _ sel) =
  do
    let (Distance iwidth, Distance iheight) = imgsize
    txt # filling "white"
    s <- getRef sel
    case s of
      Nothing -> do
                   (x, y) <- getPosition item
                   rect1 <- createRectangle cnv
                              [filling "blue", outline "blue",
                               coord
                                 [(x - Distance (div iwidth 2 + 1),
                                   y - Distance (div iheight 2 + 1)),
                                  (x + Distance (div iwidth 2),
                                   y + Distance (div iheight 2 + 1))]]
                   putItemAtBottom rect1
                   rect2 <- createRectangle cnv
                              [filling "blue", outline "blue",
                               coord
                                 [(x - Distance
                                         (max (div iwidth 2 + 30) 40),
                                   y + Distance (div iheight 2 + 1)),
                                  (x + Distance
                                         (max (div iwidth 2 + 30) 40),
                                   y + Distance (div iheight 2 + 14))]]
                   putItemAtBottom rect2
                   setRef sel (Just (rect1, rect2))
      Just _  -> done

deHighlight :: NotepadItem a -> IO ()
deHighlight item@(NotepadItem img _ txt _ sel) =
  do
    txt # filling "black"
    s <- getRef sel
    case s of
      Nothing             -> done
      Just (rect1, rect2) ->
        destroy rect1 >> destroy rect2 >> setRef sel Nothing

selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem np item =
  do
    deselectAll np
    highlight (canvas np) item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (item : selecteditems)
    sendEv np (Selected item)

selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem np item =
  do
    highlight (canvas np) item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (item : selecteditems)
    sendEv np (Selected item)

deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem np item =
  do
    deHighlight item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (filter ((/=) item) selecteditems)
    sendEv np (Deselected item)

selectAll :: Notepad a -> IO ()
selectAll np =
  do
    notepaditems <- getRef (items np)
    mapM (highlight (canvas np)) notepaditems
    mapM (\item -> do
                     b <- Notepad.isSelected np item
                     if b then done else sendEv np (Selected item))
         notepaditems
    setRef (selected_items np) notepaditems

deselectAll :: Notepad a -> IO ()
deselectAll np =
  do
    notepaditems <- getRef (items np)
    selecteditems <- getRef (selected_items np)
    mapM deHighlight selecteditems
    mapM (\item -> do
                     b <- Notepad.isSelected np item
                     if b then sendEv np (Deselected item) else done)
         notepaditems
    setRef (selected_items np) []

deleteItem :: Notepad a -> NotepadItem a -> IO ()
deleteItem np item =
  do
    notepaditems <- getRef (items np)
    selecteditems <- getRef (selected_items np)
    setRef (items np) (filter ((==) item) notepaditems)
    setRef (selected_items np) (filter ((==) item) selecteditems)
    destroy item

clearNotepad :: Notepad a -> IO ()
clearNotepad np =
  do
    notepaditems <- getRef (items np)
    mapM destroy notepaditems
    setRef (items np) []
    setRef (selected_items np) []

undoLastMotion :: Notepad a -> IO ()
undoLastMotion np =
  synchronize np (do
                    act <- getRef (undo_last_motion np)
                    case act of
                      ToPerform act' -> setRef (undo_last_motion np)
                                               Performed >>
                                        act'
                      _ -> done)

isSelected :: Notepad a -> NotepadItem a -> IO Bool
isSelected np item =
  do
    selecteditems <- getRef (selected_items np)
    return (any ((==) item) selecteditems)

selectItemsWithin :: Position -> Position -> Notepad a -> IO ()
selectItemsWithin (x0, y0) (x1, y1) np =
  do
    notepaditems <- getRef (items np)
    let within :: Position -> Bool
        within (x, y)  =
          ((x0 <= x && x <= x1) || (x1 <= x && x <= x0)) &&
          ((y0 <= y && y <= y1) || (y1 <= y && y <= y0))
    mapM (\ item -> do
                      pos <- getPosition item
                      if within pos then
                        selectAnotherItem np item
                        else do
                               b <- Notepad.isSelected np item
                               if b then done
                                 else deselectItem np item)
         notepaditems
    done

getSelectedItems :: Notepad a -> IO [NotepadItem a]
getSelectedItems np =
  do
    selecteditems <- getRef (selected_items np)
    return selecteditems


--------------------------------------------------------------------------
-- notepad construction
--------------------------------------------------------------------------

newNotepad :: (CItem c, Container par) =>
              par -> ScrollType -> Size -> Maybe (NotepadState c) ->
              [Config (Notepad c)] -> IO (Notepad c)
newNotepad par scrolltype imgsize mstate cnf =
  do
    let scrolled = (scrolltype == Scrolled)
    notepaditemsref <- newRef []
    selecteditemsref <- newRef []
    entereditemref <- newRef Nothing
    dropref <- newRef Nothing
    ulm <- newRef Performed
    evq <- newRef Nothing
    (cnv, notepad) <- if scrolled then
                        do
                          (scrollbox, cnv) <-
                            newScrollBox par (\p -> newCanvas p []) []
                          return (cnv,
                                  Notepad { canvas = cnv,
                                            scrollbox = Just scrollbox,
                                            img_size = imgsize,
                                            items = notepaditemsref,
                                            selected_items =
                                              selecteditemsref,
                                            entered_item = entereditemref,
                                            drop_item = dropref,
                                            event_queue = evq,
                                            undo_last_motion = ulm })
                      else
                        do
                          cnv <- newCanvas par []
                          return (cnv,
                                  Notepad { canvas = cnv,
                                            scrollbox = Nothing,
                                            img_size = imgsize,
                                            items = notepaditemsref,
                                            selected_items =
                                              selecteditemsref,
                                            entered_item = entereditemref,
                                            drop_item = dropref,
                                            event_queue = evq,
                                            undo_last_motion = ulm })

    (click, _) <- bind cnv [WishEvent [] (ButtonPress (Just (BNo 1)))]
    (rightclick, _) <- bind cnv
                            [WishEvent [] (ButtonPress (Just (BNo 2)))]
    (clickmotion', _) <- bind cnv [WishEvent [Button1] Motion]
    (clickmotion, _) <- Examples.watch clickmotion'
    (doubleclick, _) <- bind cnv [WishEvent [Double]
                                            (ButtonPress (Just (BNo 1)))]
    (shiftclick, _) <- bind cnv [WishEvent [Shift]
                                           (ButtonPress (Just (BNo 1)))]
    (release, _) <- bind cnv [WishEvent [] (ButtonRelease (Just (BNo 1)))]

    let addToTag :: CanvasTag -> NotepadItem a -> IO ()
        addToTag tag (NotepadItem img _ txt _ rectref) =
          do
            img # tags [tag]
            txt # tags [tag]
            rects <- getRef rectref
            case rects of
              Nothing            -> done
              Just(rect1, rect2) -> do
                                      rect1 # tags [tag]
                                      rect2 # tags [tag]
                                      done

        createTagFromSelection :: Notepad a -> IO CanvasTag
        createTagFromSelection notepad =
          do
            notepaditems <- getRef (items notepad)
            selecteditems <- getRef (selected_items notepad)
            tag <- createCanvasTag (canvas notepad) []
            mapM (addToTag tag) selecteditems
            return tag

        checkDropZones :: Notepad a -> Distance -> Distance -> IO ()
        checkDropZones notepad x y =
          let doSet item@(NotepadItem _ imgsize _ _ _)=
                do
                  (x, y) <- getPosition item
                  let (Distance iwidth, Distance iheight) = imgsize
                  rect1 <- createRectangle (canvas notepad)
                             [filling "yellow", outline "yellow",
                              coord [(x - Distance (div iwidth 2 + 1),
                                      y - Distance (div iheight 2 + 1)),
                                     (x + Distance (div iwidth 2),
                                      y + Distance (div iheight 2 + 1))]]
                  putItemAtBottom rect1
                  rect2 <- createRectangle (canvas notepad)
                             [filling "yellow", outline "yellow",
                              coord
                                [(x - Distance
                                        (max (div iwidth 2 + 30) 40),
                                  y + Distance (div iheight 2 + 1)),
                                 (x + Distance
                                        (max (div iwidth 2 + 30) 40),
                                  y + Distance (div iheight 2 + 14))]]
                  putItemAtBottom rect2
                  setRef (drop_item notepad) (Just (item, rect1, rect2))

              setDropRef item =
                do
                   drop <- getRef (drop_item notepad)
                   case drop of
                     Nothing -> doSet item
                     Just (ditem, rect1, rect2) ->
                       if item == ditem then done
                       else destroy rect1 >> destroy rect2 >> doSet item

              inDropZone (NotepadItem img _ _ _ _) =
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
                  maybeitem <- getRef (drop_item notepad)
                  case maybeitem of
                    Just (_, rect1, rect2) ->
                      destroy rect1 >> destroy rect2 >>
                      setRef (drop_item notepad) Nothing
                    Nothing -> done
          in do
               notepaditems <- getRef (items notepad)
               selecteditems <- getRef (selected_items notepad)
               let nonselecteditems =
                     filter (\item -> not(any ((==) item) selecteditems))
                            notepaditems
               checkDropZones' nonselecteditems

        selectByRectangle :: Position -> Rectangle -> Event ()
        selectByRectangle pos@(x, y) rect =
             (do
                (x1, y1) <- clickmotion >>>= getCoords
                always (rect # coord [(x, y), (x1, y1)])
--                always (selectItemsWithin (x, y) (x1, y1) notepad)
                                                   -- too slow actually
                selectByRectangle pos rect)
          +> (do
                ev_inf <- release
                always (do
                          (x1,y1) <- getCoords ev_inf
                          sendEv notepad (Release ev_inf)
                          selectItemsWithin (x, y) (x1, y1) notepad
                          destroy rect))

        -- bug: release für selectByRect wird von anderem noch laufenden
        --      Thread empfangen ???  Nur eine Idee !

        moveSelectedItems :: Position -> Position -> CanvasTag ->
                             Event ()
        moveSelectedItems rpos@(rootx, rooty) (x0, y0) t =
             (do
                (x, y) <- clickmotion >>>= getCoords
                always (checkDropZones notepad x y >>
                        setRef (undo_last_motion notepad)
                               (ToPerform (moveItem t (rootx - x0)
                                                      (rooty - y0))) >>
                        moveItem t (x - x0) (y - y0))
                moveSelectedItems rpos (x, y) t)
          +> (release >>
              always (do
                        drop <- getRef dropref
                        case drop of
                          Just (item, rect1, rect2) ->
                            do
                              act <- getRef (undo_last_motion notepad)
                              case act of
                                Performed -> done
                                _ -> do
                                       undoLastMotion notepad
                                       selecteditems <-
                                         getRef selecteditemsref
                                       sendEv notepad
                                              (Dropped (item,
                                                        selecteditems))
                              setRef dropref Nothing
                              destroy rect1 
                              destroy rect2
                          _ -> done))

        listenNotepad :: Event ()
        listenNotepad =
             (do
                 (x, y) <- click >>>= getCoords
                 always
                   (do
                      entereditem <- getRef entereditemref
                      case entereditem of
                        Nothing -> do
                                     deselectAll notepad
                                     rect <- createRectangle cnv
                                               [coord [(x, y), (x, y)]]
                                     spawnEvent
                                       (selectByRectangle (x, y) rect)
                                     done
                        Just item@(NotepadItem img _ _ _ _) ->
                          do
                            b <- Notepad.isSelected notepad item
                            if b then done else selectItem notepad item
                            t <- createTagFromSelection notepad
                            spawnEvent (moveSelectedItems (x, y) (x, y) t)
                            done)
          +> (do
                (x, y) <- rightclick >>>= getCoords
                always
                  (do
                     entereditem <- getRef entereditemref
                     case entereditem of
                       Nothing -> do
                                    deselectAll notepad
                                    sendEv notepad (Rightclick [])
                       Just entereditem ->
                         do
                           b <- Notepad.isSelected notepad
                                                       entereditem
                           (if b then
                              do
                                selecteditems <- getRef selecteditemsref
                                sendEv notepad (Rightclick selecteditems)
                            else
                              do
                                selectItem notepad entereditem
                                sendEv notepad
                                       (Rightclick [entereditem]))))
          +> (doubleclick >> always (do
                                       entereditem <-
                                         getRef entereditemref
                                       case entereditem of
                                         Just item ->
                                           sendEv notepad
                                                  (Doubleclick item)
                                         _ -> done))
          +> (shiftclick >> always (do
                                      entereditem <- getRef entereditemref
                                      case entereditem of
                                        Just item ->
                                          do
                                            b <- Notepad.isSelected
                                                   notepad item
                                            if b then deselectItem notepad
                                                        item
                                              else selectAnotherItem
                                                     notepad item
                                        _ -> done)))

    -- TD: >>> statt always

    spawnEvent (forever listenNotepad)
    foldl (>>=) (return notepad) cnf
    case mstate of
      Just state -> importNotepadState notepad state
      _ -> done
    return notepad


-- instances --

instance GUIObject (Notepad a) where
  toGUIObject np =
    case (scrollbox np) of Nothing -> toGUIObject (canvas np)
                           Just box -> toGUIObject box
  cname _ = "Notepad"

instance Destroyable (Notepad a) where
  destroy = destroy . toGUIObject          -- TD : clean up !!!

instance Widget (Notepad a)

instance Synchronized (Notepad a) where
  synchronize w = synchronize (toGUIObject w)

instance HasBorder (Notepad a)

instance HasColour (Notepad a) where
  legalColourID np = hasBackGroundColour (canvas np)
  setColour notepad cid col =
    setColour (canvas notepad) cid col >> return notepad
  getColour np cid = getColour (canvas np) cid

instance HasSize (Notepad a) where
  width s np = canvas np # width s >> return np
  getWidth np = getWidth (canvas np)
  height s np = canvas np # height s >> return np
  getHeight np = getHeight (canvas np)


-- -----------------------------------------------------------------------
-- notepad specific configuration options
-- -----------------------------------------------------------------------

npScrollRegion :: ScrollRegion -> Config (Notepad a)
npScrollRegion scr np = canvas np # scrollRegion scr >> return np


-- -----------------------------------------------------------------------
-- state import / export
-- -----------------------------------------------------------------------

data CItem c => NotepadExportItem c =
  NotepadExportItem { val :: c,
                      pos :: Position,
                      selected :: Bool }

type NotepadState c = CItem c => [NotepadExportItem c]

exportNotepadState :: CItem c => Notepad c -> IO (NotepadState c)
exportNotepadState np =
  synchronize np (do
                    items' <- getRef (items np)
                    exportNotepadState' np items')
  where exportNotepadState' :: CItem c => Notepad c -> [NotepadItem c] ->
                               IO (NotepadState c)
        exportNotepadState' np (item@(NotepadItem img _ txt val _) :
                                items) =
          do
            val' <- getRef val
            pos <- getPosition img
            is_selected <- Notepad.isSelected np item
            rest <- exportNotepadState' np items
            return (NotepadExportItem { val = val',
                                        pos = pos,
                                        selected = is_selected } : rest)
        exportNotepadState' _ _ = return []

importNotepadState :: CItem c => Notepad c -> NotepadState c -> IO ()
importNotepadState np st =
  synchronize np (do
                    clearNotepad np
                    addItems np st)
  where addItems :: CItem c => Notepad c -> NotepadState c -> IO ()
        addItems np (it : items) =
          do
            new_it <- createNotepadItem (val it) np [position (pos it)]
            if selected it then selectAnotherItem np new_it else done
            addItems np items
        addItems _ _ = done
