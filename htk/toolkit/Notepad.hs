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

---
-- A simple drag and drop field.
module Notepad (

  Notepad,
  NotepadItem,

  newNotepad,
  createNotepadItem,
  getFreeItemPosition,
  getItemValue,

  ScrollType(..),

  module Name,
  setName,

  updNotepadScrollRegion,
  selectAll,
  deselectAll,
  selectItem,
  selectAnotherItem,
  selectItemsWithin,
  deselectItem,
  getItems,
  getSelectedItems,
  isNotepadItemSelected,
  deleteItem,
  clearNotepad,
  undoLastMotion,

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
import FiniteMap

import IOExts(unsafePerformIO)

getCoords :: EventInfo -> IO (Distance, Distance)
getCoords eventInfo = return (x eventInfo, y eventInfo)

char_px = 8


-------------------
-- Notepad items --
-------------------

-- type

---
-- The <code>NotepadItem</code> datatype.
data NotepadItem a =
  NotepadItem { it_img :: ImageItem,                              -- image
                it_img_size :: Size,                      -- size of image
                it_txt :: TextItem,                      -- displayed name
                it_val :: Ref a,                                  -- value
                it_long_name_bg :: Ref (Maybe Rectangle), -- long names bg
                it_bg :: Ref (Maybe (Rectangle, Rectangle)) }
                                                         -- bg if selected

-- handler for enter events
enteredItem :: CItem c => Notepad c -> NotepadItem c -> IO ()
enteredItem notepad item =
  synchronize item
    (do
       v <- getRef (it_val item)
       nm <- getName v
       let fullnm = full nm
       it_txt item # text fullnm
       mlast_bg <- getRef (it_long_name_bg item)
       case mlast_bg of
         Nothing -> do
                      Just (x1, y1, x2, y2) <-
                        bbox (canvas notepad) (it_txt item)
                      (_, (sizex, _)) <- getScrollRegion (canvas notepad)
                      let dx = if x1 < 0 then -x1 + 6
                               else if x2 > sizex then (sizex - x2)
                                       else 0 
                      moveItem (it_txt item) dx 0
                      b <- isNotepadItemSelected notepad item
                      rect <- createRectangle (canvas notepad)
                                (coord [(x1 - 5 + dx, y1 - 1),
                                        (x2 + 5 + dx, y2 + 1)] :
                                 (if b then [filling "blue",
                                             outline "blue"]
                                  else [filling "white",
                                        outline "black"]))
                      putItemOnTop rect
                      putItemOnTop (it_txt item)
                      setRef (it_long_name_bg item) (Just rect)
         _ -> done
       done)

-- handler for leave events
leftItem :: CItem c => Notepad c -> NotepadItem c -> IO ()
leftItem notepad item =
  synchronize item
    (do
       (x, y) <- getPosition item
       let (Distance iwidth, Distance iheight) = img_size notepad
       it_txt item # position (x, y + Distance (div iheight 2 + 7))
       let (Distance dx, _) = img_size notepad
           len = div (dx + 80) char_px
       v <- getRef (it_val item)
       nm <- getName v
       let shortnm = short nm len
       it_txt item # text shortnm
       mlast_bg <- getRef (it_long_name_bg item)
       case mlast_bg of
         Just last_bg -> destroy last_bg >>
                         setRef (it_long_name_bg item) Nothing
         _ -> done
       done)


-- constructor

---
-- Creates a new notepad item and returns a handler.
-- @param val     - the notepad item's value.
-- @param notepad - the concerned notepad.
-- @param cnf     - the list of configuration options for this notepad
--                - item.
-- @return result - A notepad item.
createNotepadItem :: CItem c => c -> Notepad c ->
                                [Config (NotepadItem c)] ->
                                IO (NotepadItem c)
createNotepadItem val notepad cnf =
  do
    pho <- getIcon val
    img <- createImageItem (canvas notepad) [coord [(-200, -200)],
                                             photo pho]
    let (Distance dx, _) = img_size notepad
        len = div (dx + 80) char_px
    nm <- getName val
    txt <- createTextItem (canvas notepad) [coord [(-200, -200)],
                                            font (Helvetica, 10 :: Int),
                                            text (short nm len)]
    itemval <- newRef val
    itemsel <- newRef Nothing
    lnbg <- newRef Nothing
    let item = NotepadItem { it_img = img,
                             it_img_size = (img_size notepad),
                             it_txt = txt,
                             it_val = itemval,
                             it_long_name_bg = lnbg,
                             it_bg = itemsel }
    foldl (>>=) (return item) cnf


    (entered, _) <- bindSimple item Enter
    (left, _) <- bindSimple item Leave
    spawnEvent (forever ((entered >>>
                            (do putStrLn "entered"
                                last <- getRef (entered_item notepad)
                                if not (isJust last)
                                  then do setRef (entered_item notepad)
                                                 (Just item)
                                          enteredItem notepad item
                                  else if fromJust last /= item
                                         then do leftItem notepad
                                                          (fromJust last)
                                                 setRef
                                                   (entered_item notepad)
                                                   (Just item)
                                                 enteredItem notepad item
                                         else done)) +>
                         (left >>>
                            (do putStrLn "left"
                                last <- getRef (entered_item notepad)
                                setRef (entered_item notepad) Nothing
                                if isJust last
                                  then leftItem notepad (fromJust last)
                                  else done))))
    addItemToState notepad item
    return item

---
-- Returns a free item position on the notepad.
-- @param notepad - the concerned notepad.
-- @return result - the free position on the notepad.
getFreeItemPosition :: CItem c => Notepad c -> IO Position
getFreeItemPosition notepad =
   let num_cols = 4
       (Distance iwidth, Distance iheight) = img_size notepad
       dy_n = Distance (div iheight 2)
       dy_s = Distance (div iheight 2 + 14)
       dx = Distance (max (div iwidth 2) 40)

       overlaps (x, y) (item : items) =
         do
           (ix, iy) <- getPosition item
           (if (( (x - dx   >= ix - dx   && x - dx   <= ix + dx)   ||
                  (x + dx   >  ix - dx   && x + dx   <  ix + dx)     ) &&
                ( (y - dy_n >= iy - dy_n && y - dy_n <= iy + dy_s) ||
                  (y + dy_s >  iy - dy_n && y + dy_s <  iy + dy_s)   )) then
              return True
            else overlaps (x, y) items)
       overlaps _ _ = return False
   in do
        items <- getRef (items notepad)
        let getPos pos@(x, y) =
              do
                b <- overlaps pos items
                (if b then
                   getPos (if x + 2 * dx + 10 > 10 + dx + (num_cols * 2 * dx)
                             then (10 + dx, y + dy_s + dy_n + 10)
                             else (x + 2 * dx + 10, y))
                 else return pos)
        getPos (10 + dx, 10 + dy_n)

---
-- Gets the value from a notepad item.
getItemValue :: NotepadItem a -> IO a
getItemValue item = getRef (it_val item)


-- instances --

---
-- Internal.
instance Eq (NotepadItem a) where
---
-- Internal.
  item1 == item2 = it_img item1 == it_img item2

---
-- Internal.
instance GUIObject (NotepadItem a) where
---
-- Internal.
  toGUIObject item = toGUIObject (it_img item)
---
-- Internal.
  cname _ = "NotepadItem"

---
-- You can synchronize on a notepad item.
instance Synchronized (NotepadItem a) where
---
-- Synchronizes on a notepad item.
  synchronize item = synchronize (toGUIObject (it_img item))

---
-- A notepad item has a position on the associated notepad.
instance HasPosition (NotepadItem a) where
---
-- Sets the notepad item's position.
  position p@(x, y) item =
    itemPositionD2 p (it_img item) >>
    let (Distance iwidth, Distance iheight) = it_img_size item
    in itemPositionD2 (x, y + Distance (div iheight 2 + 7))
                      (it_txt item) >>
       return item
---
-- Gets the notepad item's position.
  getPosition item = getItemPositionD2 (it_img item)

---
-- A notepad item can be destroyed.
instance Destroyable (NotepadItem a) where
---
-- Destroys a notepad item.
  destroy item =
    do
      destroy (it_img item)
      destroy (it_txt item)
      mrects <- getRef (it_bg item)
      case mrects of
        Just (rect1, rect2) -> destroy rect1 >> destroy rect2
        _ -> done

---
-- (Re-)sets the name of a notepad item.
setName :: CItem c => NotepadItem c -> Name -> IO ()
setName item nm =
  do
    let (Distance dx, _) = it_img_size item
        len = div (dx + 80) char_px
    it_txt item # text (short nm len)
    done


--------------------------------------------------------------------------
-- notepad events
--------------------------------------------------------------------------

---
-- Binds a listener for notepad events to the notepad and returns
-- a corresponding event and an unbind action.
-- @param np      - the concerned notepad.
-- @return result - A pair of (event, unbind action).
bindNotepadEv :: Notepad a -> IO (Event (NotepadEvent a), IO ())
bindNotepadEv np =
  do
    ch <- newChannel
    setRef (event_queue np) (Just ch)
    return (receive ch, setRef (event_queue np) Nothing)

---
-- The <code>NotepadEvent</code> datatype.
data NotepadEvent a =
    Dropped (NotepadItem a, [NotepadItem a])     --' Drop event.
  | Selected (NotepadItem a)                     --' Selection event.
  | Deselected (NotepadItem a)                   --' Deselection event.
  | Doubleclick (NotepadItem a)                  --' Doubleclick event.
  | Rightclick [NotepadItem a]                   --' Rightclick event.
  | ReleaseSelection                             --' Buttonrelease after a selection.
  | ReleaseMovement EventInfo                    --' Buttonrelease after a movement.

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

---
-- The <code>Notepad</code> datatype.
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
            event_queue :: Ref (Maybe (Channel (NotepadEvent a))),

            -- clean up when destroyed
            clean_up :: [IO ()] }

---
-- The <code>ScrollType</code> datatype.
data ScrollType = Scrolled | NotScrolled deriving Eq

data UndoMotion = ToPerform (IO ()) | Performed


-- state --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad item =
  do
    notepaditems <- getRef (items notepad)
    setRef (items notepad) (item : notepaditems)

highlight :: Canvas -> NotepadItem a -> IO ()
highlight cnv item =
  do
    let (Distance iwidth, Distance iheight) = it_img_size item
    it_txt item # filling "white"
    s <- getRef (it_bg item)
    case s of
      Nothing -> do
                   (x, y) <- getPosition item
                   rect1 <- createRectangle cnv
                              [coord [(x - Distance (div iwidth 2 + 1),
                                       y - Distance (div iheight 2 + 1)),
                                      (x + Distance (div iwidth 2),
                                       y + Distance (div iheight 2))],
                               filling "blue", outline "blue"]
                   putItemAtBottom rect1
                   rect2 <- createRectangle cnv
                              [coord [(x - Distance
                                             (max (div iwidth 2 + 40) 40),
                                       y + Distance (div iheight 2)),
                                      (x + Distance
                                             (max (div iwidth 2 + 40) 40),
                                       y + Distance
                                             (div iheight 2 + 14))],
                               filling "blue", outline "blue"]
                   putItemAtBottom rect2
                   setRef (it_bg item) (Just (rect1, rect2))
      Just _  -> done

deHighlight :: NotepadItem a -> IO ()
deHighlight item =
  do
    it_txt item # filling "black"
    s <- getRef (it_bg item)
    case s of
      Just (rect1, rect2) ->
        destroy rect1 >> destroy rect2 >> setRef (it_bg item) Nothing
      _ -> done

---
-- Selects a specific notepad item.
-- @param np      - the concerned notepad.
-- @param item    - the concerned notepad item.
-- @return result - None.
selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem np item =
  do
    deselectAll np
    highlight (canvas np) item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (item : selecteditems)
    sendEv np (Selected item)

---
-- Adds an item to the notepad's selection.
-- @param np      - the concerned notepad.
-- @param item    - the concerned notepad item.
-- @return result - None.
selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem np item =
  do
    highlight (canvas np) item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (item : selecteditems)
    sendEv np (Selected item)

---
-- Deselects a notepad item.
-- @param np      - the concerned notepad.
-- @param item    - the concerned notepad item.
-- @return result - None.
deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem np item =
  do
    deHighlight item
    selecteditems <- getRef (selected_items np)
    setRef (selected_items np) (filter ((/=) item) selecteditems)
    sendEv np (Deselected item)

---
-- Selects all items inside the notepad.
-- @param np      - the concerned notepad.
-- @return result - None.
selectAll :: Notepad a -> IO ()
selectAll np =
  do
    notepaditems <- getRef (items np)
    mapM (highlight (canvas np)) notepaditems
    mapM (\item -> do
                     b <- isNotepadItemSelected np item
                     if b then done else sendEv np (Selected item))
         notepaditems
    setRef (selected_items np) notepaditems

---
-- Deselects all items inside the notepad.
-- @param np      - the concerned notepad.
-- @return result - None.
deselectAll :: Notepad a -> IO ()
deselectAll np =
  do
    notepaditems <- getRef (items np)
    selecteditems <- getRef (selected_items np)
    mapM deHighlight selecteditems
    mapM (\item -> do
                     b <- isNotepadItemSelected np item
                     if b then sendEv np (Deselected item) else done)
         notepaditems
    setRef (selected_items np) []

---
-- Deletes an item from a notepad.
-- @param np      - the concerned notepad.
-- @param item    - the concerned notepad item.
-- @return result - None.
deleteItem :: CItem c => Notepad c -> NotepadItem c -> IO ()
deleteItem np item =
  synchronize np
    (do
       notepaditems <- getRef (items np)
       selecteditems <- getRef (selected_items np)
       entereditem <- getRef (entered_item np)
       (if isJust entereditem then setRef (entered_item np) Nothing >>
                                   leftItem np (fromJust entereditem)
        else done)
       setRef (items np) (filter ((/=) item) notepaditems)
       setRef (selected_items np) (filter ((/=) item) selecteditems)
       destroy item)

---
-- Deletes all items from a notepad.
-- @param np      - the concerned notepad.
-- @return result - None.
clearNotepad :: Notepad a -> IO ()
clearNotepad np =
  do
    notepaditems <- getRef (items np)
    mapM destroy notepaditems
    setRef (items np) []
    setRef (selected_items np) []

---
-- Internal (for use with GenGUI).
undoLastMotion :: Notepad a -> IO ()
undoLastMotion np =
  synchronize np (do
                    act <- getRef (undo_last_motion np)
                    case act of
                      ToPerform act' -> setRef (undo_last_motion np)
                                               Performed >>
                                        act'
                      _ -> done)

---
-- <code>True</code> if the given notepad item is selected.
-- @param np      - the concerned notepad.
-- @param item    - the concerned notepad item.
-- @return result - <code>True</code> if the given notepad item is
--                - selected, otherwise <code>False</code>.
isNotepadItemSelected :: Notepad a -> NotepadItem a -> IO Bool
isNotepadItemSelected np item =
  do
    selecteditems <- getRef (selected_items np)
    return (any ((==) item) selecteditems)

---
-- Selects all items within the specified region.
-- @param p1      - the upper left coordinate of the region.
-- @param p2      - the lower right coordinate of the region.
-- @param np      - the concerned notepad.
-- @return result - None.
selectItemsWithin :: Position -> Position -> Notepad a -> IO ()
selectItemsWithin p1@(x0, y0) p2@(x1, y1) np =
  do
    notepaditems <- getRef (items np)
    let within :: Position -> Bool
        within (x, y)  =
          ((x0 <= x && x <= x1) || (x1 <= x && x <= x0)) &&
          ((y0 <= y && y <= y1) || (y1 <= y && y <= y0))
    mapM (\ item -> do
                      pos <- getPosition item
                      b <- isNotepadItemSelected np item
                      (if within pos then
                         if b then done else selectAnotherItem np item
                       else
                         if b then deselectItem np item else done))
         notepaditems
    done

---
-- Gets the items from a notepad.
-- @param np      - the concerned notepad.
-- @return result - A list of the contained notepad items.
getItems :: Notepad a -> IO [NotepadItem a]
getItems np = getRef (items np)

---
-- Gets the selected items from a notepad.
-- @param np      - the concerned notepad.
-- @return result - A list of the selected notepad items.
getSelectedItems :: Notepad a -> IO [NotepadItem a]
getSelectedItems np = getRef (selected_items np)


--------------------------------------------------------------------------
-- notepad construction
--------------------------------------------------------------------------

---
-- Constructs a new notepad and returns a handler.
-- @param par        - the parent widget (which has to be a container
--                   - widget).
-- @param scrolltype - the scrolltype for this notepad.
-- @param imgsize    - the size of the notepad items images for this
--                   - notepad.
-- @param mstate     - an optional previous notepad state to recover.
-- @param cnf        - the list of configuration options for this notepad.
-- @return result - A notepad.
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
                                            undo_last_motion = ulm,
                                            clean_up = [] })
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
                                            undo_last_motion = ulm,
                                            clean_up = [] })

    (click, _) <- bind cnv [WishEvent [] (ButtonPress (Just (BNo 1)))]
    (rightclick, _) <- bind cnv
                            [WishEvent [] (ButtonPress (Just (BNo 2)))]
    (motion', _) <- bind cnv [WishEvent [] Motion]
    (motion, _) <- Examples.watch motion'
    (clickmotion', _) <- bind cnv [WishEvent [Button1] Motion]
    (clickmotion, _) <- Examples.watch clickmotion'
    (doubleclick, _) <- bind cnv [WishEvent [Double]
                                            (ButtonPress (Just (BNo 1)))]
    (shiftclick, _) <- bind cnv [WishEvent [Shift]
                                           (ButtonPress (Just (BNo 1)))]
    (release, _) <- bind cnv [WishEvent [] (ButtonRelease (Just (BNo 1)))]
    (leave, _) <- bindSimple cnv Leave

    stopListening <- newChannel

    let getD :: IO (Distance, Distance)
        getD = do
                 (dx_norm, dx_displ_norm) <- view Horizontal cnv
                 (dy_norm, _) <- view Vertical cnv
                 (_, (Distance sizex, Distance sizey)) <-
                   getScrollRegion cnv
                 return (Distance (round (dx_norm *
                                          fromInteger (toInteger sizex))),
                         Distance (round (dy_norm *
                                          fromInteger (toInteger sizey))))

        addToTag :: CanvasTag -> NotepadItem a -> IO ()
        addToTag tag item =
          do
            it_img item # tags [tag]
            it_txt item # tags [tag]
            rects <- getRef (it_bg item)
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

        selectByRectangle :: Distance -> Distance -> Position ->
                             Rectangle -> Event ()
        selectByRectangle dx dy pos rect =
          let selectByRectangle' :: Position -> Rectangle -> Event ()
              selectByRectangle' pos@(x, y) rect =
                (do
                   (x1, y1) <- clickmotion >>>= getCoords
                   always (rect # coord [(x + dx, y + dy),
                                         (x1 + dx, y1 + dy)])
                   always (selectItemsWithin (x + dx, y + dy)
                                             (x1 + dx, y1 + dy) notepad)
                   selectByRectangle' pos rect) +>
                (do
                   ev_inf <- release
                   always (do
                             (dx, dy) <- getD
                             (x1,y1) <- getCoords ev_inf
                             sendEv notepad ReleaseSelection
                             selectItemsWithin (x + dx, y + dy)
                                               (x1 + dx, y1 + dy) notepad
                             destroy rect))
          in selectByRectangle' pos rect

        checkPositions :: [NotepadItem a] -> IO (Distance, Distance)
        checkPositions (item : items) =
          do
            let (Distance iwidth, Distance iheight) = it_img_size item
            (Distance x, Distance y) <- getPosition item
            (Distance dx, Distance dy) <- checkPositions items
            (_, (Distance sizex, Distance sizey)) <-
              getScrollRegion (canvas notepad)
            let min_x = x - (max (div iwidth 2 + 30) 40)
                max_x = x + (max (div iwidth 2 + 30) 40)
                min_y = y - (div iheight 2 + 1)
                max_y = y + (div iheight 2 + 14)
                dx' = if dx < 0 then min min_x dx
                      else if dx == 0 then
                             if min_x < 0 then min_x
                             else if min_x < 0 then min_x
                                  else if max_x > sizex then max_x - sizex
                                       else 0
                           else if dx > 0 then
                                  max dx (max_x - sizex)
                                else 0
                dy' = if dy < 0 then min min_y dy
                      else if dy == 0 then
                             if min_y < 0 then min_y
                             else if min_y < 0 then min_y
                                  else if max_y > sizey then max_y - sizey
                                       else 0
                           else if dy > 0 then
                                  max dy (max_y - sizey)
                                else 0
            return (Distance dx', Distance dy')
        checkPositions [] = return (Distance 0, Distance 0)




-- -----------------------------------------------------------------------
        grid_x :: Int
        grid_x = 15
        grid_y :: Int
        grid_y = 15

        checkDropZones :: FiniteMap (Int, Int) [NotepadItem a] ->
                          Notepad a -> Distance -> Distance -> IO ()
        checkDropZones it_map notepad x@(Distance ix) y@(Distance iy) =
          let doSet item =
                do
                  (x, y) <- getPosition item
                  let (Distance iwidth, Distance iheight) =
                        it_img_size item
                  rect1 <- createRectangle (canvas notepad)
                             [coord [(x - Distance (div iwidth 2 + 1),
                                      y - Distance (div iheight 2 + 1)),
                                     (x + Distance (div iwidth 2),
                                      y + Distance (div iheight 2))],
                              filling "yellow", outline "yellow"]
                  putItemAtBottom rect1
                  rect2 <- createRectangle (canvas notepad)
                             [coord [(x - Distance
                                            (max (div iwidth 2 + 40) 40),
                                      y + Distance (div iheight 2)),
                                     (x + Distance
                                            (max (div iwidth 2 + 40) 40),
                                      y + Distance (div iheight 2 + 14))],
                              filling "yellow", outline "yellow"]
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

              inDropZone item =
                do
                  (x_it, y_it) <- getPosition (it_img item)
                  return (if x_it - 30 < x && x_it + 30 > x &&
                             y_it - 10 < y && y_it + 30 > y then True
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

          in do (_, (Distance sizex, Distance sizey)) <-
                  getScrollRegion (canvas notepad)
                let idx@(idx_x, idx_y) = (div ix (div sizex grid_x), div iy (div sizey grid_y))
                checkDropZones' (lookupWithDefaultFM it_map [] idx)

        buildMap :: Notepad a -> IO (FiniteMap (Int, Int) [NotepadItem a])
        buildMap notepad =
          do notepaditems <- getRef (items notepad)
             selecteditems <- getRef (selected_items notepad)
             let nonselecteditems =
                   filter (\item -> not(any ((==) item) selecteditems))
                          notepaditems
             fmref <- newRef emptyFM
             let add (idx_x, idx_y) notepaditem =
                   if idx_x >= 0 && idx_x < 10 &&
                      idx_y >= 0 && idx_y < 10 then
                     do fm <- getRef fmref
                        let mnotepaditems = lookupFM fm (idx_x, idx_y)
                        let nufm = case mnotepaditems of
                                     Just notepaditems ->
                                       addToFM fm (idx_x, idx_y)
                                         (notepaditem : notepaditems)
                                     _ -> addToFM fm (idx_x, idx_y)
                                                  [notepaditem]
                        setRef fmref nufm
                   else done
                 getCenterIndex notepaditem =
                   do (_, (Distance sizex, Distance sizey)) <-
                        getScrollRegion (canvas notepad)
                      (Distance x, Distance y) <- getPosition notepaditem
                      return (div x (div sizex grid_x),
                              div y (div sizey grid_y))
                 addNotepadItem notepaditem =
                   do idx@(idx_x, idx_y) <- getCenterIndex notepaditem
                      add idx notepaditem
                      add (idx_x - 1, idx_y    ) notepaditem
                      add (idx_x - 1, idx_y - 1) notepaditem
                      add (idx_x    , idx_y - 1) notepaditem
                      add (idx_x + 1, idx_y - 1) notepaditem
                      add (idx_x + 1, idx_y    ) notepaditem
                      add (idx_x + 1, idx_y + 1) notepaditem
                      add (idx_x    , idx_y + 1) notepaditem
                      add (idx_x - 1, idx_y + 1) notepaditem
             mapM addNotepadItem nonselecteditems
             getRef fmref

--        moveSelectedItems :: FiniteMap (Int, Int) [NotepadItem a] ->
--                             Position -> Position -> CanvasTag -> Event ()
        moveSelectedItems it_map rpos@(rootx, rooty) (x0, y0) t =
             (do
                (x, y) <- clickmotion >>>= getCoords
                always (do
                          (dx, dy) <- getD
                          checkDropZones it_map notepad (x + dx) (y + dy)
                          setRef (undo_last_motion notepad)
                                 (ToPerform (moveItem t (rootx - x0)
                                                        (rooty - y0)))
                          moveItem t (x - x0) (y - y0))
                moveSelectedItems it_map rpos (x, y) t)
          +> (do
                ev_inf <- release
                always (do
                          sendEv notepad (ReleaseMovement ev_inf)
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
                            _ -> do
                                   selecteditems <-
                                     getRef selecteditemsref
                                   (dx, dy) <-
                                     checkPositions selecteditems
                                   moveItem t (-dx) (-dy)))


{-
        moveSelectedItems :: Position -> Position -> CanvasTag ->
                             Event ()
        moveSelectedItems rpos@(rootx, rooty) (x0, y0) t =
             (do
                (x, y) <- clickmotion >>>= getCoords
                always (do
                          (dx, dy) <- getD
                          checkDropZones notepad (x + dx) (y + dy)
                          setRef (undo_last_motion notepad)
                                 (ToPerform (moveItem t (rootx - x0)
                                                        (rooty - y0)))
                          moveItem t (x - x0) (y - y0))
                moveSelectedItems rpos (x, y) t)
          +> (do
                ev_inf <- release
                always (do
                          sendEv notepad (ReleaseMovement ev_inf)
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
                            _ -> do
                                   selecteditems <-
                                     getRef selecteditemsref
                                   (dx, dy) <-
                                     checkPositions selecteditems
                                   moveItem t (-dx) (-dy)))
-}



-- -----------------------------------------------------------------------


        checkEnteredItem (x, y) =
          let overItem item =
                do
                  (dx, dy) <- getD
                  (x_it, y_it) <- getPosition (it_img item)
                  return (if x_it - 30 < x + dx && x_it + 30 > x + dx &&
                             y_it - 10 < y + dy && y_it + 30 > y + dy then True
                          else False)
              checkItems (item : items) =
                do
                  b <- overItem item
                  (if b then setRef entereditemref (Just item)
                   else checkItems items)
              checkItems _  = setRef entereditemref Nothing
          in synchronize notepad
               (do
                  last <- getRef entereditemref
                  items <- getRef notepaditemsref
                  checkItems items
                  new <- getRef entereditemref
                  (if isJust last then
                     if isJust new then
                       if fromJust last == fromJust new then done
                       else
                         leftItem notepad (fromJust last) >>
                         enteredItem notepad (fromJust new)
                     else
                       leftItem notepad (fromJust last)
                   else
                     if isJust new then
                       enteredItem notepad (fromJust new)
                     else
                       done))

        listenNotepad :: Event ()
        listenNotepad =
             (leave >> always (do
                                 mentereditem <- getRef entereditemref
                                 (if isJust mentereditem then
                                    leftItem notepad
                                             (fromJust mentereditem) >>
                                    setRef entereditemref Nothing
                                  else done)) >>
                       listenNotepad)
{-          +> (do
                (x, y) <- motion >>>= getCoords
                always (checkEnteredItem (x, y))
                listenNotepad)-}
          +> (do
                (x, y) <- click >>>= getCoords
                always
                  (do
                     entereditem <- getRef entereditemref
                     case entereditem of
                       Nothing -> do
                                    deselectAll notepad
                                    (dx, dy) <- getD
                                    rect <- createRectangle cnv
                                              [coord [(x + dx, y + dy),
                                                      (x + dx, y + dy)]]
                                    sync
                                      (selectByRectangle dx dy (x, y)
                                                         rect)
                                    done
                       Just item ->
                         do
                           leftItem notepad item
                           b <- isNotepadItemSelected notepad item
                           if b then done else selectItem notepad item
                           t <- createTagFromSelection notepad
                           sync (do mp <- always (buildMap notepad)
                                    moveSelectedItems mp (x, y) (x, y) t)
                           done)
                listenNotepad)
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
                           b <- isNotepadItemSelected notepad
                                                      entereditem
                           (if b then
                              do
                                selecteditems <- getRef selecteditemsref
                                sendEv notepad (Rightclick selecteditems)
                            else
                              do
                                selectItem notepad entereditem
                                sendEv notepad
                                       (Rightclick [entereditem])))
                listenNotepad)
          +> (doubleclick >> do
                               always (do
                                         entereditem <-
                                           getRef entereditemref
                                         case entereditem of
                                           Just item ->
                                             sendEv notepad
                                                    (Doubleclick item)
                                           _ -> done)
                               listenNotepad)
          +> (shiftclick >> do
                              always (do
                                        entereditem <-
                                          getRef entereditemref
                                        case entereditem of
                                          Just item ->
                                            do
                                              b <- isNotepadItemSelected
                                                     notepad item
                                              (if b then
                                                 deselectItem notepad
                                                              item
                                               else
                                                 selectAnotherItem
                                                   notepad item)
                                          _ -> done)
                              listenNotepad)
          +> (release >> listenNotepad)  -- avoid cueing of release events
          +> receive stopListening

    spawnEvent listenNotepad
    foldl (>>=) (return notepad) cnf
    case mstate of
      Just state -> importNotepadState notepad state
      _ -> done
    return notepad

updNotepadScrollRegion :: Notepad a -> IO ()
updNotepadScrollRegion np =
  let getMax (item : items) mx my =
        do (x, y) <- getPosition item
           let nux = max x mx
               nuy = max y my
           getMax items nux nuy
      getMax _ mx my = return (mx, my)
  in do items <- getItems np
        (x, y) <- getMax items 0 0
        np # size (x + 80, y + 40)
        done

-- instances --

---
-- Internal.
instance GUIObject (Notepad a) where
---
-- Internal.
  toGUIObject np =
    case (scrollbox np) of Nothing -> toGUIObject (canvas np)
                           Just box -> toGUIObject box
---
-- Internal.
  cname _ = "Notepad"

---
-- A notepad can be destroyed.
instance Destroyable (Notepad a) where
---
-- Destroys a notepad.
  destroy = destroy . toGUIObject          -- TD : clean up !!!

---
-- A notepad has standard widget properties
-- (concerning focus, cursor).
instance Widget (Notepad a)

---
-- You can synchronize on a notepad object.
instance Synchronized (Notepad a) where
---
-- Synchronizes on a notepad object.
  synchronize w = synchronize (toGUIObject w)

---
-- A notepad has a configureable border.
instance HasBorder (Notepad a)

---
-- A notepad has a configureable background colour.
instance HasColour (Notepad a) where
---
-- Internal.
  legalColourID np = hasBackGroundColour (canvas np)
---
-- Internal.
  setColour notepad cid col =
    setColour (canvas notepad) cid col >> return notepad
---
-- Internal.
  getColour np cid = getColour (canvas np) cid

---
-- A notepad has a configureable size.
instance HasSize (Notepad a) where
---
-- Sets the notepad's width.
  width s np =
    do
      (_, (_, sizey)) <- getScrollRegion (canvas np)
      (canvas np) # scrollRegion ((0, 0), (s, sizey))
      if isJust (scrollbox np) then done else canvas np # width s >> done
      return np
---
-- Gets the notepad's width.
  getWidth np = getWidth (canvas np)
---
-- Sets the notepad's height.
  height s np =
    do
      (_, (sizex, _)) <- getScrollRegion (canvas np)
      (canvas np) # scrollRegion ((0, 0), (sizex, s))
      (if (isJust (scrollbox np)) then done
       else canvas np # height s >> done)
      return np
---
-- Gets the notepad's height.
  getHeight np = getHeight (canvas np)


-- -----------------------------------------------------------------------
-- state import / export
-- -----------------------------------------------------------------------

---
-- The <code>NotepadExportItem</code> datatype.
data CItem c => NotepadExportItem c =
  NotepadExportItem { val :: c,
                      pos :: Position,
                      selected :: Bool }

type NotepadState c = [NotepadExportItem c]

---
-- Exports a notepad's state.
-- @param np      - the concerned notepad.
-- @return result - The notepad's state.
exportNotepadState :: CItem c => Notepad c -> IO (NotepadState c)
exportNotepadState np =
  synchronize np (do
                    items' <- getRef (items np)
                    exportNotepadState' np items')
  where exportNotepadState' :: CItem c => Notepad c -> [NotepadItem c] ->
                               IO (NotepadState c)
        exportNotepadState' np (item : items) =
          do
            val' <- getRef (it_val item)
            pos <- getPosition (it_img item)
            is_selected <- isNotepadItemSelected np item
            rest <- exportNotepadState' np items
            return (NotepadExportItem { val = val',
                                        pos = pos,
                                        selected = is_selected } : rest)
        exportNotepadState' _ _ = return []

---
-- Imports a notepad's state.
-- @param np      - the concerned notepad.
-- @return result - None.
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
