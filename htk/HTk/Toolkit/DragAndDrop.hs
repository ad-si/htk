-- | This module is now obsolescent and does not compile.
module HTk.Toolkit.DragAndDrop (

  Notepad,
  NotepadItem,

  newNotepad,
  createNotepadItem,

  getItemValue,

  ScrollType(..),

  ImageSize,

  Name(..),
  name,
  getName,

  selectAll,
  deselectAll,
  selectItem,
  selectAnotherItem,
  selectItemsWithin,
  deselectItem,
  getSelectedItems,

  deleteItem,
  clearNotepad,

  dropEvent,
  selectionEvent,
  doubleClickEvent,
  rightClickEvent,

  ExportItem(..),
  NotepadState,
  exportState,
  importState

) where

import HTk.Toplevel.HTk
import HTk.Canvasitems.CanvasItemAux
import HTk.Toolkit.ScrollBox
import Reactor.ReferenceVariables
import HTk.Toolkit.Name
import Events.Examples(watch)
import HTk.Kernel.Core
import Data.Maybe

getCoords :: EventInfo -> IO (Distance, Distance)
getCoords eventInfo = return (x eventInfo, y eventInfo)


-------------------
-- Notepad items --
-------------------

-- type --

data NotepadItem a =
  NotepadItem ImageItem                                           -- image
              ImageSize                                   -- size of image
              TextItem                                   -- displayed name
              (Ref a)                                             -- value
              (Ref Name)                                           -- name
              (Ref (Maybe (Rectangle, Rectangle)))       -- bg if selected


-- constructor --

createNotepadItem :: a -> Notepad a -> [Config (NotepadItem a)] ->
                     IO (NotepadItem a)
createNotepadItem val notepad@(Notepad cnv _ imgsize _ _ entereditemref _
                                       _ _ _ _) cnf =
  do
    img <- createImageItem cnv []
    txt <- createTextItem cnv [font (Helvetica, 10 :: Int), text "unnamed"]
    itemval <- newRef val
    itemname <- newRef (newName "unnamed")
    itemsel <- newRef Nothing
    let item = NotepadItem img imgsize txt itemval itemname itemsel
    foldl (>>=) (return item) cnf

    (enter1, _) <- bindSimple img Enter
    (leave1, _) <- bindSimple img Leave
    (enter2, _) <- bindSimple txt Enter
    (leave2, _) <- bindSimple txt Leave

    let listenItem :: Event ()
        listenItem =
             (enter1 >> always (setRef entereditemref (Just item)))
          +> (leave1 >> always (setRef entereditemref Nothing))
          +> (enter2 >> always (setRef entereditemref (Just item)))
          +> (leave2 >> always (setRef entereditemref Nothing))

    spawnEvent (forever listenItem)

    addItemToState notepad item
    return item

getItemValue :: NotepadItem a -> IO a
getItemValue (NotepadItem _ _ _ valref _ _) = getRef valref


-- instances --

instance Eq (NotepadItem a) where
  (NotepadItem img1 _ _ _ _ _) == (NotepadItem img2 _ _ _ _ _) =
    img1 == img2

instance GUIObject (NotepadItem a) where
  toGUIObject (NotepadItem img _ _ _ _ _) = toGUIObject img
  cname _ = "NotepadItem"

instance Synchronized (NotepadItem a) where
  synchronize = synchronize . toGUIObject

instance HasPosition (NotepadItem a) where
  position p@(x, y) n@(NotepadItem img imgsize txt _ _ _) =
    itemPositionD2 p img >>
    let (Distance iwidth, Distance iheight) = imgsize
    in itemPositionD2 (x, y + Distance (div iheight 2 + 7)) txt >>
       return n
  getPosition (NotepadItem img _ _ _ _ _) = getItemPositionD2 img

instance HasPhoto (NotepadItem a) where
  photo i item@(NotepadItem img _ _ _ _ _) =
    img # photo i >> return item
  getPhoto (NotepadItem img _ _ _ _ _) = getPhoto img

instance Destroyable (NotepadItem a) where
  destroy item@(NotepadItem img _ txt _ _ itemsel) =
    do
      destroy img
      destroy txt
      mrects <- getRef itemsel
      case mrects of
        Just (rect1, rect2) -> destroy rect1 >> destroy rect2
        _ -> done

name :: Name -> Config (NotepadItem a)
name itname w@(NotepadItem _ (Distance dx, _) txt _ nm _) =
  do
    setRef nm itname
    let len = div (dx + 80) 5
    txt # text {- (full itname) -} (short itname len)
    return w

getName :: NotepadItem a -> (IO Name)
getName (NotepadItem _ _ _ _ nm _) =
  do
    itemname <- getRef nm
    return itemname


-- events --

dropEvent :: Notepad a -> Channel (NotepadItem a, [NotepadItem a])
dropEvent (Notepad _ _ _ _ _ _ _ _ dmsgQ _ _) = dmsgQ

selectionEvent :: Notepad a -> Channel (NotepadItem a, Bool)
selectionEvent (Notepad _ _ _ _ _ _ _ smsgQ _ _ _) = smsgQ

doubleClickEvent :: Notepad a -> Channel (NotepadItem a)
doubleClickEvent (Notepad _ _ _ _ _ _ _ _ _ doubleclickmsgQ _) =
  doubleclickmsgQ

rightClickEvent :: Notepad a -> Channel [NotepadItem a]
rightClickEvent (Notepad _ _ _ _ _ _ _ _ _ _ rightclickmsgQ) =
  rightclickmsgQ



-------------
-- Notepad --
-------------

-- type --

data Notepad a =
  Notepad Canvas                                            -- main canvas
          (Maybe (ScrollBox Canvas))                          -- scrollbox
          ImageSize                                      -- size of images
          (Ref ([NotepadItem a]))                       -- contained items
          (Ref ([NotepadItem a]))                        -- selected items
          (Ref (Maybe (NotepadItem a)))                    -- entered item
          (Ref (Maybe (NotepadItem a, Rectangle, Rectangle)))
                             -- entered item when other items dragged / bg
          (Channel (NotepadItem a, Bool))         -- selection event queue
          (Channel (NotepadItem a, [NotepadItem a]))    -- dnd event queue
          (Channel (NotepadItem a))             -- doubleclick event queue
          (Channel [NotepadItem a])              -- rightclick event queue
  deriving Eq

data ScrollType = Scrolled | NotScrolled deriving Eq

type ImageSize = Size


-- state --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad@(Notepad _ _ _ notepaditemsref _ _ _ _ _ _ _)
               item =
  do
    notepaditems <- getRef notepaditemsref
    setRef notepaditemsref (item : notepaditems)

highlight :: Canvas -> NotepadItem a -> IO ()
highlight cnv item@(NotepadItem img imgsize txt _ _ sel) =
  do
    let (Distance iwidth, Distance iheight) = imgsize
    txt # filling "white"
    s <- getRef sel
    case s of
      Nothing -> do
                   (x, y) <- getPosition item
                   rect1 <- createRectangle cnv
                              [filling "grey", outline "grey",
                               coord
                                 [(x - Distance (div iwidth 2 + 1),
                                   y - Distance (div iheight 2 + 1)),
                                  (x + Distance (div iwidth 2 + 1),
                                   y + Distance (div iheight 2 + 1))]]
                   putItemAtBottom rect1
                   rect2 <- createRectangle cnv
                              [filling "grey", outline "grey",
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
deHighlight item@(NotepadItem img _ txt _ _ sel) =
  do
    txt # filling "black"
    s <- getRef sel
    case s of
      Nothing             -> done
      Just (rect1, rect2) ->
        destroy rect1 >> destroy rect2 >> setRef sel Nothing

selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem np@(Notepad cnv _ _ _ selecteditemsref _ _ smsgQ _ _ _) item =
  do
    deselectAll np
    highlight cnv item
    selecteditems <- getRef selecteditemsref
    setRef selecteditemsref (item : selecteditems)
    spawnEvent (noWait (send smsgQ (item, True)))
    done

selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem np@(Notepad cnv _ _ _ selecteditemsref _ _ smsgQ _ _ _)
                  item =
  do
    highlight cnv item
    selecteditems <- getRef selecteditemsref
    setRef selecteditemsref (item : selecteditems)
    spawnEvent (noWait (send smsgQ (item, True)))
    done

deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem np@(Notepad _ _ _ _ selecteditemsref _ _ smsgQ _ _ _) item =
  do
    deHighlight item
    selecteditems <- getRef selecteditemsref
    setRef selecteditemsref (filter ((/=) item) selecteditems)
    spawnEvent (noWait (send smsgQ (item, False)))
    done

selectAll :: Notepad a -> IO ()
selectAll np@(Notepad cnv _ _ notepaditemsref selecteditemsref _ _ smsgQ
                      _ _ _) =
  do
    notepaditems <- getRef notepaditemsref
    mapM (highlight cnv) notepaditems
    mapM (\item -> do
                     b <- DragAndDrop.isSelected np item
                     if b then done
                       else spawnEvent
                              (noWait (send smsgQ (item, True))) >> done)
         notepaditems
    setRef selecteditemsref notepaditems

deselectAll :: Notepad a -> IO ()
deselectAll np@(Notepad _ _ _ notepaditemsref selecteditemsref _ _ smsgQ
                        _ _ _) =
  do
    notepaditems <- getRef notepaditemsref
    selecteditems <- getRef selecteditemsref
    mapM deHighlight selecteditems
    mapM (\item -> do
                     b <- DragAndDrop.isSelected np item
                     if b then spawnEvent
                                 (noWait (send smsgQ (item, False))) >>
                               done
                       else done)
         notepaditems
    setRef selecteditemsref []
    done

deleteItem :: Notepad a -> NotepadItem a -> IO ()
deleteItem np@(Notepad _ _ _ notepaditemsref selecteditemsref _ _ _ _ _ _)
           item =
  do
    notepaditems <- getRef notepaditemsref
    selecteditems <- getRef selecteditemsref
    setRef notepaditemsref (filter ((==) item) notepaditems)
    setRef selecteditemsref (filter ((==) item) selecteditems)
    destroy item

clearNotepad :: Notepad a -> IO ()
clearNotepad np@(Notepad _ _ _ notepaditemsref selecteditemsref _ _ _ _
                         _ _) =
  do
    notepaditems <- getRef notepaditemsref
    mapM destroy notepaditems
    setRef notepaditemsref []
    setRef selecteditemsref []

isSelected :: Notepad a -> NotepadItem a -> IO Bool
isSelected np@(Notepad _ _ _ _ selecteditemsref _ _ _ _ _ _) item =
  do
    selecteditems <- getRef selecteditemsref
    return (any ((==) item) selecteditems)

selectItemsWithin :: Position -> Position -> Notepad a -> IO ()
selectItemsWithin (x0, y0) (x1, y1)
                  np@(Notepad _ _ _ notepaditemsref selecteditemsref _
                              _ _ _ _ _) =
  do
    notepaditems <- getRef notepaditemsref
    let within :: Position -> Bool
        within (x, y)  =
          ((x0 <= x && x <= x1) || (x1 <= x && x <= x0)) &&
          ((y0 <= y && y <= y1) || (y1 <= y && y <= y0))
    mapM (\ item -> do
                      pos <- getPosition item
                      if within pos then
                        selectAnotherItem np item
                        else do
                               b <- DragAndDrop.isSelected np item
                               if b then done
                                 else deselectItem np item)
         notepaditems
    done

getSelectedItems :: Notepad a -> IO [NotepadItem a]
getSelectedItems np@(Notepad _ _ _ _ selecteditemsref _ _ _ _ _ _) =
  do
    selecteditems <- getRef selecteditemsref
    return selecteditems


-- constructor --

newNotepad :: Container par => par -> ScrollType -> ImageSize ->
                               [Config (Notepad a)] -> IO (Notepad a)
newNotepad par scrolltype imgsize cnf =
  do
    let scrolled = (scrolltype == Scrolled)
    notepaditemsref <- newRef []
    selecteditemsref <- newRef []
    entereditemref <- newRef Nothing
    dropref <- newRef Nothing
    smsgQ <- newChannel
    dmsgQ <- newChannel
    doubleclickmsgQ <- newChannel
    rightclickmsgQ <- newChannel
    (cnv, notepad) <- if scrolled then
                        do
                          (scrollbox, cnv) <-
                            newScrollBox par (\ p -> newCanvas p []) []
                          return (cnv,
                                  Notepad cnv (Just scrollbox) imgsize
                                    notepaditemsref selecteditemsref
                                    entereditemref dropref smsgQ dmsgQ
                                    doubleclickmsgQ rightclickmsgQ)
                      else
                        do
                          cnv <- newCanvas par []
                          return (cnv,
                                  Notepad cnv Nothing imgsize
                                    notepaditemsref selecteditemsref
                                    entereditemref dropref smsgQ dmsgQ
                                    doubleclickmsgQ rightclickmsgQ)

    (click, _) <- bind cnv [WishEvent [] (ButtonPress (Just (BNo 1)))]
    (rightclick, _) <-
      bind cnv [WishEvent [] (ButtonPress (Just (BNo 2)))]
    (clickmotion', _) <- bind cnv [WishEvent [Button1] Motion]
    (clickmotion, _) <- Examples.watch (clickmotion')
    (doubleclick, _) <- bind cnv [WishEvent [Double]
                                            (ButtonPress (Just (BNo 1)))]
    (shiftclick, _) <- bind cnv [WishEvent [Shift]
                                           (ButtonPress (Just (BNo 1)))]
    (release, _) <- bind cnv [WishEvent [] (ButtonRelease (Just (BNo 1)))]

    let addToTag :: CanvasTag -> NotepadItem a -> IO ()
        addToTag tag (NotepadItem img _ txt _ _ rectref) =
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
        createTagFromSelection notepad@(Notepad cnv _ _ notepaditemsref
                                                selecteditemsref _ _ _
                                                _ _ _) =
          do
            notepaditems <- getRef notepaditemsref
            selecteditems <- getRef selecteditemsref
            tag <- createCanvasTag cnv []
            mapM (addToTag tag) selecteditems >> done
            return tag

        checkDropZones :: Notepad a -> Distance -> Distance -> IO ()
        checkDropZones notepad@(Notepad cnv _ _ notepaditemsref
                                        selecteditemsref _ dropref _ _ _
                                        _)
                       x y =
          let doSet item@(NotepadItem _ imgsize _ _ _ _)=
                do
                  (x, y) <- getPosition item
                  let (Distance iwidth, Distance iheight) = imgsize
                  rect1 <- createRectangle cnv
                             [filling "yellow", outline "yellow",
                              coord [(x - Distance (div iwidth 2 + 1),
                                      y - Distance (div iheight 2 + 1)),
                                     (x + Distance (div iwidth 2 + 1),
                                      y + Distance (div iheight 2 + 1))]]
                  putItemAtBottom rect1
                  rect2 <- createRectangle cnv
                             [filling "yellow", outline "yellow",
                              coord
                                [(x - Distance
                                        (max (div iwidth 2 + 30) 40),
                                  y + Distance (div iheight 2 + 1)),
                                 (x + Distance
                                        (max (div iwidth 2 + 30) 40),
                                  y + Distance (div iheight 2 + 14))]]
                  putItemAtBottom rect2
                  setRef dropref (Just (item, rect1, rect2))

              setDropRef item =
                do
                   drop <- getRef dropref
                   (case drop of
                      Nothing -> doSet item
                      Just (ditem, rect1, rect2) ->
                        if item == ditem then done
                        else destroy rect1 >> destroy rect2 >> doSet item)

              inDropZone (NotepadItem img _ _ _ _ _) =
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
                  maybeitem <- getRef dropref
                  case maybeitem of
                    Just (_, rect1, rect2) ->
                      destroy rect1 >> destroy rect2 >>
                      setRef dropref Nothing
                    Nothing -> done
          in do
               notepaditems <- getRef notepaditemsref
               selecteditems <- getRef selecteditemsref
               (let nonselecteditems =
                      filter (\item -> not(any ((==) item) selecteditems))
                             notepaditems
                in checkDropZones' nonselecteditems)

        selectByRectangle :: Position -> Rectangle -> Event ()
        selectByRectangle pos@(x, y) rect =
             (do
                (x1, y1) <- clickmotion >>>= getCoords
                always (rect # coord [(x, y), (x1, y1)])
--                always (selectItemsWithin (x, y) (x1, y1) notepad)
                selectByRectangle pos rect)
          +> (do
                (x1, y1) <- release >>>= getCoords
                always (selectItemsWithin (x, y) (x1, y1) notepad >>
                        destroy rect))

        moveSelectedItems :: Position -> Position -> CanvasTag -> Event ()
        moveSelectedItems rpos@(rootx, rooty) (x0, y0) t =
             (do
                (x, y) <- clickmotion >>>= getCoords
                always (checkDropZones notepad x y >>
                        moveItem t (x - x0) (y - y0))
                moveSelectedItems rpos (x, y) t)
          +> (release >>
              always (do
                        drop <- getRef dropref
                        case drop of
                          Just (item, rect1, rect2) ->
                            do
                              selecteditems <- getRef selecteditemsref
                              moveItem t (rootx - x0) (rooty - y0)
                              setRef dropref Nothing
                              destroy rect1
                              destroy rect2
                              spawnEvent
                                (noWait (send dmsgQ
                                              (item, selecteditems)))
                              done
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
                        Just item@(NotepadItem img _ _ _ _ _) ->
                          do
                            b <- DragAndDrop.isSelected notepad item
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
                                    syncNoWait (send rightclickmsgQ [])
                       Just entereditem ->
                         do
                           b <- DragAndDrop.isSelected notepad
                                                       entereditem
                           (if b then
                              do
                                selecteditems <- getRef selecteditemsref
                                syncNoWait (send rightclickmsgQ
                                                 selecteditems)
                            else
                              do
                                selectItem notepad entereditem
                                syncNoWait (send rightclickmsgQ
                                                 [entereditem]))))
          +> (doubleclick >> always (do
                                       entereditem <-
                                         getRef entereditemref
                                       case entereditem of
                                         Just item ->
                                           spawnEvent (noWait
                                             (send doubleclickmsgQ
                                                   item)) >> done
                                         _ -> done))
          +> (shiftclick >> always (do
                                      entereditem <- getRef entereditemref
                                      case entereditem of
                                        Just item ->
                                          do
                                            b <- DragAndDrop.isSelected
                                                   notepad item
                                            if b then deselectItem notepad
                                                        item
                                              else selectAnotherItem
                                                     notepad item
                                        _ -> done)))

    spawnEvent (forever listenNotepad)
    foldl (>>=) (return notepad) cnf
    return notepad


-- instances --

instance GUIObject (Notepad a) where
  toGUIObject (Notepad cnv scr _ _ _ _ _ _ _ _ _) =
    case scr of Nothing  -> toGUIObject cnv
                Just box -> toGUIObject box
  cname _ = "Notepad"

instance Destroyable (Notepad a) where
  destroy   = destroy . toGUIObject

instance Widget (Notepad a)

instance Synchronized (Notepad a) where
  synchronize w = synchronize (toGUIObject w)

instance HasBorder (Notepad a)

instance HasColour (Notepad a) where
  legalColourID (Notepad cnv _ _ _ _ _ _ _ _ _ _) =
    hasBackGroundColour cnv
  setColour notepad@(Notepad cnv _ _ _ _ _ _ _ _ _ _) cid col =
    setColour cnv cid col >> return notepad
  getColour (Notepad cnv _ _ _ _ _ _ _ _ _ _) cid = getColour cnv cid

instance HasSize (Notepad a) where
  width s notepad@(Notepad cnv _ _ _ _ _ _ _ _ _ _)  =
    cnv # width s >> return notepad
  getWidth (Notepad cnv _ _ _ _ _ _ _ _ _ _) = getWidth cnv
  height s notepad@(Notepad cnv _ _ _ _ _ _ _ _ _ _) =
    cnv # height s >> return notepad
  getHeight (Notepad cnv _ _ _ _ _ _ _ _ _ _) = getHeight cnv


-- -----------------------------------------------------------------------
-- state import / export
-- -----------------------------------------------------------------------

data ExportItem a = ExportItem { nm :: Name,
                                 img :: Maybe Image,
                                 val :: a,
                                 pos :: Position,
                                 selected :: Bool }

type NotepadState a = [ExportItem a]

exportState :: Notepad a -> IO (NotepadState a)
exportState np@(Notepad _ _ _ items _ _ _ _ _ _ _) =
  do
    items' <- getRef items
    exportState' np items'
  where exportState' :: Notepad a -> [NotepadItem a] ->
                        IO (NotepadState a)
        exportState' np (item@(NotepadItem img _ txt val nm _) : items) =
          do
            nm' <- getRef nm
            putStr "getting photo ... "
            img' <- getPhoto img
            putStrLn "got the photo"
            val' <- getRef val
            pos <- getPosition img
            is_selected <- DragAndDrop.isSelected np item
            rest <- exportState' np items
            return (ExportItem { nm = nm',
                                 img = img',
                                 val = val',
                                 pos = pos,
                                 selected = is_selected } : rest)
        exportState' _ _ = return []

importState :: Notepad a -> NotepadState a -> IO ()
importState np st =
  do
    clearNotepad np
    addItems np st
  where addItems :: Notepad a -> NotepadState a -> IO ()
        addItems np (it : items) =
          do
            new_it <- createNotepadItem (val it) np
                        [position (pos it), name (nm it)]
            (if (isJust (img it)) then
               new_it # photo (fromJust (img it)) >> done
             else done)
            if selected it then selectAnotherItem np new_it else done
            addItems np items
        addItems _ _ = done

-- geht so nicht, wg. Typ (Config a) :
-- getItems :: Notepad a -> IO [Config (NotepadItem a)]
