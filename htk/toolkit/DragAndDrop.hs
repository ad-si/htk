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
ScrollType(..),

name {- :: ItemName -> Config (NotepadItem a) -},
getName {- :: NotepadItem a -> (IO ItemName) -},

newNotepadItem {- :: a -> Notepad a -> [Config (NotepadItem a)] -> IO (NotepadItem a) -},
newNotepad {- :: ScrollType -> [Config Box] -> [Config Canvas {- noch ändern!! -}] -> IO (Notepad a) -}

) where


import Concurrency
import GUICore
import HTk
import EmbeddedCanvasWin
import Frame
import Image
import Canvas
import CanvasItemAux
import CanvasItem
import ImageItem
import Rectangle
import Mouse
import CanvasTag
import ScrollBar
import Printer
import RVar

debugMsg str = putStr(">>> " ++ str ++ "\n\n")


-------------------
-- Notepad items --
-------------------

-- types --

data NotepadItem a =
  NotepadItem ImageItem (RVar a) (RVar ItemName) (RVar (Maybe Rectangle)) deriving Eq


-- constructor --

newNotepadItem :: a -> Notepad a -> [Config (NotepadItem a)] -> IO (NotepadItem a)
newNotepadItem val notepad@(Notepad cnv _ _) cnf =
  do
    img <- newImageItem [parent cnv]
    itemval <- newRVar val
    itemname <- newRVar (ItemName { short = \_ -> "", full = "" })
    itemsel <- newRVar Nothing
    item <- return(NotepadItem img itemval itemname itemsel)
    foldl (>>=) (return item) cnf
    interactor (select notepad img item)
    addItemToState notepad item
    return item
  where select :: Notepad a -> ImageItem -> NotepadItem a -> InterActor -> IA ()
        select notepad@(Notepad cnv _ _) img item iact =
          let sel = do
                      is_sel <- isSelected notepad item
                      if is_sel then done else selectItem notepad item
              add = do
                      is_sel <- isSelected notepad item
                      if is_sel then deselectItem notepad item else selectAnotherItem notepad item
          in    (mouseButtonPress img 1 >>> sel)
             +> (mouseEvent img (Button1, Motion) >>>=
                 \ ((x, y), _) -> become iact (moving notepad x y img item iact))
             +> (mouseEvent img (Shift, ButtonPress(Just 1)) >>> add)
        moveItems :: [NotepadItem a] -> Distance -> Distance -> IO ()
        moveItems ((NotepadItem img _ _ sel):items) dx dy =
          do
            sel' <- getVar sel
            case sel' of
              Nothing   ->  moveItem img dx dy >> moveItems items dx dy
              Just rect -> moveItem rect dx dy >> moveItem img dx dy >> moveItems items dx dy
        moveItems [] _ _                                = done
        moving :: Notepad a -> Distance -> Distance -> ImageItem ->
                  NotepadItem a -> InterActor -> IA ()
        moving notepad@(Notepad cnv _ state) x0 y0 img item@(NotepadItem _ _ _ sel) iact =
	  let move x0 y0 =
                (mouseEvent img (Button1, Motion) >>>=
                \ ((x, y), _)-> do debugMsg("Moving...  x= " ++ show x ++ 
	                                  ", y= "++ show y++
	                                  ", dx = " ++ show(x-x0) ++ 
	                                  ", dy = " ++ show(y-y0))
                                 -- (_, selecteditems) <- getVar state
                                 -- moveItems selecteditems (x - x0) (y - y0)
                                   sel' <- getVar sel
                                   case sel' of
                                     Nothing -> done
                                     Just rect -> moveItem rect (x - x0) (y - y0)
                                   moveItem img (x - x0) (y - y0)
                                   sync (move x y))
                +> (mouseEvent img (ButtonRelease Nothing) >>>
                    do debugMsg "Moved"
                       become iact (select notepad img item iact)) in move x0 y0
-- instances --

instance GUIObject (NotepadItem a) where
  toGUIObject (NotepadItem img _ _ _) = toGUIObject img
  cname _ = "NotepadItem"

instance HasPosition (NotepadItem a) where
  position p n@(NotepadItem img _ _ _) = itemPositionD2 p img >> return n
  getPosition (NotepadItem img _ _ _) = getItemPositionD2 img

instance HasPhoto (NotepadItem a) where
  photo i n@(NotepadItem img _ _ _) = img # photo i >> return n

data ItemName = ItemName { short :: Int -> String,
	                   full  :: String }

name :: ItemName -> Config (NotepadItem a)
name itname w@(NotepadItem _ _ nm _) =
  do
    setVar nm itname
    return w

getName :: NotepadItem a -> (IO ItemName)
getName (NotepadItem _ _ nm _) =
  do
    itemname <- getVar nm
    return itemname

instance Interactive (NotepadItem a)

--dropEvent :: NotepadItem a -> IA [(NotepadItem a)]

--selectionEvent :: NotepadItem a -> IA ()


-------------
-- Notepad --
-------------

-- types --

data (Notepad a) = Notepad Canvas Box (RVar ([NotepadItem a],[NotepadItem a])) deriving Eq

data ScrollType =
  NoScroller | LeftScroller | RightScroller | TopScroller | BottomScroller |
  LeftTopScroller | LeftBottomScroller | RightTopScroller | RightBottomScroller


-- state functionality --

addItemToState :: Notepad a -> NotepadItem a -> IO ()
addItemToState notepad@(Notepad _ _ state) item =
  do
    (notepaditems, selecteditems) <- getVar state
    setVar state (item : notepaditems, selecteditems)
    done

highlight :: NotepadItem a -> Canvas -> IO ()
highlight item@(NotepadItem img _ _ sel) cnv =
  do
    s <- getVar sel
    case s of
      Nothing -> do
                   (x,y) <- getPosition item
                   rect <- newRectangle [parent cnv, filling "grey", outline "white",
                                         coord [(x - 30, y - 30),
                                                (x + 30, y + 30)]]
                   putItemAtBottom rect
                   setVar sel (Just rect)
      Just _  -> done

deHighlight :: NotepadItem a -> IO ()
deHighlight (NotepadItem img _ _ sel) =
  do
    s <- getVar sel
    case s of
      Nothing   -> done
      Just rect -> do
                     destroy rect
                     setVar sel Nothing

selectItem :: Notepad a -> NotepadItem a -> IO ()
selectItem notepad@(Notepad cnv _ state) item =
  do
    deselectAll notepad
    highlight item cnv
    (notepaditems, selecteditems) <- getVar state
    setVar state (notepaditems, item : selecteditems)
    done

selectAnotherItem :: Notepad a -> NotepadItem a -> IO ()
selectAnotherItem notepad@(Notepad cnv _ state) item =
  do
    highlight item cnv
    (notepaditems, selecteditems) <- getVar state
    setVar state (notepaditems, item : selecteditems)
    done

deselectItem :: Notepad a -> NotepadItem a -> IO ()
deselectItem notepad@(Notepad _ _ state) item =
  do
    deHighlight item
    (notepaditems, selecteditems) <- getVar state
    setVar state (notepaditems, filter ((==) item) selecteditems)
    done

deselectAll :: Notepad a -> IO ()
deselectAll notepad@(Notepad _ _ state) =
  do
    (notepaditems, selecteditems) <- getVar state
    setVar state (notepaditems, [])
    mapM deHighlight selecteditems
    done

deleteItem :: Notepad a -> NotepadItem a -> IO ()
deleteItem notepad@(Notepad _ _ state) item =
  do
    (notepaditems, selecteditems) <- getVar state
    setVar state (filter ((==) item) notepaditems,
                  filter ((==) item) selecteditems)
    done

isSelected :: Notepad a -> NotepadItem a -> IO Bool
isSelected (Notepad _ _ state) item =
  do
    (_, selecteditems) <- getVar state
    return (any ((==) item) selecteditems)

selectItemsWithin :: Distance -> Distance -> Distance -> Distance ->
                     Notepad a -> IO ()
selectItemsWithin x0 y0 x1 y1 notepad@(Notepad _ _ state) =
  do
    (notepaditems, _) <- getVar state
    (let selectSingle item@(NotepadItem img _ _ _) =
           do
             ((x,y):_) <- getCoord img
             if (x >= x0 && x <= x1 && y >= y0 && y <= y1)
               then selectAnotherItem notepad item
               else done
         tryAll (item:items) =
           do
             selectSingle item
             tryAll items
         tryAll []           = done
     in tryAll notepaditems)


-- constructor --

newNotepad :: ScrollType -> [Config Box] -> [Config Canvas {- noch ändern!! -}] -> IO (Notepad a)
newNotepad scrolltype cnf_box cnf_cnv =
  do
    state <- newRVar ([],[])
    outer <- newVFBox cnf_box
    inner <- newHFBox [parent outer]
    cnv <- newCanvas (side AtLeft : parent inner : cnf_cnv)
    scb_v <- newScrollBar [parent inner, side AtRight, fill Vertical]
    scb_h <- newScrollBar [parent outer, fill Horizontal, orient Horizontal]
    cnv # scrollbar Vertical scb_v
    cnv # scrollbar Horizontal scb_h
    notepad <- return (Notepad cnv outer state)
--    interactor (select notepad)
    return notepad
  where select :: Notepad a -> InterActor-> IA ()
        select notepad@(Notepad cnv _ _) iact =
          mouseButtonPress cnv 1 >>>=
          \(x, y)-> do
                      deselectAll notepad
                      rect <- newRectangle [coord [(x,y),(x,y)], parent cnv]
                      debugMsg "rectangle created"
                      become iact (selecting notepad rect x y x y iact)
        selecting :: Notepad a -> Rectangle -> Distance -> Distance -> Distance -> Distance -> InterActor -> IA ()
        selecting notepad@(Notepad cnv _ _) rect x0 y0 x1 y1 iact =
             (mouseEvent cnv (Button1, Motion) >>>=
              \ ((x, y), _)-> do
                               rect # coord [(x0,y0),(x,y)]
                               debugMsg "resize"
-- leider zu langsam           selectItemsWithin x0 y0 x1 y1 notepad
                               become iact (selecting notepad rect x0 y0 x y iact))
          +> (mouseEvent cnv (ButtonRelease Nothing)
              >>> do
                    destroy rect
                    selectItemsWithin x0 y0 x1 y1 notepad
                    become iact (select notepad iact))


-- instances --

instance GUIObject (Notepad a) where
  toGUIObject (Notepad cnv box state) = toGUIObject box
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
  legalColourID = hasBackGroundColour

instance HasSize (Notepad a)

instance HasScroller (Notepad a)

instance HasPostscript (Notepad a)

instance ParentWidget (Notepad a) (NotepadItem a)


-- state export --

--getItems :: Notepad a -> IO [Config (NotepadItem a)]
