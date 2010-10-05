-- | A generic data browser.
module HTk.Toolkit.GenericBrowser (

  newGenericBrowser,
  GenericBrowser,
  GBObject(..),

  GenericBrowserEvent(..),
  bindGenericBrowserEv

) where

import Control.Monad

import System.IO.Unsafe

import Util.Computation

import Events.Events
import Events.Channels

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk
import HTk.Kernel.Core
import HTk.Toolkit.TreeList as TreeList
import qualified HTk.Toolkit.Notepad as Notepad
import HTk.Toolkit.Notepad hiding (NotepadEvent(..))

-- | Browsed data needs to instantiate the class @CItem@.
class CItem o => GBObject o where
  getChildren :: o -> IO [o]
  isObjectNode :: o -> IO Bool

posRef :: Ref Position
posRef = unsafePerformIO (newRef (10, 10))
{-# NOINLINE posRef #-}

resetPos :: IO ()
resetPos = setRef posRef (40, 40)

max_x :: Distance
max_x = 350

dx :: Distance
dx = 60

dy :: Distance
dy = 50

getPos :: IO Position
getPos = do pos@(x,y) <- getRef posRef
            let nupos = if (x + dx > max_x) then (40, y + dy)
                                            else (x + dx, y)
            setRef posRef nupos
            return pos


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @GenericBrowser@ datatype.
data GBObject o => GenericBrowser o =
  GenericBrowser { container :: Frame,
                   treelist :: TreeList o,
                   notepad  :: Notepad o,

                   -- event queue
                   event_queue ::
                     Ref (Maybe (Channel (GenericBrowserEvent o))) }


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Constructs a new generic browser and returns a handler.
newGenericBrowser :: (GBObject o, Container par) =>
   par
   -- ^ the parent widget (which has to be a container
   -- widget).
   -> [o]
   -- ^ the list of top level objects.
   -> [Config (GenericBrowser o)]
   -- ^ the list of configuration options for this
   -- generic browser.
   ->
   IO (GenericBrowser o)
   -- ^ A generic browser.
newGenericBrowser par rootobjs cnf =
  do fr <- newFrame par []
     let toTreeListObject obj = do --ch <- getChildren obj
                                   --let is_node = not (null ch)
                                   is_node <- isObjectNode obj
                                   return (newTreeListObject obj
                                             (if is_node then Node
                                              else Leaf))
         cfun :: GBObject o => ChildrenFun o
         cfun tlobj = do ch <- getChildren (getTreeListObjectValue tlobj)
                         ch' <- filterM isObjectNode ch
                         mapM toTreeListObject ch'
     tl <- newTreeList fr cfun [] [bg "white"]
     pack tl [Side AtLeft, Fill Both, Expand On]
     np <- newNotepad fr Scrolled (12, 12) Nothing [bg "white" {-,
                                                    size (500, 2000)-}]
     pack np [Side AtRight, Fill Both, Expand On]
     evq <- newRef Nothing
     let gb = GenericBrowser { container = fr,
                               treelist = tl,
                               notepad = np,
                               event_queue = evq }
     foldl (>>=) (return gb) cnf
     (tl_ev, _) <- bindTreeListEv tl
     (np_ev, _) <- bindNotepadEv np
     let listenComponents = (do ev <- tl_ev
                                always (case ev of
                                          TreeList.Selected mobj ->
                                            tlObjectSelected gb mobj
                                          TreeList.Focused (mobj, _) ->
                                            tlObjectFocused gb mobj
                                        )) +>
                            (do ev <- np_ev
                                always (case ev of
                                          Notepad.Dropped
                                            (npobj, npobjs) ->
                                            npItemsDropped gb
                                              (npobj, npobjs)
                                          Notepad.Selected npobj ->
                                            npItemSelected gb npobj
                                          Notepad.Deselected npobj ->
                                            npItemDeselected gb npobj
                                          Notepad.Doubleclick npobj ->
                                            npItemDoubleclick gb npobj
                                          Notepad.Rightclick npobjs ->
                                            npItemsRightclick gb npobjs
                                          _ -> done))
     _ <- spawnEvent (forever listenComponents)
     rootobjs' <- filterM isObjectNode rootobjs
     initBrowser gb rootobjs'
     return gb

{-
containsSubNodes :: GBObject o => o -> IO Bool
containsSubNodes obj =
  let containsSubNodes' (obj : objs) =
        do b <- isObjectNode obj
           if b then return True else containsSubNodes' objs
      containsSubNodes' _ = return False
  in do ch <- getChildren obj
        containsSubNodes' ch
-}

-- Initializes the browser.
initBrowser :: GBObject o => GenericBrowser o -> [o] -> IO ()
initBrowser gb rootobjs =
  let addObject obj =
        do b <- isObjectNode obj
           if b then addTreeListRootObject (treelist gb)
                       (newTreeListObject obj Node)
                else done
  in mapM addObject rootobjs >> done

-- Treelist selection event handler.
tlObjectSelected :: GBObject o => GenericBrowser o ->
                                  Maybe (TreeListObject o) -> IO ()
tlObjectSelected gb mtlobj =
  let addObject obj = do pos <- getPos
                         createNotepadItem obj (notepad gb) False
                                           [position pos]
                         done
  in do case mtlobj of
          Just tlobj -> let obj = getTreeListObjectValue tlobj
                        in do clearNotepad (notepad gb)
                              resetPos
                              sendEv gb (SelectedInTreeList (Just obj))
                              ch <- getChildren obj
                              ch' <- filterM
                                       (\obj -> do b <- isObjectNode obj
                                                   return (not b)) ch
                              mapM addObject ch'
                              updNotepadScrollRegion (notepad gb)
                              done
          _ -> sendEv gb (SelectedInTreeList Nothing)

-- Treelist focus event handler.
tlObjectFocused :: GBObject o => GenericBrowser o ->
                                 Maybe (TreeListObject o) -> IO ()
tlObjectFocused gb mtlobj =
  case mtlobj of
    Just tlobj -> let obj = getTreeListObjectValue tlobj
                  in sendEv gb (FocusedInTreeList (Just obj))
    _ -> sendEv gb (FocusedInTreeList Nothing)

-- Notepad drop event handler.
npItemsDropped :: GBObject o => GenericBrowser o ->
                                (NotepadItem o, [NotepadItem o]) -> IO ()
npItemsDropped gb (npobj, npobjs) =
  do obj <- getItemValue npobj
     objs <- mapM getItemValue npobjs
     sendEv gb (Dropped (obj, objs))

-- Notepad selection event handler.
npItemSelected :: GBObject o => GenericBrowser o -> NotepadItem o -> IO ()
npItemSelected gb npobj = do obj <- getItemValue npobj
                             sendEv gb (SelectedInNotepad obj)

-- Notepad deselection event handler.
npItemDeselected :: GBObject o => GenericBrowser o -> NotepadItem o ->
                                  IO ()
npItemDeselected gb npobj = do obj <- getItemValue npobj
                               sendEv gb (DeselectedInNotepad obj)

-- Notepad doubleclick event handler.
npItemDoubleclick :: GBObject o => GenericBrowser o -> NotepadItem o ->
                                   IO ()
npItemDoubleclick gb npobj = do obj <- getItemValue npobj
                                sendEv gb (Doubleclick obj)

-- Notepad rightclick event handler.
npItemsRightclick :: GBObject o => GenericBrowser o -> [NotepadItem o] ->
                                   IO ()
npItemsRightclick gb npobjs = do objs <- mapM getItemValue npobjs
                                 sendEv gb
                                   (Rightclick objs)


-- -----------------------------------------------------------------------
-- events
-- -----------------------------------------------------------------------

data GBObject o => GenericBrowserEvent o =
    SelectedInTreeList (Maybe o)
  | FocusedInTreeList (Maybe o)
  | Dropped (o, [o])
  | SelectedInNotepad o
  | DeselectedInNotepad o
  | Doubleclick o
  | Rightclick [o]

-- send an event if bound
sendEv :: GBObject o => GenericBrowser o -> GenericBrowserEvent o -> IO ()
sendEv gb ev =
  do
    mch <- getRef (event_queue gb)
    case mch of
      Just ch -> syncNoWait (send ch ev)
      _ -> done

-- | Binds a listener for generic browser events to the tree list and
-- returns a corresponding event and an unbind action.
bindGenericBrowserEv :: GBObject o => GenericBrowser o
   -- ^ the concerned generic browser.
   ->
   IO (Event (GenericBrowserEvent o),
   IO ())
   -- ^ A pair of (event, unbind action).
bindGenericBrowserEv gb =
  do
    ch <- newChannel
    setRef (event_queue gb) (Just ch)
    return (receive ch, setRef (event_queue gb) Nothing)


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

-- | Internal.
instance GBObject o => GUIObject (GenericBrowser o) where
  toGUIObject = toGUIObject . container
  cname _ = "GenericBrowser"

-- | Internal.
instance GBObject o => Widget (GenericBrowser o)
