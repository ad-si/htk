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

module GenericBrowser (

  newGenericBrowser,
  GenericBrowser,
  GBObject(..)

) where

import HTk
import Core
import TreeList
import Notepad
import CItem
import Monad
import ReferenceVariables
import IOExts(unsafePerformIO)

class CItem o => GBObject o where
  getChildren :: o -> IO [o]
  isObjectNode :: o -> IO Bool

posRef :: Ref Position
posRef = unsafePerformIO (newRef (10, 10))

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

data GBObject o => GenericBrowser o =
  GenericBrowser { container :: Frame,
                   treelist :: TreeList o,
                   notepad  :: Notepad o }


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

newGenericBrowser :: (GBObject o, Container par) =>
                     par -> [o] -> [Config (GenericBrowser o)]  ->
                     IO (GenericBrowser o)
newGenericBrowser par rootobjs cnf =
  do fr <- newFrame par []
     let toTreeListObject obj = do ch <- getChildren obj
                                   let is_node = not (null ch)
                                   return (newTreeListObject obj
                                             (if is_node then Node
                                              else Leaf))
         cfun :: GBObject o => ChildrenFun o
         cfun tlobj = do ch <- getChildren (getTreeListObjectValue tlobj)
                         ch' <- filterM isObjectNode ch
                         mapM toTreeListObject ch'
     tl <- newTreeList fr cfun [] [bg "white"]
     pack tl [Side AtLeft, Fill Both, Expand On]
     np <- newNotepad fr Scrolled (12, 12) Nothing [bg "white",
                                                    size (500, 2000)]
     pack np [Side AtRight, Fill Both, Expand On]
     let gb = GenericBrowser { container = fr,
                               treelist = tl,
                               notepad = np }
     foldl (>>=) (return gb) cnf
     (tl_ev, _) <- bindTreeListEv tl
     let listenComponents = do ev <- tl_ev
                               always (case ev of
                                         TreeList.Selected mobj ->
                                           tlObjectSelected gb mobj
                                         _ -> done)
     spawnEvent (forever listenComponents)
     initBrowser gb rootobjs
     return gb

initBrowser :: GBObject o => GenericBrowser o -> [o] -> IO ()
initBrowser gb rootobjs =
  let addObject obj =
        do b <- isObjectNode obj
           if b then addTreeListRootObject (treelist gb)
                       (newTreeListObject obj Node)
                else done
  in mapM addObject rootobjs >> done

tlObjectSelected :: GBObject o => GenericBrowser o ->
                                  Maybe (TreeListObject o) -> IO ()
tlObjectSelected gb mtlobj =
  let addObject obj = do pos <- getPos
                         createNotepadItem obj (notepad gb) [position pos]
                         done
  in do case mtlobj of
          Just tlobj -> let obj = getTreeListObjectValue tlobj
                        in do clearNotepad (notepad gb)
                              resetPos
                              ch <- getChildren obj
                              ch' <- filterM
                                       (\obj -> do b <- isObjectNode obj
                                                   return (not b)) ch
                              putStrLn ("contains " ++ show(length ch') ++ " elements")
                              mapM addObject ch'
                              done
          _ -> done


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GBObject o => GUIObject (GenericBrowser o) where
  toGUIObject = toGUIObject . container

instance GBObject o => Widget (GenericBrowser o)
