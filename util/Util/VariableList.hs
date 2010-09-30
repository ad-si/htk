{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.VariableList(
   newVariableListFromSet, -- :: Ord a => VariableSetSource a -> VariableList a
   newVariableListFromList, -- :: Ord a => SimpleSource [a] -> VariableList a
   emptyVariableList, -- :: VariableList a
   singletonList, -- :: a -> VariableList a
   VariableList,
   ListDrawer(..),
   attachListOp, -- :: VariableList a -> ListDrawer a -> IO (IO ())
   coMapListDrawer, -- :: (a -> b) -> ListDrawer b pos -> ListDrawer a pos
   map2ListDrawer, -- :: (pos1 -> pos2) -> (pos2 -> pos2) ->
      -- ListDrawer b pos1 -> ListDrawer b pos2

   catVariableLists, -- :: VariableList a -> VariableList a -> VariableList a

   ) where

import Data.Maybe

import Data.IORef

import Util.Computation
import Util.Registry
import Util.Sources
import Util.Sink
import Util.VariableSet
import Util.Delayer
import Util.Myers

-- -----------------------------------------------------------------------
-- The types
-- -----------------------------------------------------------------------

data ListDrawer a pos = ListDrawer {
   -- A drawn list consists of a list of positions of type "pos", each of
   -- which optionally has a value of type "a" attached to it.  The value
   -- of type "a" is mutable.

   newPos :: Maybe pos -> Maybe a -> IO pos,
      -- newPos posOpt aOpt creates a new position.  If posOpt is
      -- Nothing this is at the beginning of the list, otherwise it is
      -- after the given position. aOpt specifies the value.
   setPos :: pos -> Maybe a -> IO (),
      -- Alter the value at pos.
   delPos :: pos -> IO (),
      -- Remove pos.  After this, pos should not be used.

   redraw :: IO ()
      -- This should be done after every group of updates, to ensure they
      -- physically happen.
   }

-- | Return the close action.
-- attachListOp :: ParallelExec -> VariableList a -> ListDrawer a -> IO (IO ())

data VariableList a = VariableList {
   attachListOp :: forall pos . ParallelExec -> ListDrawer a pos -> IO (IO ())
   }

-- -----------------------------------------------------------------------
-- The instances
-- -----------------------------------------------------------------------

instance Functor VariableList where
   fmap fn (VariableList {attachListOp = attachListOp0}) =
      let
         attachListOp1 parallelEx listDrawer =
            attachListOp0 parallelEx (coMapListDrawer fn listDrawer)
      in
         VariableList attachListOp1

coMapListDrawer :: (a -> b) -> ListDrawer b pos -> ListDrawer a pos
coMapListDrawer fn (ListDrawer {
      newPos = newPos0,setPos = setPos0,delPos = delPos0,redraw = redraw0}) =
   let
      newPos1 posOpt aOpt = newPos0 posOpt (fmap fn aOpt)
      setPos1 pos aOpt = setPos0 pos (fmap fn aOpt)
      delPos1 = delPos0
      redraw1 = redraw0
   in
      ListDrawer {
         newPos = newPos1,setPos = setPos1,delPos = delPos1,redraw = redraw1}

map2ListDrawer :: (pos1 -> pos2) -> (pos2 -> pos1) ->
      ListDrawer b pos1 -> ListDrawer b pos2
map2ListDrawer toPos2 toPos1 (ListDrawer {
      newPos = newPos1,setPos = setPos1,delPos = delPos1,redraw = redraw1}) =
   let
      newPos2 pos2Opt aOpt =
         do
            pos1 <- newPos1 (fmap toPos1 pos2Opt) aOpt
            return (toPos2 pos1)
      setPos2 pos2 aOpt = setPos1 (toPos1 pos2) aOpt
      delPos2 pos2 = delPos1 (toPos1 pos2)
      redraw2 = redraw1
   in
      ListDrawer {
         newPos = newPos2,setPos = setPos2,delPos = delPos2,redraw = redraw2}

instance HasAddDelayer (VariableList a) where
   addDelayer delayer (VariableList attachListOp0) =
      let
         attachListOp1 parallelX listDrawer0 =
            do
               listDrawer1 <- addDelayerIO delayer listDrawer0
               attachListOp0 parallelX listDrawer1
      in
         VariableList attachListOp1

instance HasAddDelayerIO (ListDrawer a pos) where
   addDelayerIO delayer listDrawer0 =
      do
         delayedAction <- newDelayedAction (redraw listDrawer0)
         let
            redraw1 = delayedAct delayer delayedAction

            listDrawer1 = listDrawer0 {redraw = redraw1}
         return listDrawer1

-- -----------------------------------------------------------------------
-- Constructing VariableList's from other things.
-- -----------------------------------------------------------------------

emptyVariableList :: VariableList a
emptyVariableList =
   let
      attachListOp _ _ = return done
   in
      VariableList attachListOp

singletonList :: forall a . a -> VariableList a
singletonList a =
   let
      attachListOp :: forall pos . ParallelExec -> ListDrawer a pos
        -> IO (IO ())
      attachListOp parallelX listDrawer =
         do
            parallelExec parallelX (
               do
                  newPos listDrawer Nothing (Just a)
                  done
               )
            return done
   in
      VariableList attachListOp

newVariableListFromSet :: forall a . Ord a => VariableSetSource a
    -> VariableList a
newVariableListFromSet (variableSetSource :: VariableSetSource a) =
   let
      attachListOp :: forall pos . ParallelExec -> ListDrawer a pos
        -> IO (IO ())
      attachListOp parallelX listDrawer =
         do
            (posRegistry :: Registry a pos) <- newRegistry

            groupingCount <- newIORef 0

            let
               updateFn :: VariableSetUpdate a -> IO ()
               updateFn (AddElement a) =
                  do
                     addElement a
                     groupCount <- readIORef groupingCount
                     when (groupCount == 0) (redraw listDrawer)
               updateFn (DelElement a) =
                  do
                     delElement a
                     groupCount <- readIORef groupingCount
                     when (groupCount == 0) (redraw listDrawer)
               updateFn BeginGroup = modifyIORef groupingCount (+1)
               updateFn EndGroup =
                  do
                     groupCount0 <- readIORef groupingCount
                     let
                        groupCount1 = groupCount0 - 1
                     writeIORef groupingCount groupCount1
                     when (groupCount1 == 0) (redraw listDrawer)

               initialElements :: [a] -> IO ()
               initialElements as =
                  do
                     mapM_ addElement as
                     redraw listDrawer

               addElement :: a -> IO ()
               addElement a =
                  do
                     pos <- newPos listDrawer Nothing (Just a)
                     setValue posRegistry a pos

               delElement :: a -> IO ()
               delElement a =
                  transformValue posRegistry a (\ posOpt -> case posOpt of
                     Just pos ->
                        do
                           delPos listDrawer pos
                           return (Nothing,())
                     )

            sinkID <- newSinkID

            (x,sink) <- addNewSinkWithInitial variableSetSource
               initialElements updateFn sinkID parallelX

            return (invalidate sinkID)
   in
      VariableList attachListOp

newVariableListFromList :: forall a . Ord a => SimpleSource [a]
    -> VariableList a
newVariableListFromList (simpleSource :: SimpleSource [a]) =
   let
      attachListOp :: forall pos . ParallelExec -> ListDrawer a pos
        -> IO (IO ())
      attachListOp parallelX listDrawer =
         do
            -- state stores the current a values and a list of the same length
            -- containing their pos values.
            (state :: IORef ([a],[pos])) <- newIORef ([],[])

            let
               updateList :: [a] -> IO ()
               updateList newAs =
                  do
                     (oldAs,oldPos) <- readIORef state
                     let
                        changes = diff2 oldAs newAs

                        oldAsPlus :: [(a,Bool)]
                           -- True means that it is in both lists
                        oldAsPlus = concat (map
                           (\ diffElement -> case diffElement of
                              InSecond _ -> []
                              InFirst l -> map (\ a -> (a,False)) l
                              InBoth l -> map (\ a -> (a,True)) l
                              )
                           changes
                           )

                        newAsPlus :: [(a,Bool)]
                           -- True means that it is in both lists
                        newAsPlus = concat (map
                           (\ diffElement -> case diffElement of
                              InFirst _ -> []
                              InSecond l -> map (\ a -> (a,False)) l
                              InBoth l -> map (\ a -> (a,True)) l
                              )
                           changes
                           )

                        -- (1) compute the delete actions
                        deleteAct = sequence_ (zipWith
                           (\ (oldA,isCommon) oldPos ->
                              if isCommon then done else
                                 delPos listDrawer oldPos
                              )
                           oldAsPlus oldPos
                           )

                        -- (2) compute the positions which are common
                        commonPos = catMaybes
                           (zipWith
                              (\ (oldA,isCommon) oldPos ->
                                 if isCommon
                                    then
                                       Just oldPos
                                    else
                                       Nothing
                                 )
                              oldAsPlus oldPos
                              )

                        -- (3) compute pairs (Maybe pos,[a]) where (Maybe pos)
                        -- is the last position before an insertion, [a] is
                        -- the insertion.
                        mkPairs :: Maybe pos -> [pos] -> [(a,Bool)]
                           -> [(Maybe pos,[a])] -> [(Maybe pos,[a])]
                        mkPairs lastPosOpt [] [] acc0 = acc0
                        mkPairs lastPosOpt poss0
                              (xs0@((a,isCommon):rest)) acc0 =
                           if isCommon
                              then
                                 case poss0 of
                                    pos:poss1 ->
                                       mkPairs (Just pos) poss1 rest acc0
                              else
                                 -- scan to next common element or end
                                 let
                                    getInsertion :: [(a,Bool)]
                                       -> ([a],[(a,Bool)])
                                    getInsertion [] = ([],[])
                                    getInsertion (xs@((_,True):_)) = ([],xs)
                                    getInsertion (((a,False):xs0)) =
                                       let
                                          (as,xs1) = getInsertion xs0
                                       in
                                          (a:as,xs1)

                                    (as,xs1) = getInsertion xs0
                                    acc1 = (lastPosOpt,as) : acc0
                                in
                                    case (poss0,xs1) of
                                       ([],[]) -> acc1
                                       (pos:pos1,((_,True):xs2)) ->
                                          mkPairs (Just pos) pos1 xs2 acc1

                        -- NB.  pairs is in reverse order.
                        pairs :: [(Maybe pos,[a])]
                        pairs = mkPairs Nothing commonPos newAsPlus []

                        -- (4) The add action, which also the
                        -- new positions.  The lists are all in reverse order.
                        addAct :: IO [[pos]]
                        addAct = mapM
                           (\ (posOpt,as) ->
                              mapM
                                 (\ a -> newPos listDrawer posOpt (Just a))
                                 (reverse as)
                              )
                           pairs

                     -- (5) Do the additions and deletions.
                     (newPosss0 :: [[pos]]) <- addAct
                     deleteAct
                     redraw listDrawer

                     let
                        -- (6) Compute all the new positions given the new
                        -- list + old common and new positions.
                        mkNewPos :: [(a,Bool)] -> [pos] -> [pos] -> [pos]
                           -> [pos]
                        mkNewPos [] [] [] posAcc = posAcc
                        mkNewPos ((_,isCommon):xs0) posOld posNew posAcc =
                           if isCommon
                              then
                                 case posOld of
                                    pos:posOld1 ->
                                       mkNewPos xs0 posOld1 posNew (pos:posAcc)
                              else
                                 case posNew of
                                    pos:posNew1 ->
                                       mkNewPos xs0 posOld posNew1 (pos:posAcc)

                        newPos =  mkNewPos newAsPlus commonPos
                           (reverse (concat newPosss0)) []

                     writeIORef state (newAs,reverse newPos)

            sinkID <- newSinkID

            addNewSinkWithInitial simpleSource updateList updateList sinkID
               parallelX

            return (invalidate sinkID)
   in
      VariableList attachListOp

-- -----------------------------------------------------------------------
-- Combinators
-- -----------------------------------------------------------------------

catVariableLists :: VariableList a -> VariableList a -> VariableList a
catVariableLists (VariableList attachListOp1) (VariableList attachListOp2) =
   let
      attachListOp parallelX listDrawer =
         do
            -- Separate the two using an unused position in the middle.
            middlePos <- newPos listDrawer Nothing Nothing
            let
               listDrawer1 = listDrawer

               newPos2 posOpt aOpt =
                  let
                     pos = fromMaybe middlePos posOpt
                  in
                     newPos listDrawer (Just pos) aOpt

               listDrawer2 = listDrawer {newPos = newPos2}

            destroy1 <- attachListOp1 parallelX listDrawer1
            destroy2 <- attachListOp2 parallelX listDrawer2
            return (destroy1 >> destroy2)
   in
      VariableList attachListOp
