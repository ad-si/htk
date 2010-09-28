-- | A DeleteQueue is a queue where entries can be deleted by an
-- IO action.  This is a fairly specialised implementation, designed
-- for event handling.
--
-- Queue entries are either active or invalid.  Once invalid,
-- removeQueue will not return them, but they still take up (a little) memory.
--
-- addQueue, removeQueue, isEmptyQueue, cleanQueue all take a delete queue
-- as argument.  We assume that this argument is not used again.
--
-- Either removeQueue or isEmptyQueue or cleanQueue should be run
-- occasionally, to remove invalid entries.
module Events.DeleteQueue(
   DeleteQueue,
   emptyQueue, -- :: DeleteQueue v
   addQueue, -- :: DeleteQueue v -> v -> IO (DeleteQueue v,IO ())
   -- add an item to the queue, returning the new queue + a new action which
   -- will invalidate that item.
   removeQueue,
   -- :: DeleteQueue v -> IO (Maybe (v,DeleteQueue v,DeleteQueue v))
   -- returns the next active element of the queue, and the succeeding
   -- queue.  3rd result is a queue which is identical to the original
   -- queue.
   isEmptyQueue, -- :: DeleteQueue v -> IO (Maybe (DeleteQueue v))
   -- If queue has active entries, returns it, otherwise return Nothing.
   cleanQueue, -- :: DeleteQueue v -> IO (DeleteQueue v)
   ) where

import Util.Queue
import Events.Cells

newtype DeleteQueue v = DeleteQueue (Queue (Cell v))

emptyQueue :: DeleteQueue v
emptyQueue = DeleteQueue emptyQ

addQueue :: DeleteQueue v -> v -> IO (DeleteQueue v,IO ())
addQueue (DeleteQueue queue) v =
   do
      cell <- newCell v
      let deleteQueue1 = DeleteQueue (insertQ queue cell)
      return (deleteQueue1,emptyCell cell)

cleanQueue :: DeleteQueue v -> IO (DeleteQueue v)
-- cleanQueue pops empty cells from the front of the queue as long as possible
cleanQueue deleteQueue@(DeleteQueue queue) =
   case removeQ queue of
      Nothing -> return deleteQueue
      Just (cell,queue2) ->
         do
            cellContents <- inspectCell cell
            case cellContents of
               Nothing -> cleanQueue (DeleteQueue queue2)
               Just _ -> return (DeleteQueue (insertAtEndQ queue2 cell))


isEmptyQueue :: DeleteQueue v -> IO (Maybe (DeleteQueue v))
-- isEmptyQueue is like cleanQueue, but if the queue is empty returns Nothing.
isEmptyQueue deleteQueue@(DeleteQueue queue) =
   case removeQ queue of
      Nothing -> return Nothing
      Just (cell,queue2) ->
         do
            cellContents <- inspectCell cell
            case cellContents of
               Nothing -> isEmptyQueue (DeleteQueue queue2)
               Just _ ->
                  return (Just (DeleteQueue (insertAtEndQ queue2 cell)))

removeQueue :: DeleteQueue v -> IO (Maybe (v,DeleteQueue v,DeleteQueue v))
removeQueue (DeleteQueue queue) =
   case removeQ queue of
      Nothing -> return Nothing
      Just (cell,queue2) ->
         do
            vOpt <- inspectCell cell
            case vOpt of
               Nothing -> removeQueue (DeleteQueue queue2)
               Just v ->
                  return (Just(v,DeleteQueue queue2,
                     DeleteQueue(insertAtEndQ queue2 cell)))

