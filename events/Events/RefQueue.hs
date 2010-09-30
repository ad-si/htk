{-# LANGUAGE ScopedTypeVariables #-}

-- | RefQueue are standard non-functional
-- queues using pointers (aka IORefs).  Events can be deleted asynchronously,
-- but this is done only by nulling the cell they are contained in, otherwise
-- we would need to double-link.   Other operations, IE the push and pop
-- function must not occur on the same queue concurrently.
--
-- Although the queues are impure, we return the new queue to be used
-- in future after push and search operations.
--
-- RefQueue are intended for use for queues of guarded strings,
-- hence the specialised implementation.
module Events.RefQueue(
   RefQueue,
   newRefQueue, -- :: IO (RefQueue a)
   pushRefQueue, -- :: RefQueue a -> a -> IO (RefQueue a,IO ())
      -- place an item on the queue.  The action argument deletes tje
      -- item.
   searchRefQueue, -- :: RefQueue a -> (a -> Bool) ->
      -- IO (Maybe (a,IO (RefQueue a)),RefQueue a)
      -- searchRefQueue searchs a queue from the front
      -- for an item matching the given condition.  If returns (second
      -- argument) the new queue with (if the item was found) the item
      -- deleted.  The first argument then contains a and a RefQueue
      -- which puts a back in the queue where it was PROVIDED THAT
      -- no operations were done on the queue inbetween except
      -- for pushRefQueue, action arguments returned from it, and
      -- searchRefQueue with the same function as the one provided.
   ) where


import Data.IORef

import Util.Computation(done)

import Events.Cells

type ListPtr a = IORef (Maybe (ListItem a))

data ListItem a = ListItem ! (Cell a) ! (ListPtr a)

data RefQueue a = RefQueue {
   front :: ! (ListPtr a),
   backRef :: ! (IORef (ListPtr a)),
   sinceClean :: ! Int
   }

newRefQueue :: IO (RefQueue a)
newRefQueue =
   do
      ioRef <- newIORef Nothing
      backRef <- newIORef ioRef
      return (RefQueue {front = ioRef,backRef = backRef,sinceClean = 0})

pushRefQueue :: RefQueue a -> a -> IO (RefQueue a,IO ())
pushRefQueue (refQueue@RefQueue {backRef = backRef,sinceClean = sinceClean})
      val =
   do
      cell <- newCell val
      newBack <- newIORef Nothing
      oldBack <- readIORef backRef
      writeIORef oldBack (Just (ListItem cell newBack))
      writeIORef backRef newBack
      let
         refQueue2 = refQueue {sinceClean = sinceClean+1}
      refQueue3 <- if sinceClean >= 10
         then
            do
               (cleanedQueue,_) <- cleanRefQueue refQueue2
               return cleanedQueue
         else
            return refQueue2
      return (refQueue3,emptyCell cell)
{-# INLINE pushRefQueue #-}

searchRefQueue :: RefQueue a -> (a -> Bool)
   -> IO (Maybe (a,IO (RefQueue a)),RefQueue a)
searchRefQueue (refQueue :: RefQueue a) (filter :: a -> Bool) =
   do
      (refQueue2,listItem') <- cleanRefQueue refQueue
      case listItem' of
         Nothing -> return (Nothing,refQueue2)
         Just listItem ->
            do
               valFound' <- searchPtr (front refQueue2) listItem
               let
                  valAndAct' = fmap
                     (\ (b,act) -> (b,(act >> return refQueue2)))
                     valFound'
               return (valAndAct',refQueue2)
   where
      switchBack :: ListPtr a -> ListPtr a -> IO ()
      -- switchBack oldPtr newPtr indicates that a cell has
      -- just moved from oldPtr to newPtr, and updates backRef
      -- if necessary
      switchBack oldPtr newPtr =
         do
            oldBack <- readIORef (backRef refQueue)
            if (oldBack == oldPtr)
               then
                  writeIORef (backRef refQueue) newPtr
               else
                  done

      searchPtr :: ListPtr a -> ListItem a
         -> IO (Maybe (a,IO ()))
      -- The second argument is (Just ptr) to make ptr the new
      -- backref.
      searchPtr ptr (listItem0 @ (ListItem cell next))  =
         do
            cellContents <- inspectCell cell
            case cellContents of
               Nothing ->
                  do
                     -- Unlink this item from the list
                     listItem' <- readIORef next
                     writeIORef ptr listItem'
                     switchBack next ptr
                     case listItem' of
                        Nothing -> return Nothing
                        Just listItem -> searchPtr next listItem
               Just a ->
                  do
                     if filter a
                        then
                           do
                              -- Unlink this item from the list
                              listItem' <- readIORef next
                              writeIORef ptr listItem'
                              switchBack next ptr
                              let
                                 relink =
                                    do
                                       switchBack ptr next
                                       writeIORef ptr (Just listItem0)
                              return (Just(a,relink))
                        else
                           do
                              listItem' <- readIORef next
                              case listItem' of
                                 Nothing -> return Nothing
                                 Just listItem -> searchPtr next listItem
{-# INLINE searchRefQueue #-}

cleanRefQueue :: RefQueue a -> IO (RefQueue a,Maybe (ListItem a))
-- cleanRefQueue cleans items from the front of the queue, and returns
-- the front list element, if any.
cleanRefQueue refQueue =
   do
      (newFront,listItem') <- cleanQueue (front refQueue)
      return (refQueue {front = newFront,sinceClean=0},listItem')
   where
      cleanQueue :: ListPtr a -> IO (ListPtr a,Maybe (ListItem a))
      cleanQueue ptr =
         do
            contents <- readIORef ptr
            case contents of
               Nothing -> return (ptr,Nothing)
               Just (listItem @ (ListItem cell next)) ->
                  do
                     cellContents <- inspectCell cell
                     case cellContents of
                        Nothing -> cleanQueue next
                        Just _ -> return (ptr,Just listItem)


