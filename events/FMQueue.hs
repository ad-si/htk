{- FMQueue handles finite maps of delete queues, so that we
   can implement EqGuard. -}
module FMQueue(
   FMQueue,
   emptyFMQueue, 
      -- :: FMQueue key contents
   addFMQueue, 
      -- :: Ord key => FMQueue key contents -> key -> contents -> 
      --    IO (FMQueue key contents,IO ())
      -- adds an item, returning the new queue and an invalidate action.
   removeFMQueue, 
      -- :: Ord key => FMQueue key contents -> key -> 
      -- IO (Maybe (contents,FMQueue key contents),FMQueue key contents)
   removeFMQueueAny 
      -- :: Ord key => FMQueue key contents ->
      -- IO (Maybe (key,contents,FMQueue key contents),FMQueue key contents)
   ) where

import FiniteMap

import DeleteQueue

data Ord key => FMQueue key contents = 
   FMQueue {
      dqMap :: FiniteMap key (DeleteQueue contents),
      cleanList :: [key]
      -- To clear out the map, we go through the keys in cleanList
      -- each time we add an item, and check for empty deleteQueues.
      }

emptyFMQueue :: Ord key => FMQueue key contents
emptyFMQueue = FMQueue {
   dqMap = emptyFM,
   cleanList = []
   }

addFMQueue :: Ord key => FMQueue key contents -> key -> contents -> 
   IO (FMQueue key contents,IO ())
addFMQueue fmQueue key contents =
  do
      let
         fmMap = (dqMap fmQueue) 
         deleteQueue = lookupWithDefaultFM fmMap emptyQueue key
      (deleteQueue2,invalidate) <-
         addQueue deleteQueue contents
      let
         fmMap2 = addToFM fmMap key deleteQueue2
         fmQueue2 = fmQueue {dqMap = fmMap2}
      fmQueue3 <- doClean fmQueue2
      return (fmQueue3,invalidate)

removeFMQueue :: Ord key => FMQueue key contents -> key -> 
   IO (Maybe (contents,FMQueue key contents),FMQueue key contents)
   -- The last returned item is the queue WITHOUT an item removed.
removeFMQueue fmQueue key=
   do
      let fmMap = dqMap fmQueue
      case lookupFM fmMap key of
         Nothing -> return (Nothing,fmQueue)
         Just deleteQueue ->
            do
               pop <- removeQueue deleteQueue
               case pop of
                  Nothing -> 
                     return (Nothing,fmQueue {dqMap = delFromFM fmMap key})
                  Just (contents,deleteQueue2,deleteQueue0) ->
                     do
                        let updateQueue queue = 
                              fmQueue {dqMap = addToFM fmMap key queue}
                        return (Just (contents,updateQueue deleteQueue2),
                           updateQueue deleteQueue0)

removeFMQueueAny :: Ord key => FMQueue key contents ->
   IO (Maybe (key,contents,FMQueue key contents),FMQueue key contents)
   -- Like removeFMQueue, but matches any key, and returns it.
removeFMQueueAny fmQueue =
   let 
      keyContents = keysFM (dqMap fmQueue)
   in
      doRemove fmQueue keyContents
   where
      doRemove fmQueue [] = return (Nothing,emptyFMQueue)
      doRemove fmQueue (key:keys)  =
         do
            tryRemove <- removeFMQueue fmQueue key
            case tryRemove of
               (Nothing,fmQueue0) -> doRemove fmQueue0 keys
               (Just (contents,fmQueue2),fmQueue0) ->
                  return (Just(key,contents,fmQueue2),fmQueue0)

doClean :: Ord key => FMQueue key contents -> IO (FMQueue key contents) 
doClean fmQueue =
   case cleanList fmQueue of 
      [] ->
         return (fmQueue {cleanList = keysFM (dqMap fmQueue)})
      toClean:nextCleanList ->
         do
            let fmMap = dqMap fmQueue
            nextMap <- case lookupFM fmMap toClean of
               Nothing -> return fmMap
               Just deleteQueue ->
                  do
                     isEmpty <- isEmptyQueue deleteQueue
                     case isEmpty of
                        Nothing -> return (delFromFM fmMap toClean)
                        Just cleaned -> return (addToFM fmMap toClean cleaned)
            return (FMQueue {
               dqMap = nextMap,
               cleanList = nextCleanList
               })
