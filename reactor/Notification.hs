{- Notification is intended for UniForM processes (possibly on
   different machines) to communicate when files are touched.
   A server is used like the one in uni/concurrency/server/Mainserver.hs.
   -}
module Notification(
   Notifier,
   mkNotifier, -- :: DescribesHost a => a -> IO Notifier
               -- connects
   notify, -- :: Notifier -> String -> IO()
           -- sends a notification
   isNotified   -- :: Notifier -> String -> IA ()
                -- when someone sends a notification with this String.
   ) where

import Object

import Thread
import Selective
import FileEV
import SocketEV

import Listener
import ExternalEvent
import EventBroker
import SIMClasses(Destructible(..))
import InfoBus

data Notifier =
   Notifier {
      oID :: ObjectID,
      handle :: HandleEV,
      eventBroker :: EventBroker () 
         -- we use an EventBroker to handle registrations.
      }

instance Object Notifier where
   objectID notifier = oID notifier

instance Destructible Notifier where
   destroy(Notifier{handle=handle}) =
      do
         writeFileEV handle "\0"
         closeFileEV handle

instance EventDesignator (Notifier,String) where
   toEventID (notifier,key) = EventID (objectID notifier) key

mkNotifier :: DescribesHost a => a -> IO Notifier
mkNotifier hostDesc =
   do
      oID <- newObject
      echoHost <- makeHost hostDesc
      echoPort <- makePort (11393::Int)
      handle <- connect echoHost echoPort
      eventBroker <- newEventBroker 
      let
         notifier = Notifier{oID = oID,handle=handle,eventBroker=eventBroker}
         reader = readFileEV handle
         readerThread =
            sync(
               reader >>>=
                  (\ key ->
                     do
                        dispatch eventBroker (notifier,key) () done
                        readerThread
                     )
            )
      forkIO readerThread
      registerTool notifier
      return notifier

notify :: Notifier -> String -> IO()
notify (Notifier{handle=handle}) key = writeFileEV handle key

isNotified :: Notifier -> String -> IA ()
isNotified notifier@(Notifier{eventBroker=eventBroker}) key =
   awaitEvent eventBroker (notifier,key) Oneway done done

