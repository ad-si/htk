{- Notification is intended for UniForM processes (possibly on
   different machines) to communicate when files are touched.
   We use the EchoService of uni/server.
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

import IO

import Object

import Thread
import Selective
import SocketEV(DescribesHost)

import EchoService
import CallServer

import Listener
import ExternalEvent
import EventBroker
import SIMClasses(Destructible(..))
import InfoBus

data Notifier =
   Notifier {
      oID :: ObjectID,
      writeAction :: String -> IO (),
      closeAction :: IO(),
      eventBroker :: EventBroker () 
         -- we use an EventBroker to handle registrations.
      }

instance Object Notifier where
   objectID notifier = oID notifier

instance Destructible Notifier where
   destroy(Notifier{closeAction=closeAction}) = closeAction

instance EventDesignator (Notifier,String) where
   toEventID (notifier,key) = EventID (objectID notifier) key

mkNotifier :: DescribesHost a => a -> IO Notifier
mkNotifier hostDesc =
   do
      oID <- newObject
      (writeAction,receiveAction,closeAction) <-
         connectBroadcast echoService hostDesc (11393::Int)
      eventBroker <- newEventBroker 
      let
         notifier = Notifier{
            oID = oID,
            writeAction=writeAction,
            closeAction=closeAction,
            eventBroker=eventBroker
            }
         readerThread =
            do
               key <- receiveAction
               dispatch eventBroker (notifier,key) () done
            
      forkIO readerThread
      registerTool notifier
      return notifier

notify :: Notifier -> String -> IO()
notify (Notifier{writeAction=writeAction}) key = writeAction key

isNotified :: Notifier -> String -> IA ()
isNotified notifier@(Notifier{eventBroker=eventBroker}) key =
   awaitEvent eventBroker (notifier,key) Oneway done done


