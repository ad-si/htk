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

import IO

import Object

import Thread
import Selective
import SocketEV

import Listener
import ExternalEvent
import EventBroker
import SIMClasses(Destructible(..))
import InfoBus

data Notifier =
   Notifier {
      oID :: ObjectID,
      handle :: Handle,
      eventBroker :: EventBroker () 
         -- we use an EventBroker to handle registrations.
      }

instance Object Notifier where
   objectID notifier = oID notifier

instance Destructible Notifier where
   destroy(Notifier{handle=handle}) =
      do
         hPutStrLn handle "\0"
         hClose handle

instance EventDesignator (Notifier,String) where
   toEventID (notifier,key) = EventID (objectID notifier) key

mkNotifier :: DescribesHost a => a -> IO Notifier
mkNotifier hostDesc =
   do
      oID <- newObject
      handle <- connect hostDesc (11393::Int)
      eventBroker <- newEventBroker 
      let
         notifier = Notifier{oID = oID,handle=handle,eventBroker=eventBroker}
         readerThread =
            do
               key <- hGetLine handle
               dispatch eventBroker (notifier,key) () done
            
      forkIO readerThread
      registerTool notifier
      return notifier

notify :: Notifier -> String -> IO()
notify (Notifier{handle=handle}) key = hPutStrLn handle key

isNotified :: Notifier -> String -> IA ()
isNotified notifier@(Notifier{eventBroker=eventBroker}) key =
   awaitEvent eventBroker (notifier,key) Oneway done done

