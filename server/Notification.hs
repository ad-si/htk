{- Notification is intended for UniForM processes (possibly on
   different machines) to communicate when files are touched.
   We use the EchoService of uni/server.
   -}
module Notification(
   Notifier,
   mkNotifier, -- :: IO Notifier
               -- connects
   notify, -- :: Notifier -> String -> IO()
           -- sends a notification
   isNotified   -- :: Notifier -> String -> Event ()
                -- when someone sends a notification with this String.
   ) where

import IO

import Debug
import Object
import Binary

import Events
import GuardedEvents
import GuardedChannels
import EqGuard

import Thread
import HostsPorts
import EchoService
import CallServer

import Destructible
import InfoBus

data Notifier =
   Notifier {
      oID :: ObjectID,
      writeAction :: String -> IO (),
      closeAction :: IO(),
      
      eventChannel :: EqGuardedChannel String ()
      }

instance Object Notifier where
   objectID notifier = oID notifier

instance Destroyable Notifier where
   destroy(Notifier{closeAction=closeAction}) = closeAction


mkNotifier :: IO Notifier
mkNotifier =
   do
      server <- getDefaultHostPort
      let ?server = server in mkNotifier1

mkNotifier1 :: (?server :: HostPort) => IO Notifier
mkNotifier1 =
   do
      oID <- newObject
      (writeAction,receiveAction,closeAction,header :: String) <-
         connectBroadcast echoService
      eventChannel <- newEqGuardedChannel
      let
         notifier = Notifier{
            oID = oID,
            writeAction=(\ str -> writeAction str),
            closeAction=closeAction,
            eventChannel=eventChannel
            }

         readerThread =
            do
               key <- receiveAction
               sync (send eventChannel (key,()))
               readerThread            

      forkIO readerThread
      registerTool notifier
      return notifier

notify :: Notifier -> String -> IO()
notify (Notifier{writeAction=writeAction}) key = writeAction key

isNotified :: Notifier -> String -> Event ()
isNotified notifier@(Notifier{eventChannel=eventChannel}) key =
   toEvent (listen eventChannel |> Eq key) >> done


