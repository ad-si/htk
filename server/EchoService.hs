{- This is a very simple service which rebroadcasts all lines it
   receives to all currently connected clients. -}
module EchoService(
   echoService, -- :: pass to connectBroadcast to call server
   echoServiceWrapped -- :: pass to runServer to run server.
   ) where

import BinaryIO

import ServiceClass

newtype EchoState = EchoState ()
-- The Echo service has no state but we create a new type for it to
-- distinguish it from all the other instances of ServiceClass.

echoService = serviceArg :: (ReadShow String,ReadShow String,EchoState)
echoServiceWrapped = Service echoService 

instance ServiceClass (ReadShow String) (ReadShow String) EchoState where
   serviceId _ = "Echo"
   serviceMode _ = Broadcast
   initialState _ = return(EchoState ())
   handleRequest _ _ (inVal,state) = return (inVal,state)
   getBackupDelay _ = return BackupNever
   





