{- In Server we implement a general framework for a server, which
   is supposed to make it easier to share information between 
   computers, in a similar method to inetd.  Unlike inetd however
   each client computer maintains a session with the server.  Also
   we only use one port for all services, though an additional
   integer is sent at the start of opening a connection to specify
   which service is to be used.

   To specify a new service you need to define an instance of
   the class ServerClass, defined in the module of that name.
   -}
module Server(
   runServer 
   -- :: DescribesPort port => port -> [Service] -> IO () 
   -- runs a server on a particular port.  It does not return until
   -- the server is interrupted in some way.
   ) where

import IO
import List

import Exception
import FiniteMap
import Socket hiding (PortID(..))

import Computation(done)
import Debug(debug)

import Concurrent
import Thread
import Object
import SocketEV(getPortNumber,DescribesPort(..))

import ServiceClass

data ClientData = ClientData {
   oid :: ObjectID,
   handle :: Handle
   }

instance Object ClientData where
   objectID (ClientData{oid=oid}) = oid
-- Actually that instance is probably unnecessary; I only put it in
-- in the hope that it would make ClientData an instance of Eq, but
-- it doesn't.

instance Eq ClientData where
   (==) clientData1 clientData2 =
      (oid clientData1) == (oid clientData2)

data ServiceData = ServiceData {
--   clients :: MVar [ClientData],
   newClientAction :: Handle -> IO (), 
   disconnect :: IO ()
   }

runServer :: DescribesPort port => port -> [Service] -> IO ()
runServer portDesc serviceList =
   do
------------------------------------------------------------------------
-- This obscenely long function is divided as follows:
------------------------------------------------------------------------
      (serviceDataList :: [(String,ServiceData)]) <-
         mapM
            (\ (Service service) -> 
               do
------------------------------------------------------------------------
-- For each service to provide . . .
-- (1) set up initial variables
------------------------------------------------------------------------
                  let
                     serviceKey = serviceId service

                  clients <- newMVar []

                  initial <- initialState service 
                  stateMVar <- newMVar initial

------------------------------------------------------------------------
-- (2) set up backups.
------------------------------------------------------------------------

                  backupDelay <- getBackupDelay service
                  let
                     doBackup =
                        do
                           state <- takeMVar stateMVar
                           putMVar stateMVar state
                           backupAction service state
                     -- Deal with BackupEvery-type backups
                     backupTick = -- do after each update
                        case backupDelay of
                           BackupEvery howOft ->
                              do 
                                 counter <- newMVar howOft
                                 let
                                    backupTick =
                                       do
                                          cVal <- takeMVar counter
                                          if cVal <= 0 
                                             then
                                                do
                                                   doBackup
                                                   putMVar counter howOft
                                             else
                                                putMVar counter (cVal - 1)
                                 return backupTick
  
                           _ -> return done

                     -- Deal with BackupAfter type backups
                  case backupDelay of
                     BackupAfter delayTime ->
                        let
                           backupThread =
                              do
                                 delay delayTime
                                 backupThread
                        in
                           do
                              forkIO backupThread
                              done
                     _ -> done

------------------------------------------------------------------------
-- (3) Define newClientAction to be done for each new client
------------------------------------------------------------------------
                  let
                     newClientAction handle =
                        do
                           debug "Registering client"
                           oid <- newObject
                           let
                              clientData = ClientData {oid=oid,handle=handle}

                              deleteClient :: IO ()
                              deleteClient =
                                 do
                                    oldClients <- takeMVar clients
                                    putMVar clients 
                                       (delete clientData oldClients)

                              protect :: IO () -> IO result -> IO result
                              protect cleanUp toDo =
                              -- Execute toDo, returning its result.
                              -- if toDo raises an exception, first 
                              -- deleteClient, then do cleanUp, then pass
                              -- on the exception
                                 do
                                    result <- tryAllIO toDo
                                    case result of
                                       Right correct -> return correct
                                       Left exception ->
                                          do
                                             deleteClient
                                             cleanUp
                                             throw exception
                           -- add client to client list
                           oldClients <- takeMVar clients
                           putMVar clients (clientData:oldClients)
                           -- clientReadAction is basically thread for 
                           -- reading client output
                           let
                              clientReadAction :: IO ()
                              clientReadAction =
                                 do
                                    debug "Waiting"
                                    inLine <- protect done (hGetLine handle)
                                    debug ("Server read "++inLine)
                                    let
                                       input = read inLine
                                    oldState <- takeMVar stateMVar 
                                    (output,newState) <- 
                                       protect
                                          (putMVar stateMVar oldState)
                                          (handleRequest service 
                                             (input,oldState))
                                    putMVar stateMVar newState
                                    backupTick
                                    let
                                       outLine = show output
                                    protect done (hPutStrLn handle outLine)
                                    clientReadAction
                           -- however it needs a wrapper so harmless
                           -- (EOF) errors don't cause any trouble.
                           forkIO(
                              do
                                 Left exception <- tryAllIO clientReadAction
                                 -- clientReadAction cannot return otherwise
                                 let
                                    isHarmless (IOException ioError) = 
                                       isEOFError ioError
                                    isHarmless _ = False
                                 if isHarmless exception
                                    then
                                       done
                                    else
                                       do
                                          debug("Server error on "++
                                             (show serviceKey)
                                             )
                                          debug exception
                              ) -- end of forkIO
                           done    
                     -- end of definition of newClientAction
------------------------------------------------------------------------
-- (4) Define action to be done at end of server.
--     The main purpose of this is to backup, since it's assumed
--     that the program will shortly afterwards be halted.
--     If the program is going to continue, it would be a good
--     idea to kill all the threads.
------------------------------------------------------------------------
                     disconnect = 
                        -- what to do at end of program with this 
                        -- server
                        do
                           -- backup and stop anyone updating state.
                           state <- takeMVar stateMVar
                           backupAction service state
                           -- close all clients
                           oldClients <- takeMVar clients
                           putMVar clients [] -- probably not necessary
                           sequence_
                              (map
                                 (\ ClientData{handle=handle} ->
                                    hClose handle
                                    )
                                 oldClients
                                 )
                  -- Now construct (key,new service data) 
                  return (serviceKey,
                     ServiceData{
                        newClientAction=newClientAction,
                        disconnect=disconnect
                        }
                     )
               ) -- end of map function   
            serviceList

------------------------------------------------------------------------
-- (5) That defines the server.  Now construct server value,
-- and set up thread to monitor new connections and pass them onto
-- the appropriate newClientAction
------------------------------------------------------------------------
      let
         serviceMap = listToFM serviceDataList
 
      portNumber <- getPortNumber portDesc
      socket <- listenOn portNumber
      let
         lookupService handle =
         -- lookup service number and call appropriate
         -- newClientAction or possibly raise an error.
            do
               serviceKey <- hGetLine handle
               case lookupFM serviceMap serviceKey of
                  Just(ServiceData{newClientAction = newClientAction}) ->
                     newClientAction handle
                  Nothing -> error ("Service "++serviceKey++" not known")
         serverAction =
            do
               (handle,_,_) <- accept socket
               registration <- tryAllIO (lookupService handle)
               case registration of
                  Right () -> done
                  Left exception ->
                     do
                        debug "Failed service registration:"
                        debug exception   
               serverAction  

      tryAllIO serverAction
      -- disconnect everything
------------------------------------------------------------------------
-- (6) serverAction ended mysteriously, perhaps someone has interrupted
--     the program from outside.  Disconnect everything and quit.
------------------------------------------------------------------------
      sequence_
         (map
            (\ (_,ServiceData {disconnect = disconnect}) -> disconnect)
            (fmToList serviceMap)
            )

