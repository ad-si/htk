#if (__GLASGOW_HASKELL__ >= 503)
#define NEW_GHC 
#else
#undef NEW_GHC
#endif

#ifndef NEW_GHC 
{-# OPTIONS -#include "default_options.h" #-}
#endif /* NEW_GHC */

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
   -- :: [Service] -> IO () 
   -- runs a server on the standard port.  It does not return until
   -- the server is interrupted in some way.
   ) where

import IO
import List

#ifdef NEW_GHC
import Exception hiding (handle)
#else
import Exception
#endif

import FiniteMap
import Socket hiding (PortID(..))
import Posix

import Computation(done)
import WBFiles(getPort)
import Debug

import Concurrent
import Thread
import Object
import HostsPorts

import ServiceClass

data ClientData = ClientData {
   oid :: ObjectID,
   handle :: Handle
   }

instance Object ClientData where
   objectID (ClientData{oid=oid}) = oid
-- Actually that instance is probably unnecessary; I only put it in-- in the hope that it would make ClientData an instance of Eq, but
-- it doesn't.

instance Eq ClientData where
   (==) clientData1 clientData2 =
      (oid clientData1) == (oid clientData2)

data ServiceData = ServiceData {
--   clients :: MVar [ClientData],
   newClientAction :: Handle -> IO (), 
   disconnect :: IO ()
   }

runServer :: [Service] -> IO ()
runServer serviceList =
   do
      portDesc <- getPort
      installHandler sigPIPE Ignore Nothing
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
-- Note on concurrency.  We have two MVars, stateMVar and clients.
-- The rule is, don't take from clients unless stateMVar is empty.
-- Thus we are locking operations on stateMVar.
------------------------------------------------------------------------

                  let
                     -- This should not be done unless stateMVar
                     -- is empty.
                     deleteClient :: ClientData -> IO ()
                     deleteClient clientData =
                        do
                           oldClients <- takeMVar clients
                           putMVar clients 
                              (delete clientData oldClients)

                     -- This should not be done unless stateMVar
                     -- is empty.
                     broadcastAction :: ClientData -> String -> IO ()
                     -- The first argument is the client who initiated
                     -- the request; the second is the message
                     broadcastAction =
                        case serviceMode service of
                           Reply ->
                              (\ clientData message -> 
                                 hPutStrLnFlush (handle clientData) message
                                 )
                           Broadcast ->
                              (\ _ message ->
                                 do
                                    clientList <- readMVar clients
                                    broadcastToClients clientList message
                                 )
                           BroadcastOther ->
                              (\ clientData message ->
                                 do
                                    clientList <- readMVar clients
                                    let
                                       otherClients =
                                          filter (/= clientData) clientList
                                    broadcastToClients otherClients message
                                 )

                     -- This should not be done unless stateMVar
                     -- is empty.
                     broadcastToClients :: [ClientData] -> String -> IO ()
                     -- Broadcasts the string to all clients listed
                     broadcastToClients clientList message =
                        sequence_ (
                           map
                              (\ clientData ->
                                 do 
                                    -- If fail, delete clientData
                                    success <- IO.try(hPutStrLnFlush
                                       (handle clientData) message
                                       )
                                    case success of
                                       Left error ->
                                          deleteClient clientData
                                       Right () -> done
                                 ) 
                              clientList
                           )

------------------------------------------------------------------------
-- (2) set up backups.
------------------------------------------------------------------------
                  backupDelay <- getBackupDelay service
                  let
                     doBackup =
                        do
                           state <- takeMVar stateMVar
                           backupAction service state
                           putMVar stateMVar state
                     -- Deal with BackupEvery-type backups
                  backupTick <- -- do after each update
                     case backupDelay of
                        BackupEvery howOft ->
                           do 
                              counter <- newMVar howOft
                              let
                                 backupTick =
                                    do
                                       cVal <- takeMVar counter
                                       if cVal <= 1 
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
                                 doBackup
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
                           oid <- newObject
                           let
                              clientData = ClientData {oid=oid,handle=handle}

                              -- This should not be done unless stateMVar
                              -- is empty.
                              protect :: IO () -> IO result -> IO result
                              protect cleanUp toDo =
                              -- Execute toDo, returning its result.
                              -- if toDo raises an exception, first 
                              -- deleteClient, then do cleanUp, then pass
                              -- on the exception
                                 do
                                    result <- Exception.try toDo
                                    case result of
                                       Right correct -> return correct
                                       Left exception ->
                                          do
                                             deleteClient clientData
                                             cleanUp
                                             throw exception

                              clientStartup =
                                 do
                                    state <- takeMVar stateMVar
                                    -- Send initial stuff and add client to 
                                    -- client list

                                    header <- sendOnConnect service state
                                    hPutStrLnFlush handle (show header)
                                    oldClients <- takeMVar clients
                                    putMVar clients (clientData:oldClients)
                                    putMVar stateMVar state

                              -- clientReadAction is basically thread for 
                              -- reading client output
                              clientReadAction :: IO ()
                              clientReadAction =
                                 do
                                    inLine <- hGetLine handle
                                    -- An error in this hGetLine will
                                    -- probably mean the handle is
                                    -- invalid, but we don't try to pick
                                    -- that up until we try to write it.
                                    debugRead service inLine

                                    let
                                       input = read inLine
                                    oldState <- takeMVar stateMVar
                                    newState <- 
                                       protect
                                          (putMVar stateMVar oldState)
                                          (do
                                             (output,newState) <-
                                                handleRequest service 
                                                   (input,oldState)
                                             let
                                                outLine = show output
                                             debugWrite service outLine
                                             broadcastAction clientData 
                                                outLine
                                             return newState
                                             )
                                    putMVar stateMVar newState
                                    backupTick
                                    clientReadAction

                           -- however it needs a wrapper so harmless
                           -- (EOF) errors don't cause any trouble.
                           forkIO(
                              do
                                 clientStartup
                                 Left exception 
                                    <- Exception.try clientReadAction
                                 -- clientReadAction cannot return otherwise
                                 let
                                    isHarmless exception = 
                                       case ioErrors exception of
                                          Nothing -> False
                                          Just ioError -> isEOFError ioError
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
               hSetBuffering handle NoBuffering
               registration <- Exception.try (lookupService handle)
               case registration of
                  Right () -> done
                  Left exception ->
                     do
                        debug "Failed service registration:"
                        debug exception   
               serverAction  

      Exception.try serverAction
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


------------------------------------------------------------------------
-- Debugging functions
------------------------------------------------------------------------

debugWrite :: ServiceClass inType outType stateType => 
   (inType,outType,stateType) -> String -> IO ()
debugRead :: ServiceClass inType outType stateType => 
   (inType,outType,stateType) -> String -> IO ()

#ifdef DEBUG
debugWrite service mess = debugString (serviceId service++"]"++mess++"\n")
debugRead service mess = debugString (serviceId service++"["++mess++"\n")
#else
debugWrite _ _ = done
debugRead _ _ = done
#endif