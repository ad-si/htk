-- |
-- Description: Run servers defined by 'ServiceClass.ServiceClass' instances.
--
-- In Server we implement a general framework for a server, which
-- is supposed to make it easier to share information between
-- computers, in a similar method to inetd.  Unlike inetd however
-- each client computer maintains a session with the server.  Also
-- we only use one port for all services, though an additional
-- integer is sent at the start of opening a connection to specify
-- which service is to be used.
--
-- To specify a new service you need to define an instance of
-- the class ServerClass, defined in the module of that name.
module Server(
   runServer
   -- :: [Service] -> IO ()
   -- runs a server on the standard port.  It does not return until
   -- the server is interrupted in some way.
   ) where

import IO
import List
import Time

import Control.Exception hiding (handle)

import DeprecatedFiniteMap
import Network hiding (Service)

import Control.Concurrent

import Computation
import ExtendedPrelude
import WBFiles(getPort)
import Thread
import Object
import BinaryAll

import BlockSigPIPE

import HostsPorts hiding (user)
import ServiceClass
import Crypt
import PasswordFile

data ClientData = ClientData {
   oid :: ObjectID,
   handle :: Handle,
   user :: User
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
   newClientAction :: Handle -> User -> IO (),
   disconnect :: IO ()
   }

runServer :: [Service] -> IO ()
runServer serviceList =
   do
      portDesc <- getPort
      blockSigPIPE
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
                     broadcastAction
                        :: ClientData -> (Handle -> IO ()) -> IO ()
                     -- The first argument is the client who initiated
                     -- the request; the second is an action sending the
                     -- message
                     broadcastAction =
                        case serviceMode service of
                           Reply ->
                              (\ clientData outputAction ->
                                 outputAction (handle clientData)
                                 )
                           BroadcastOther ->
                              (\ clientData outputAction ->
                                 do
                                    clientList <- readMVar clients
                                    let
                                       otherClients =
                                          filter (/= clientData) clientList
                                    broadcastToClients otherClients
                                       outputAction
                                 )
                           _ ->
                              (\ _ outputAction ->
                                 do
                                    clientList <- readMVar clients
                                    broadcastToClients clientList outputAction
                                 )

                     -- This should not be done unless stateMVar
                     -- is empty.
                     broadcastToClients :: [ClientData] -> (Handle -> IO ())
                        -> IO ()
                     -- Broadcasts the string to all clients listed
                     broadcastToClients clientList outputAction =
                        sequence_ (
                           map
                              (\ clientData ->
                                 do
                                    -- If fail, delete clientData
                                    success <- IO.try(
                                       outputAction (handle clientData)
                                       )
                                    case success of
                                       Left error ->
                                          deleteClient clientData
                                       Right () -> done
                                 )
                              clientList
                           )

                     -- This is the function that handles a new bit of
                     -- input.  The clientData may be undefined for an
                     -- External service mode (as, indeed, it will be).
                     -- The inputAction returns the input (it will just be
                     -- a "return" except for External, where it is a
                     -- user-supplied action which needs to be performed
                     -- inside the stateMVar lock).
                     handleInput clientData inputAction =
                        do
                           oldState <- takeMVar stateMVar
                           input <- inputAction
                           newStateEither <-
                              Control.Exception.try
                                 (do
                                    (output,newState) <- handleRequest
                                       service (user clientData)
                                       (input,oldState)
                                    let
                                       outputAction handle =
                                          do
                                             hWrite handle output
                                             hFlush handle
                                    broadcastAction clientData outputAction
                                    return newState
                                    )
                           case newStateEither of
                              Right newState ->
                                 do
                                    putMVar stateMVar newState
                                    backupTick
                              Left excep ->
                                 do
                                    putMVar stateMVar oldState
                                    throw excep

------------------------------------------------------------------------
-- (3) Define newClientAction to be done for each new client
------------------------------------------------------------------------
                  let
                     newClientAction handle user =
                        do
                           oid <- newObject
                           let
                              clientData = ClientData {
                                 oid=oid,handle=handle,user=user}

                              clientStartup =
                                 do
                                    state <- takeMVar stateMVar
                                    -- Send initial stuff and add client to
                                    -- client list

                                    wrappedHeader <- sendOnConnectWrapped
                                       service user state
                                    hWriteWrappedBinary handle wrappedHeader
                                    hFlush handle
                                    oldClients <- takeMVar clients
                                    putMVar clients (clientData:oldClients)
                                    putMVar stateMVar state

                              -- clientReadAction is basically thread for
                              -- reading client output
                              clientReadAction :: IO ()
                              clientReadAction =
                                 do
                                    input <- hRead handle
                                    handleInput clientData (return input)
                                    clientReadAction

                           case serviceMode service of
                              External _ ->
                                 do
                                    forkIO clientStartup
                                    done
                              _ ->
                                 do
                                    -- clientReadAction needs a wrapper so
                                    -- harmless (EOF) errors don't cause any
                                    -- trouble.
                                    forkIO(
                                       do
                                          clientStartup
                                          Left exception
                                             <- Control.Exception.try
                                             clientReadAction
                                          -- clientReadAction cannot return
                                          -- otherwise
                                          case exception of
                                             IOException excep
                                                | isEOFError excep
                                                   -> done
                                             _ ->
                                                putStrLn (
                                                   "Server error on "
                                                   ++ show serviceKey ++ ": "
                                                   ++ show exception
                                                   )
                                          state0 <- takeMVar stateMVar
                                          deleteClient clientData
                                          state1 <-
                                             handleClientDisconnect service
                                                user state0
                                          putMVar stateMVar state1


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

                  -- Create a function
                  case serviceMode service of
                     External register ->
                        let
                           handleValue inputAction =
                              handleInput (error "Server.1") inputAction
                        in
                           register handleValue
                     _ -> done

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
      socket <- listenOn (PortNumber portNumber)
      let
         serverAction =
            do
               (handle,_,_) <- accept socket

               hSetBuffering handle (BlockBuffering (Just 4096))
                  -- since we may well be doing the connection via SSL,
                  -- we use a big buffer, and only flush when necessary.

               connectionOpt <- initialConnect serviceMap handle
               case connectionOpt of
                  Nothing -> done
                  Just (serviceData,user)
                     -> newClientAction serviceData handle user
               serverAction

      Control.Exception.try serverAction
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
-- Function for doing the initial service lookup-up and authentication.
------------------------------------------------------------------------

initialConnect :: FiniteMap String ServiceData -> Handle
   -> IO (Maybe (ServiceData,User))
initialConnect serviceMap handle =
   do
      -- wrap everything so EOF's can't cause trouble here.  This means
      -- we have two levels of exception handlers, incidentally.
      resultOrExcep <- Control.Exception.try (
         do
            resultOrString <- addFallOut (\ break ->
               do
                  connectDataWE <- hReadLtd 256 handle
                  (key,userId,password) <-
                     coerceWithErrorOrBreakIO break connectDataWE

                  -- (1) find the service
                  serviceData <- case lookupFM serviceMap key of
                     Nothing -> break ("Service " ++ show key
                        ++ " not recognised")
                     Just serviceData -> return serviceData

                  -- (2) find the user

                  let
                     authError = break "Unable to authenticate user"

                  userOpt <- getUserEntry userId
                  user <- case userOpt of
                     Nothing -> authError
                     Just user -> return user

                  passwordOK <- verifyPassword password
                     (encryptedPassword user)
                  if passwordOK
                     then
                        return (serviceData,user)
                     else
                        authError
               )

            case resultOrString of
               Right result ->
                 do
                     hPutStrLn handle "OK"
                     hFlush handle
                     return (Just result)
               Left "" -> -- null service key
                  do
                     hClose handle
                     return Nothing
               Left mess ->
                  do
                     hPutStrLn handle mess
                     hClose handle
                     return Nothing
         )

      case resultOrExcep of
         Left excep ->
            do
               case excep of
                 IOException _ ->
                    done
                       -- we ignore this, because mmiss/test/IsConnected
                       -- provokes it.
                 _ ->
                    putStrLn ("IO error in initial connect " ++ show excep)
               hClose handle
               return Nothing
         Right result ->
            do
               case result of
                  Just (serviceData,user) ->
                     do
                        clockTime <- getClockTime
                        calendarTime <- toCalendarTime clockTime

                        putStrLn (userId user ++ ":"
                           ++ calendarTimeToString calendarTime)
                        hFlush stdout
                  Nothing -> done
               return result

