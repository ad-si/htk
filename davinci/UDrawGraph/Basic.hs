{-# LANGUAGE ScopedTypeVariables #-}

-- | DaVinciBasic contains the code to do the following things:
-- (1) get daVinci going (it calls it via a ChildProcess).
-- (2) open new contexts.
-- (3) pass on events and sort answers out for particular
--     contexts.
-- It uses the DaVinciTypes module to parse the different
-- results, but makes minimal attempts to interpret the
-- different datatypes (the main reason for doing so is
-- to interpret DaVinciAnswer to work out what to do
-- with a particular answer).
module UDrawGraph.Basic(
   Context, -- refers to a context, IE a particular (independent) graph.
      -- instance of Eq, Ord.

   newContext, -- :: (DaVinciAnswer -> IO ()) -> IO Context
      -- Open a new context (and start daVinci, if this is the first time).
      -- The argument is a sink for asynchronous output relating
      -- to the context, such as drag-and-drop messages.
      -- Also, "Quit" messages, indicating that daVinci itself
      -- is terminated, are broadcast to every context.

   doInContextGeneral, -- :: DaVinciCmd -> Context -> IO DaVinciAnswer
      -- do a context-specific command in the context, returning the
      -- resulting answer.

   doInContext, -- :: DaVinciCmd -> Context -> IO ()
      -- do a context-specific command in the context.  If the answer
      -- isn't OK, complain vigorously, and throw an exception.

   withHandler, -- :: (DaVinciAnswer -> IO ()) -> Context -> IO a -> IO a
      -- temporarily change the handler of this context.
      -- If withHandler is used twice simultaneously on the same context, the
      -- second invocation will block, until the first one terminates.

   -- Generating unique identifiers.

   newType, -- :: Context -> IO Type
      -- returns a new, unique, type identifier.
   newNodeId, -- :: Context -> IO NodeId
      -- Generate a unique nodeId for this context
   newEdgeId, -- :: Context -> IO EdgeId
   newMenuId, -- :: Context -> IO MenuId

   -- Version information
   daVinciVersion, -- :: Maybe String
      -- For versions before 3.0, this is Nothing.  For 3.0 onwards,
      -- it will be the string returned by the (new) "special(version)"
      -- commands

   exitDaVinci, -- exit function because normal do in context end up in
      -- infinite blocking thread

   ) where

import Data.Maybe
import Data.List (isPrefixOf)

import System.IO.Unsafe
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Foreign.C.String
import Data.IORef
import System.Environment

import Util.Object
import Util.WBFiles
import Util.Computation as Computation (propagate, done)
import Util.Registry
import Util.UniqueString
import Util.Thread
import Util.ExtendedPrelude (mapOrd, mapEq)

import Events.Spawn
import Events.Events
import Events.Channels
import Events.Destructible
import Events.Synchronized

import Reactor.BSem
import Posixutil.ChildProcess
import Reactor.InfoBus

import qualified UDrawGraph.Types as DaVinciTypes
import UDrawGraph.Types hiding (DaVinciAnswer(Context))
import UDrawGraph.Types (DaVinciAnswer())


-- ---------------------------------------------------------------------
-- The DaVinci process
-- ---------------------------------------------------------------------

-- daVinci will be the only value of type DaVinci.
data DaVinci = DaVinci {
   childProcess :: ChildProcess,
   contextRegistry :: Registry ContextId Context,
   currentContextIdMVar :: MVar ContextId,
   destroyActMVar :: MVar (IO ()),
      -- This MVar contains the destruction action.
   responseMVar :: MVar (ContextId,DaVinciAnswer),
      -- The daVinci answer dispatcher writes answers to specific
      -- commands to this MVar, from which they should be picked
      -- up quickly!
   oID :: ObjectID, -- Unique identifier, needed for registerTool

   version :: Maybe String -- For daVinci versions before 3.0 Nothing,
      -- Afterwards, should be the version of daVinci.
   }

daVinci :: DaVinci
daVinci = unsafePerformIO newDaVinci
{-# NOINLINE daVinci #-}

challengeResponsePair :: (String,String)
challengeResponsePair =
  (unlines $ replicate 3 "nothing",
   unlines $ replicate 4 "ok")
-- 3 nothings and 4 oks, because daVinci also outputs an extra "ok"
-- right at the beginning.

newDaVinci :: IO DaVinci
newDaVinci =
   do
      daVinciPath <- getDaVinciPath
      daVinciIconsOpt <- getDaVinciIcons
      env <- getEnvironment
      let
         configs = [
            environment $ maybe id
                 (\ daVinciIcons -> (("DAVINCI_ICONDIR", daVinciIcons) :))
                 daVinciIconsOpt env,
            arguments ["-pipe"],
            standarderrors False,
            linemode True,
            challengeResponse challengeResponsePair,
            toolName "daVinci"
            ]
      childProcess <- newChildProcess daVinciPath configs

-- Send initial command.
      sendMsg childProcess (show (Special Version))
-- We will collect the answer from this in a moment . . .

      contextRegistry <- newRegistry
      currentContextIdMVar <- newMVar invalidContextId
      typeSource <- newUniqueStringSource
      destroyActMVar <- newEmptyMVar
      responseMVar <- newEmptyMVar
      oID <- newObject

-- Collect initial answers from daVinci.
      versionAnswer <- getNextAnswer childProcess
      -- With the exception of "Menu(File(Exit))" which will be used in
      -- a couple of lines to terminate daVinci, all commands in future will
      -- be channelled through doInContextVeryGeneral, and all answers through
      -- the answer dispatcher.
      let
         version = case versionAnswer of
            Versioned str -> Just str
            CommunicationError _ -> Nothing

         daVinci =
            DaVinci {
               childProcess = childProcess,
               contextRegistry = contextRegistry,
               currentContextIdMVar = currentContextIdMVar,
               destroyActMVar = destroyActMVar,
               responseMVar = responseMVar,
               oID = oID,
               version = version
               }

      destroyAnswerDispatcher <- spawn (answerDispatcher daVinci)
      putMVar destroyActMVar (
         do
            deregisterTool daVinci
            forAllContexts destroy
            sendMsg childProcess (show(Menu(File(Exit))))
               -- ask daVinci nicely to go, before we kill it.
            destroy childProcess
            destroyAnswerDispatcher
         )

      registerToolDebug "daVinci" daVinci
      return daVinci

daVinciVersion :: Maybe String
daVinciVersion = version daVinci

workAroundDaVinciBug1 :: Bool
workAroundDaVinciBug1 =
   case daVinciVersion of
      Just "daVinci Presenter Professional 3.0.3" -> True
      Just "daVinci Presenter Professional 3.0.4" -> True
      Just "daVinci Presenter Professional 3.0.5" -> True
      _ -> False

daVinciSkip :: IO ()
daVinciSkip =
   if workAroundDaVinciBug1 then delay (secs 0.1) else done

instance Destroyable DaVinci where
   destroy (DaVinci {
      destroyActMVar = destroyActMVar,
      responseMVar = responseMVar
      }) =
      do
         destroyAct <- takeMVar destroyActMVar
         putMVar destroyActMVar done
         destroyAct
         tryPutMVar responseMVar
            (invalidContextId,CommunicationError
                "daVinci ended before command completed")
         done

instance Object DaVinci where
   objectID daVinci = oID daVinci

-- ---------------------------------------------------------------------
-- Code for getting the environment that is or might be relevant to daVinci in
-- a portable way.
-- ---------------------------------------------------------------------

getDaVinciEnvironment :: IO [(String,String)]
getDaVinciEnvironment =
   do
      let
         getEnvOpt :: String -> IO (Maybe (String,String))
         getEnvOpt envName =
            do
               res <- Exception.try (getEnv envName)
               return (case res of
                  Left (_ :: Exception.IOException) -> Nothing
                  Right envVal -> Just (envName,envVal)
                  )

      daVinciEnvs
         <- mapM getEnvOpt [
            "DISPLAY","LD_LIBRARY_PATH","DAVINCIHOME","LANG","OSTYPE",
            "PATH","PWD","USER"]

      return (catMaybes (daVinciEnvs :: [Maybe (String,String)]))


-- ---------------------------------------------------------------------
-- Contexts
-- ---------------------------------------------------------------------

data Context = Context {
   contextId :: ContextId,
   destructChannel :: Channel (),
   typeSource :: UniqueStringSource,
   idSource :: UniqueStringSource,
   -- source for node and edge ids (which we keep distinct).
   menuIdSource :: UniqueStringSource,
   -- source for menu ids.

   handlerIORef :: IORef (DaVinciAnswer -> IO ()),
   -- contains current handler
   withHandlerLock :: BSem
   -- locks withHandler operations
   }

newContext :: (DaVinciAnswer -> IO ()) -> IO Context
newContext handler =
   do
      (newContextId,result)
         <- doInContextVeryGeneral (Multi NewContext) Nothing
      case result of
         Ok -> done
         CommunicationError str ->
            error ("DaVinciBasic: newContext returned error "++str)
      destructChannel <- newChannel
      typeSource <- newUniqueStringSource
      idSource <- newUniqueStringSource
      menuIdSource <- newUniqueStringSource
      handlerIORef <- newIORef handler
      withHandlerLock <- newBSem

      let
         newContext = Context {
            contextId = newContextId,
            destructChannel = destructChannel,
            typeSource = typeSource,
            idSource = idSource,
            menuIdSource = menuIdSource,
            handlerIORef = handlerIORef,
            withHandlerLock = withHandlerLock
            }
      setValue (contextRegistry daVinci) newContextId newContext
      return newContext

instance Destroyable Context where
   destroy (context@ Context {contextId = contextId}) =
      do
         deleted <- deleteFromRegistryBool (contextRegistry daVinci) contextId
         if not deleted
            then
               done -- someone else has already deleted this context
            else
               do
                  -- delete context
                  -- Things get tricky here.
                  -- The menu(file(close)) command we use, to delete the
                  -- context, will generate a "close" event, but no
                  -- "Ok" response.  doInContext (and the more generalised
                  -- versions) expect an Ok response.  We can't reclassify
                  -- close, because the user might generate it from the
                  -- file menu.  Instead we naughtily forge an "Ok" response
                  -- in advance, by writing it to responseMVar.
                  putMVar (responseMVar daVinci) (contextId,Ok)
                  doInContext (Menu(File(Close))) context


instance Destructible Context where
   destroyed context = receive (destructChannel context)

-- exit function because normal do in context end up in infinite blocking thread
exitDaVinci :: Context -> IO ()
exitDaVinci (context@ Context {contextId = contextId}) = do
  putMVar (responseMVar daVinci) (contextId,Ok)
  doInContext (Menu(File(Exit))) context

doInContext :: DaVinciCmd -> Context -> IO ()
doInContext daVinciCmd context =
   do
      answer <- doInContextGeneral daVinciCmd context
      case answer of
         Ok -> done
         CommunicationError str ->
            error ("DaVinciBasic: "++(show daVinciCmd)++
               " returned error "++str)
         -- TclAnswer is also theoretically possible, if someone
         -- is foolish enough to use doInContext with a Tcl command.

doInContextGeneral :: DaVinciCmd -> Context -> IO DaVinciAnswer
doInContextGeneral daVinciCmd context =
   do
      (cId,answer) <- doInContextVeryGeneral daVinciCmd (Just context)
      return answer

-- doInContextVeryGeneral is the all-purpose daVinci command
-- function, which is good enough, for example, for
-- context-setting functions, which require extra generality
-- and are only used in this module.
--
-- doInContextVeryGeneral is locked on currentContextIdMVar.
-- If contextOpt is Just (something), we set
--    the context and the MVar to (something), if different from
--    the existing value, and check that the returned context from
--    daVinci is also (something).
-- If contextOpt is Nothing, we change the MVar to the returned context
--    from daVinci.
doInContextVeryGeneral :: DaVinciCmd -> Maybe Context
   -> IO (ContextId,DaVinciAnswer)
doInContextVeryGeneral daVinciCmd contextOpt =
   do
      -- Pack the command as a String.  (This also prevents
      -- us having to do too much precomputation during the
      -- locked period.)
      let
         cmdString = shows daVinciCmd "\n"
         cIdOpt = (fmap contextId) contextOpt

         DaVinci {
            childProcess = childProcess,
            responseMVar = responseMVar,
            currentContextIdMVar = currentContextIdMVar
            } = daVinci

      withCStringLen cmdString (\ cStringLen ->
         -- packing the command string this early has the advantage of forcing
         -- it to be fully evaluated before we lock daVinci.
         do
            currentContextId <- takeMVar currentContextIdMVar
            -- Here is where daVinci actually gets created, if necessary.
            -- Change context id, if necessary.
            case cIdOpt of
               Nothing -> done
               Just newContextId ->
                  if currentContextId == newContextId
                     then
                        done
                     else
                        do
                           sendMsg childProcess
                              (show(Multi(SetContext newContextId)))
                           (gotContextId,result) <- takeMVar responseMVar
                           if gotContextId /= newContextId
                              then
                                 do
                                    putStrLn ("daVinci bug: "
                                       ++ "set_context returned wrong context")
                                    failSafeSetContext newContextId
                              else
                                 done
                           daVinciSkip
                           case result of
                              Ok -> done
                              _ -> error ("set_context returned "++
                                 (show result))
            sendMsgRaw childProcess cStringLen
            result@(gotContextId,daVinciAnswer) <- takeMVar responseMVar
            putMVar currentContextIdMVar gotContextId
            case cIdOpt of
               Nothing -> done
               Just newContextId ->
                  if gotContextId == newContextId
                     then
                        done
                  else
                     do
                        putStrLn "daVinci bug: Mismatch in returned context"
                        failSafeSetContext gotContextId
            return result
         )

failSafeSetContext :: ContextId -> IO ()
failSafeSetContext contextId =
   do
      putStrLn "Trying again with setContext"
      sendMsg (childProcess daVinci) (show(Multi(SetContext contextId)))
      (gotContextId,result) <- takeMVar (responseMVar daVinci)
      if gotContextId /= contextId
         then
            do
               putStrLn "Yet another mismatch; trying again with delay"
               delay (secs 0.1)
               failSafeSetContext contextId
         else
            done



forAllContexts :: (Context -> IO ()) -> IO ()
forAllContexts contextAct =
   do
      idsContexts <- listRegistryContents (contextRegistry daVinci)
      sequence_ (map (contextAct . snd) idsContexts)

invalidContextId :: ContextId
-- This should not equal any context id auto-generated by
-- daVinci.  As a matter of fact daVinci seems to generate only
-- ids starting with an underline, and gets confused by null
-- context ids.
invalidContextId = ContextId ""


withHandler :: (DaVinciAnswer -> IO ()) -> Context -> IO a -> IO a
withHandler newHandler context act =
   do
      result <- synchronize (withHandlerLock context) (
         do
            let
               ioRef = handlerIORef context

            oldHandler <- readIORef ioRef
            writeIORef ioRef newHandler
            result <- Exception.try act
            writeIORef ioRef oldHandler
            return result
         )
      Computation.propagate result

-- ---------------------------------------------------------------------
-- Answer dispatcher
-- This has three jobs:
-- (0) handle the file-menu events we allow, namely "#%print" and "#%close".
-- (1) It runs the handler attached to the context.
-- (2) It sends the destruct event for a context when appropriate.
--     (IE we receive a "close" message for that context, or
--     a "quit" message for any context).
-- ---------------------------------------------------------------------

-- Classification of answers from daVinci.
data AnswerDestination =
      Response -- result of a command to daVinci
   |  LocalEvent -- an event particular to a context
   |  GlobalEvent -- an event to be forwarded to all contexts.

answerDestination :: DaVinciAnswer -> AnswerDestination
answerDestination Ok = Response
answerDestination (CommunicationError _) = Response
answerDestination (TclAnswer _) = Response
answerDestination (Versioned _) = Response
answerDestination DaVinciTypes.Quit = GlobalEvent
answerDestination Disconnect = GlobalEvent
-- this should never occur actually, since we are starting daVinci
answerDestination _ = LocalEvent

data DestroysContext = Yes | No

destroysContext :: DaVinciAnswer -> DestroysContext
destroysContext Closed = Yes
destroysContext DaVinciTypes.Quit = Yes
destroysContext (CloseWindow _) = Yes
destroysContext _ = No

answerDispatcher :: DaVinci -> IO ()
-- We expect all commands from now on to be multi.
answerDispatcher (daVinci@DaVinci{
   childProcess = childProcess,
   contextRegistry = contextRegistry,
   currentContextIdMVar = currentContextIdMVar,
   responseMVar = responseMVar
   }) =
   do
      answerDispatcher'
   where
      forward :: DaVinciAnswer -> Context -> IO ()
      forward daVinciAnswer context =
         do
            handler <- readIORef (handlerIORef context)
            handler daVinciAnswer
            case destroysContext daVinciAnswer of
               Yes ->
                  do
                     -- Invalidate current context.
                     takeMVar currentContextIdMVar
                     putMVar currentContextIdMVar (ContextId "")
                     sync (noWait (send (destructChannel context) ()))
               No -> done

      answerDispatcher' =
         do
            (contextId,daVinciAnswer) <- getMultiAnswer childProcess
            case answerDestination daVinciAnswer of
               LocalEvent ->
                  do
                     contextOpt <- getValueOpt contextRegistry contextId
                     case contextOpt of
                        Nothing -> done
                           -- this can theoretically happen if there is an
                           -- event immediately after the context is opened
                           -- and before newContext registers it.
                        Just context -> forward daVinciAnswer context
               Response ->
                  do
                     tryPutMVar responseMVar (contextId,daVinciAnswer)
                     done
               GlobalEvent ->
                  forAllContexts (forward daVinciAnswer)
            answerDispatcher'


getMultiAnswer :: ChildProcess -> IO (ContextId,DaVinciAnswer)
-- Get next answer (associated with a ContextId).  We assume these
-- are never split by an intervening error.
getMultiAnswer childProcess =
   do
      answer1 <- getNextAnswer childProcess
      case answer1 of
         DaVinciTypes.Context contextId ->
            do
               answer2 <- getNextAnswer childProcess
               return (contextId,answer2)
         _ -> error ("Unexpected daVinci answer expecting contextId: "
            ++ show answer1)

{- unused
simpleExec :: ChildProcess -> DaVinciCmd -> IO ()
   -- used during initialisation, before we've entered multi-mode.
   -- If the result isn't OK we provoke an error.
simpleExec childProcess cmd =
   do
      sendMsg childProcess (show cmd)
      answer <- getNextAnswer childProcess
      case answer of
         Ok -> done
         _ -> error ("daVinci failure: command " ++ show cmd ++ " returned "
            ++ show answer)
   -}

getNextAnswer :: ChildProcess -> IO DaVinciAnswer
getNextAnswer childProcess =
   do
      line <- readMsg childProcess
      if isPrefixOf "program error:" line
         then
            do
               putStrLn line
               putStrLn "************ DAvINCI BUG IGNORED ***************"
               getNextAnswer childProcess
         else
            return (read line)

-- ---------------------------------------------------------------------
-- Generating new identifiers.
-- ---------------------------------------------------------------------

newType :: Context -> IO Type
newType context =
   do
      typeString <- newUniqueString (typeSource context)
      return (Type typeString)

newNodeId :: Context -> IO NodeId
newNodeId context =
   do
      nodeString <- newUniqueString (idSource context)
      return (NodeId nodeString)

newEdgeId :: Context -> IO EdgeId
newEdgeId context =
   do
      edgeString <- newUniqueString (idSource context)
      return (EdgeId edgeString)


newMenuId :: Context -> IO MenuId
newMenuId context =
   do
      menuIdString <- newUniqueString (menuIdSource context)
      return (MenuId menuIdString)

-- ---------------------------------------------------------------------
-- Instances of Eq and Ord for Context
-- ---------------------------------------------------------------------

instance Eq Context where
   (==) = mapEq contextId

instance Ord Context where
   compare = mapOrd contextId
