{- DaVinciBasic contains the code to do the following things:
   (1) get daVinci going (it calls it via a ChildProcess).
   (2) open new contexts.
   (3) pass on events and sort answers out for particular
       contexts.
   It uses the DaVinciTypes module to parse the different
   results, but makes minimal attempts to interpret the
   different datatypes (the main reason for doing so is
   to interpret DaVinciAnswer to work out what to do
   with a particular answer).
   -}
module DaVinciBasic(
   Context, -- refers to a context, IE a particular (independent) graph.

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
   
   
   ) where

import qualified IOExts(unsafePerformIO)
import Concurrent
import ByteArray
import CString
import Posix hiding (version)

import Object
import Computation
import WBFiles
import FileNames
import Registry
import UniqueString

import Spawn
import Events
import Channels
import Destructible

import ChildProcess
import InfoBus

import qualified DaVinciTypes 
import DaVinciTypes hiding (DaVinciAnswer(Context))
import DaVinciTypes (DaVinciAnswer())


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
daVinci = IOExts.unsafePerformIO newDaVinci
{-# NOINLINE daVinci #-}

newDaVinci :: IO DaVinci
newDaVinci =
   do
      daVinciPath <- getDaVinciPath
      daVinciIconsOpt <- WBFiles.getDaVinciIcons
      daVinciIcons <-
         case daVinciIconsOpt of
            Nothing ->
               do
                  top <- WBFiles.getTOP
                  return (FileNames.trimDir top ++ "/database/icons")
            Just daVinciIcons -> return daVinciIcons

      existingEnv <- Posix.getEnvironment

      let 
         configs = [
            environment (("DAVINCI_ICONDIR",daVinciIcons):existingEnv),
            arguments ["-pipe"],
            standarderrors False
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
      firstOK <- getNextAnswer childProcess
      case firstOK of
         Ok -> done
         _ -> error "DaVinci did not start properly"
      versionAnswer <- getNextAnswer childProcess
      -- All commands in future will be channelled through
      -- doInContextVeryGeneral, and all answers through
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
            destroy childProcess
            destroyAnswerDispatcher
         )
      registerTool daVinci
      return daVinci
  
daVinciVersion :: Maybe String
daVinciVersion = version daVinci
    
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
-- Contexts
-- ---------------------------------------------------------------------

data Context = Context {
   contextId :: ContextId,
   handler :: DaVinciAnswer -> IO (),
   destructChannel :: Channel (),
   typeSource :: UniqueStringSource,
   idSource :: UniqueStringSource,
   -- source for node and edge ids (which we keep distinct).
   menuIdSource :: UniqueStringSource
   -- source for menu ids.
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

      let 
         newContext = Context {
            contextId = newContextId,
            handler = handler,
            destructChannel = destructChannel,
            typeSource = typeSource,
            idSource = idSource,
            menuIdSource = menuIdSource
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
         cmdString = (show daVinciCmd) ++ "\n"
         packed = packString cmdString
         len = length cmdString 
         cIdOpt = (fmap contextId) contextOpt

         DaVinci {
            childProcess = childProcess,
            responseMVar = responseMVar,
            currentContextIdMVar = currentContextIdMVar
            } = daVinci 
      
      currentContextId <- 
         cIdOpt `seq` packed `seq` len `seq`
            takeMVar currentContextIdMVar 
            -- Here is where daVinci actually gets created.
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
                     changed@(gotContextId,result) 
                        <- getMultiAnswer childProcess
                     if gotContextId /= newContextId
                        then
                           error "set_context returned wrong context"
                        else
                           done
                     case result of
                        Ok -> done
                        _ -> error ("set_context returned "++(show result)) 
      sendMsgRaw childProcess packed len
      result@(gotContextId,daVinciAnswer) <- takeMVar responseMVar
      putMVar currentContextIdMVar gotContextId
      case cIdOpt of
         Nothing -> done
         Just newContextId -> 
            if gotContextId == newContextId
               then
                  done
            else
               error "DaVinciBasic: Mismatch in returned context"
      return result

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

-- ---------------------------------------------------------------------
-- Answer dispatcher
-- This has two jobs: 
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
   responseMVar = responseMVar
   }) =
   do
      answerDispatcher' 
   where
      forward :: DaVinciAnswer -> Context -> IO ()
      forward daVinciAnswer context =
         do
            forkIO ((handler context) daVinciAnswer)
            case destroysContext daVinciAnswer of
               Yes -> sendIO (destructChannel context) ()
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
         _ -> error "Unexpected daVinci answer expecting contextId"

getNextAnswer :: ChildProcess -> IO DaVinciAnswer
getNextAnswer childProcess =
   do
      line <- readMsg childProcess
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
