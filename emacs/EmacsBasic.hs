{- This module contains basic Haskell code for talking to Emacs. -}
module EmacsBasic(
   EmacsSession, 
      -- A particular Emacs session with its own buffer

   newEmacsSession, -- :: String -> IO EmacsSession
      -- Start a new EmacsSession.
      -- The String argument is the buffer name to be used by Emacs
      -- (it may have to slightly modify it to make it unique)

      -- There can be several EmacsSession's at the same time, but 
      -- they will all be using the same Emacs (courtesy of gnuclient).

   evalEmacsString, -- :: EmacsSession -> String -> IO String
      -- Evaluate the given Emacs Lisp expression, which should return a 
      -- String.

   evalEmacsStringQuick, -- :: EmacsSession -> String -> IO String
      -- Evaluate the given Emacs Lisp expression, which should return a 
      -- String, guaranteed not to contain any newline characters.

   execEmacsString, -- :: EmacsSession -> String -> IO ()
      -- Evaluate the given Emacs Lisp expression.  Any result is ignored.

   emacsEvent, -- :: EmacsSession -> String -> EV String
      -- Event sent by the Emacs Lisp uni-ev function (used for example
      -- when extents are added) with a particular key.

   unUniEscape, -- :: String -> String
   -- Undo the effects of sendmess.el's uni-escape function

   diyDestroy, -- :: EmacsSession -> IO ()
   -- destroys the EmacsSession itself, but assume any necessary
   -- destruction on the Emacs side has been done.

   ) where

import IO

import Socket(PortNumber)
import Concurrent
import qualified IOExts(unsafePerformIO)

import Object
import Debug(debugString)
import Computation(done)
import WBFiles
import CommandStringSub(emacsEscape)
import IOExtras(catchEOF)
import DeepSeq
import ExtendedPrelude

import GuardedEvents
import EqGuard
import Events
import Channels
import Destructible

import BSem
import InfoBus

import ChildProcess

import MultiServer

-- ------------------------------------------------------------------------
-- The datatype
-- ------------------------------------------------------------------------

data EmacsSession = EmacsSession {
   handle :: Handle,

   -- emacsLock and emacsResponse handle responses to evalEmacsString commands
   emacsLock :: BSem,
   emacsResponse :: MVar String,

   -- events 
   eventChannel :: EqGuardedChannel String String,
   
   oID :: ObjectID,
   closeAction :: IO ()
   }

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance Object EmacsSession where
   objectID emacsSession = oID emacsSession

instance Destroyable EmacsSession where
   destroy emacsSession = 
      do
         execEmacsString emacsSession "(kill-buffer (current-buffer))"
         closeAction emacsSession

---
-- destroys the EmacsSession itself, but assume any necessary
-- destruction on the Emacs side has been done.
diyDestroy :: EmacsSession -> IO ()
diyDestroy emacsSession = closeAction emacsSession

-- ------------------------------------------------------------------------
-- Opening a new session
-- ------------------------------------------------------------------------

newEmacsSession :: String -> IO EmacsSession
newEmacsSession bufferName =
   do
      key <- newMultiServerKey emacsMultiServer
      portNumber <- getPortNumber emacsMultiServer

      let
         keyString = fromMultiServerKey key
      
         clientAction =
            "(uni-initialise "++
               show portNumber++" "++
               quoteEmacsString keyString++" "++
               quoteEmacsString bufferName++
               ")"
      handle <- waitForClient emacsMultiServer key 
         (execGnuClient clientAction)
      emacsLock <- newBSem
      emacsResponse <- newEmptyMVar
      eventChannel <- newEqGuardedChannel
      oID <- newObject
      let
         closeAction = done -- for now
      
         emacsSession = EmacsSession {
            handle = handle,
            emacsLock = emacsLock,
            emacsResponse = emacsResponse,
            eventChannel = eventChannel,
            oID = oID,
            closeAction = closeAction
            }

      forkIO (readEmacsOutput emacsSession)
      return emacsSession

readEmacsOutput :: EmacsSession -> IO ()
readEmacsOutput emacsSession =
   do
      lineOpt <- catchEOF (hGetLine (handle emacsSession))
      case lineOpt of
         Nothing -> done -- buffer and session have been destroyed.
         Just line ->
            do
               debugString ("XEmacs<"++line++"\n")

               case line of
                  'O':'K':' ':rest ->
                      do
                         written <- tryPutMVar (emacsResponse emacsSession) 
                            rest
                         if written
                            then
                               done
                            else
                               error "EmacsBasic: emacsResponse not empty"
                  'E':'R':' ':rest -> 
                     error ("EmacsBasic: XEmacs error: "++unUniEscape rest)
                  'E':'V':' ':rest ->
                     let
                        (key,value) =
                           case splitToChar ' ' rest of
                              Nothing -> error "EmacsBasic: bad event"
                              Just (key,unEscaped) 
                                 -> (key,unUniEscape unEscaped)
                     in
                        sync(noWait(send (eventChannel emacsSession) 
                           (key,value)))

                  _ -> parseError line
               readEmacsOutput emacsSession

parseError :: String -> a
parseError line =
   error ("EmacsBasic: couldn't parse Emacs response "++ (show line))

-- ------------------------------------------------------------------------
-- Communicating with Emacs
-- ------------------------------------------------------------------------

-- We need to lock execEmacsString calls because otherwise two calls to 
-- execEmacsString running simultaneously could interleave their output to 
---Emacs.
execEmacsString :: EmacsSession -> String -> IO ()
execEmacsString emacsSession command =
   deepSeq command 
      (synchronize (emacsLock emacsSession) 
         (writeEmacs emacsSession command))
        

evalEmacsString :: EmacsSession -> String -> IO String
evalEmacsString emacsSession expression =
   deepSeq expression
      (synchronize (emacsLock emacsSession) (
         do
            let
               command' = "(uni-ok "++expression++")"
            writeEmacs emacsSession command'
            str <- takeMVar (emacsResponse emacsSession)
            return (unUniEscape str)
         ))

evalEmacsStringQuick :: EmacsSession -> String -> IO String
evalEmacsStringQuick emacsSession expression =
   deepSeq expression 
      (synchronize (emacsLock emacsSession) (
         do
            let
               command' = "(uni-ok-quick "++expression++")"
            writeEmacs emacsSession command'
            takeMVar (emacsResponse emacsSession)
         ))

writeEmacs :: EmacsSession -> String -> IO ()
writeEmacs emacsSession command =
   do
      debugString ("XEmacs>"++command++"\n")
      let
         hand = handle emacsSession
      hPutStrLn hand command
      hFlush hand


emacsEvent :: EmacsSession -> String -> Event String
emacsEvent emacsSession key =
   toEvent (listen (eventChannel emacsSession) |> (Eq key)) >>>=
      (\ (_,rest) -> return rest)


-- ------------------------------------------------------------------------
-- Quoting Strings for Emacs
-- Most of the work is already done for us by CommandStringSub.emacsEscape
-- ------------------------------------------------------------------------

quoteEmacsString :: String -> String
quoteEmacsString str = "\""++emacsEscape str++"\""

-- ------------------------------------------------------------------------
-- The Emacs MultiServer
-- ------------------------------------------------------------------------

emacsMultiServerPort :: PortNumber
emacsMultiServerPort = IOExts.unsafePerformIO getEmacsMultiServerPort
{-# NOINLINE emacsMultiServerPort #-}

getEmacsMultiServerPort :: IO PortNumber
getEmacsMultiServerPort = getPortNumber emacsMultiServer

emacsMultiServer :: MultiServer
emacsMultiServer = IOExts.unsafePerformIO initialiseEmacsBasic
{-# NOINLINE emacsMultiServer #-}

-- Start the multi-server and tell Emacs to load the required library.
initialiseEmacsBasic :: IO MultiServer
initialiseEmacsBasic =
   do 
      multiServer <- newMultiServer False Nothing
      top <- getTOP
      let 
         load :: String -> String
         load name = " (load \"" ++ top ++ "/emacs/" ++ name ++ ".elc\")"
      execGnuClient (
         "(progn "++load "sendmess"++load "mmisstex"++load "extents"++
            load "allmmiss"++")"
         )
      return multiServer

-- ------------------------------------------------------------------------
-- Calling gnuclient.
-- ------------------------------------------------------------------------

---
-- Execute the given command via gnuclient.  We attempt to catch
-- common errors, except for bracket-counting.
execGnuClient :: String -> IO ()
execGnuClient command =
   do
      let
         okMess = "gnuClient OK                                             "
         toEval = "(condition-case err (progn "++command++
            "\""++okMess++"\") (error (format \"gnuclient error: %s\" err)))"
      gnuClientPath <- getGnuClientPath
     
      gnuClient <- newChildProcess gnuClientPath [
         arguments ["-batch","-eval",toEval],
         linemode False,
         challengeResponse ("",okMess++"\n")
         ]
      done

-- ------------------------------------------------------------------------
-- Undoing escapes
-- ------------------------------------------------------------------------

---
-- Undo the effects of sendmess.el's uni-escape function
unUniEscape :: String -> String
unUniEscape "" = ""
unUniEscape ('\\':'n':rest) = '\n':unUniEscape rest
unUniEscape ('\\':'\\':rest) = '\\':unUniEscape rest
unUniEscape (str@('\\':_:rest)) = parseError str
unUniEscape (ch:rest) =ch:unUniEscape rest
      
        

