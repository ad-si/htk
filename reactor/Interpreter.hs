{- #########################################################################

MODULE        : Interpreter
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Generic, loosely coupled interpreter tool.

Interpreter wraps up an interactive tool.  There are two examples of
uses of interpreters, both via Dispatcher:
(1) Wish (used of course for HTk).
(2) DaVinci.
You need to supply Interpreter with four things
(1) what's needed to start the process up by ChildProccess; EG path
    of command, and options (passed to newInterpreter).
(2) a handler action.  This is called whenever the tool returns something
    along its output channel.  (stdout and if the ChildProcess options
    request it, stderr as well.)
(3) A finaliser action.
(4) If you want to use execCmd/evalCmd (when an additional extra string
    result is expected) the tool wrapper must call sendReply to send
    the result back to the interpreter.

   ######################################################################### -}


module Interpreter (
   ToolStatus,
   
   Destructible(..),
   Tool(..),
   CommandTool(..),
   
   Interpreter,
   newInterpreter,
   
   commandFailed
   ) where

import Concurrency
import ChildProcess
import SIMClasses
import Object
import Debug(debug,(@:))

-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Interpreter        = 
   Interpreter 
      ObjectID -- unique identifier
      ReplyAddr
      ChildProcess
      (Interpreter -> IO ())  -- finalizer command
      BSem -- used to synchronize on

newtype ReplyAddr  = ReplyAddr(MVar (Answer String))  
-- Place where answers to commands are put by sendReply.

                
-- --------------------------------------------------------------------------
--  Set up new interpreter
-- --------------------------------------------------------------------------

newInterpreter :: 
   FilePath ->                    -- toolname
   [Config PosixProcess] ->       -- options
   (Interpreter -> IO ()) ->      -- finalizer
   (String -> Interpreter -> IO ()) -> 
      -- handler for tool stdout output (plus stderr if options ask for them)
   IO Interpreter 
   -- not only do we return the interpreter, we also set it going.
newInterpreter toolName confs finaliser handler = 
   do
      oid <- newObject
      lock <- newBSem
      child <- newChildProcess toolName confs
      replyMVar <- newEmptyMVar     
      let replyLoc = ReplyAddr replyMVar
      let interpreter = Interpreter oid replyLoc child finaliser lock
      forkIO(dispatcher interpreter handler)
      return interpreter
   where
      dispatcher :: Interpreter -> (String -> Interpreter -> IO ()) -> IO ()
      dispatcher interpreter@(Interpreter _ _ child _ _) handler = 
          do
             msg <- readMsg child
             debug ("Interpreter:<<" ++ msg)
             handler msg interpreter
             dispatcher interpreter handler

-- --------------------------------------------------------------------------
--  Tool Instances
-- --------------------------------------------------------------------------

instance Object Interpreter where
   objectID (Interpreter oid _ _ _ _) = oid

instance Destructible Interpreter where
   destroy interpreter @ (Interpreter _ _ child finaliser _) =
      do 
         finaliser interpreter
         -- to make sure we kill the child as well, if still around.
         try(destroy child)
         done

   destroyed (Interpreter _ _ child _ _) = destroyed child

instance Tool Interpreter where
   getToolStatus (Interpreter _ _ child _ _) = getToolStatus child

instance UnixTool Interpreter where
   getUnixProcessID (Interpreter _ _ child _ _) = getUnixProcessID child

-- --------------------------------------------------------------------------
--  Command Tool Instance
-- --------------------------------------------------------------------------

instance CommandTool Interpreter where 
   evalCmd cmd (Interpreter oid replyLoc t _ lock) = 
      synchronize
         lock
         (do
            debug ("Interpreter:>>" ++ cmd)
            sendMsg t cmd
            let ReplyAddr replyMVar = replyLoc
            res <- takeMVar replyMVar
            propagate res
         )
                             
   execOneWayCmd cmd (Interpreter _ _ t _ lock) = 
      synchronize
         lock
         (do
            debug ("Interpeter'>>" ++ cmd)
            sendMsg t cmd
          )

-- --------------------------------------------------------------------------
--  Reactive Command Tool Instance
-- --------------------------------------------------------------------------

instance ReactiveCommandTool Interpreter where
   sendReply ans (Interpreter _ (ReplyAddr replyMVar)  _ _ _) = 
      putMVar replyMVar ans

-- --------------------------------------------------------------------------
-- IOError
-- --------------------------------------------------------------------------

commandFailed :: IOError
commandFailed = userError "command failed with exception"



