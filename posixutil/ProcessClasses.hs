{- ProcessClasses describes some classes which tools encapsulating
   processes may instance. -}
module ProcessClasses(
   ToolStatus, -- encodes status of process
   Tool(..), -- can get tool status
   SingleInstanceTool(..), -- this tool has at most one instance
   UnixTool(..), -- this tool has a process identifier.
   CommandTool(..), -- can send commands.
   ) where

import Posix

import Computation

-- --------------------------------------------------------------------------
--  Can get status
-- --------------------------------------------------------------------------

type ToolStatus = Maybe ProcessStatus 

class Tool t where
    getToolStatus   :: t -> IO ToolStatus

-- --------------------------------------------------------------------------
-- Tool has at most one instance, which this gets.
-- --------------------------------------------------------------------------

class SingleInstanceTool t where
    getToolInstance :: IO t

-- --------------------------------------------------------------------------
-- Tool has a ProcessID.
-- --------------------------------------------------------------------------

class Tool t => UnixTool t where
    getUnixProcessID :: t -> IO ProcessID
        
-- --------------------------------------------------------------------------
-- Command tools, IE tools where you can send a message and (maybe)
-- get a response.
-- --------------------------------------------------------------------------

class Tool t => CommandTool t where
   -- Tools have two sorts of output.  One is what comes out of their
   -- stdout channel (and if the appropriate mode in ChildProcess is set
   -- their stderr channel as well).  
   -- execOneWayCmd is used when that's all there is.
   -- execCmd is used when there's also a string from somewhere else as well.
   evalCmd         :: String -> t -> IO String
   execCmd         :: String -> t -> IO ()
   execOneWayCmd   :: String -> t -> IO ()
   execCmd cmd t   = 
      do
         evalCmd cmd t
         done
   -- only overrridden by Expect, in which all commands are one-way.