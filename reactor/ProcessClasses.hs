{- ProcessClasses describes some classes which tools encapsulating
   processes may instance. -}
module ProcessClasses(
   ToolStatus, -- encodes status of process
   Tool(..), -- can get tool status
   SingleInstanceTool(..), -- this tool has at most one instance
   UnixTool(..), -- this tool has a process identifier.
   ) where

import Posix


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
        
