{- #########################################################################

MODULE        : SIMClasses
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : A couple of Classes Relevant for the 
                Subsystem Interaction Manager.
 
   ######################################################################### -}


module SIMClasses (

        ToolStatus(..),
        ProcessStatus(..),
        toolFailed,

        Reactive(..),
        HasTrigger(..),
        HasMapTrigger(..),
        HasBinding(..),

        Destructible(..),

        Listener,
        DispatchMode(..),
        Adaptor(..),

        Tool(..),
        UnixTool(..),
        SingleInstanceTool(..),
        CommandTool(..),
        ReactiveCommandTool(..)

        ) where


import Dynamics
import Concurrency
import Interaction
import Listener
import Object
import FiniteMap
import Posix(ProcessStatus(..),ProcessID)
import qualified ExternalEvent(IA)
import Debug(debug)



-- --------------------------------------------------------------------------
--  Interactions
-- --------------------------------------------------------------------------

class Reactive w a where
        triggered :: w a -> IA a

class HasTrigger w a where
        getTrigger :: w a -> IO (IA a)

class HasMapTrigger w where
        mapTrigger :: (a -> IO b) -> w a -> IO (w b)


-- --------------------------------------------------------------------------
--  Event Bindings
-- --------------------------------------------------------------------------

class HasBinding o a where
        bind   :: o -> IA a -> IO ()
        unbind :: o -> IA a -> IO ()


-- --------------------------------------------------------------------------
--  Tool Status
-- --------------------------------------------------------------------------

type ToolStatus = Maybe ProcessStatus


-- --------------------------------------------------------------------------
--  Object Classes
-- --------------------------------------------------------------------------

class Destructible o where
        destroy         :: o -> IO ()
        destroyed       :: o -> ExternalEvent.IA ()



-- --------------------------------------------------------------------------
--  Adaptor
-- --------------------------------------------------------------------------

class Adaptor t where
        register     :: (EventDesignator e, Typeable a) => 
                        t a -> e -> DispatchMode -> IO () -> Listener -> IO ()
        deregister   :: (EventDesignator e, Typeable a) => 
                        t a -> e -> IO () -> Listener -> IO ()
        dispatch     :: (EventDesignator e, Typeable a) => 
                        t a -> e -> a -> IO () -> IO ()
        

-- --------------------------------------------------------------------------
--  WorkBench Tool
-- --------------------------------------------------------------------------

class Tool t where
        getToolStatus   :: t -> IO ToolStatus


-- --------------------------------------------------------------------------
--  Unix Tool
-- --------------------------------------------------------------------------

class Tool t => UnixTool t where
        getUnixProcessID :: t -> IO ProcessID
        

-- --------------------------------------------------------------------------
--  Single Instance Tool
-- --------------------------------------------------------------------------

class SingleInstanceTool t where
        getToolInstance :: IO t


-- --------------------------------------------------------------------------
--  Command Tool
-- --------------------------------------------------------------------------

class Tool t => CommandTool t where
        evalCmd         :: String -> t -> IO String
        execCmd         :: String -> t -> IO ()
        execOneWayCmd   :: String -> t -> IO ()
        execCmd cmd t   = evalCmd cmd t >> done


-- --------------------------------------------------------------------------
--  Reactive Command Tool
-- --------------------------------------------------------------------------

class CommandTool t => ReactiveCommandTool t where
        sendReply       :: Answer String -> t -> IO ()


-- --------------------------------------------------------------------------
--  IOErrors
-- --------------------------------------------------------------------------

toolFailed :: String -> IOError
toolFailed tname = userError ("tool " ++ tname ++ " failed")


