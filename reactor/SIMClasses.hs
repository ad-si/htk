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
--  Interactions  (Einar's thesis 7.4.2)
-- --------------------------------------------------------------------------

-- Instances of Reactive are not constructed by the reactor part of
-- UniForM; instead EG in htk/menuitems/Button.hs
-- where a Button a is made an instance.  This is done using
-- htk/kernel/GUIState.userinteraction, which is (I think) the same as
-- htk/kernel/GUIState.listenGUI, which in turn calls the interaction
-- routine.
class Reactive w a where
   triggered :: w a -> IA a

-- An EventStream is an instance of HasTrigger (see EventStream.hs)
-- Other instances in HTk are for example Button/CheckButton/DialogWin/
-- SpinButton (naive implementation from triggered) 
-- and Menu/SelectBox where other approaches are taken.
class HasTrigger w a where
   getTrigger :: w a -> IO (IA a)

-- This class is used only in HTk as far as I know.  Instances include
-- Button/CheckButton/MenuButton
class HasMapTrigger w where
   mapTrigger :: (a -> IO b) -> w a -> IO (w b)


-- --------------------------------------------------------------------------
--  Event Bindings
-- --------------------------------------------------------------------------

-- This class appears to be unused (although implemented) in
-- both the reactor and HTk.  It is however used in tools/Hugs.hs
class HasBinding o a where
    bind   :: o -> IA a -> IO ()
    unbind :: o -> IA a -> IO ()
-- bind/unbind are implemented particularly for event streams, and add/remove 
-- an interaction for that event stream.

-- --------------------------------------------------------------------------
--  Object Classes
-- --------------------------------------------------------------------------

class Destructible o where
-- destroy destroys the object; the destroyed event should then
-- occur.  See EWK thesis 7.4.1.
    destroy         :: o -> IO ()
    destroyed       :: o -> ExternalEvent.IA ()



-- --------------------------------------------------------------------------
--  Adaptor (EWK thesis 7.6.2)
-- --------------------------------------------------------------------------
-- Instances of Adaptors: Dispatcher and EventBroker.
-- 
class Adaptor adaptor where
-- An Adaptor sits between the original generator of an
-- event and a number of listeners.  The generator sends pairs
-- (eventId,a).
-- Each operation takes an IO() ack value.  This is performed after the
-- operation if certain conditions are met, reconstructed below on
-- the basis of guesses from Signal.hs (the only thing I can find
-- which provides non-trivial ack operations) and EventBroker.
-- For register/deregister the Listener argument is last so
-- that we can supply all but the last argument to make a suitable
-- function for Interaction.interaction.
   register     :: (EventDesignator eventDesignator, Typeable a) => 
      adaptor a -> eventDesignator -> DispatchMode -> IO () -> Listener -> 
         IO ()
-- register adaptor eventDesignator mode op listener
--    registers the listener for messages for events eventDesignator.
--    mode indicates what mode to use.  
-- ack is performed if the listener queue for this event was previously
--    empty
   deregister   :: (EventDesignator eventDesignator, Typeable a) => 
      adaptor a -> eventDesignator -> IO () -> Listener -> IO ()
-- ack is performed if the listener queue for this event is subsequently
--    empty.
-- deregister similar to register but deregisters.
   dispatch     :: (EventDesignator eventDesignator, Typeable a) => 
      adaptor a -> eventDesignator -> a -> IO () -> IO ()
-- dispatch is called to send a new value to the adaptor.
-- ack is performed when the value has been adapted.  (dispatch may
-- return before that happens.)
-- --------------------------------------------------------------------------
--  Tool Status
-- --------------------------------------------------------------------------

type ToolStatus = Maybe ProcessStatus 
-- ProcessStatus comes from Posix and encodes the exit code.


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

-- An instance is for example Signal, which when getToolInstance
-- is called calls (unsafely) startSignalDispatcher.  So I presume instance
-- are tools of which you only need one with no arguments.  
class SingleInstanceTool t where
    getToolInstance :: IO t


-- --------------------------------------------------------------------------
--  Command Tool (documented in thesis, 8.4.2)
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

-- --------------------------------------------------------------------------
--  Reactive Command Tool
-- --------------------------------------------------------------------------

-- The only instances I can find of a ReactiveCommandTool are 
-- Interpreter and Dispatcher (which I think just uses the
-- implementation of Interpreter).  The sendReply function is used
-- for example in the dispatchTk function of htk/kernel/GUIWish.
class CommandTool t => ReactiveCommandTool t where
    sendReply       :: Answer String -> t -> IO ()


-- --------------------------------------------------------------------------
--  IOErrors
-- --------------------------------------------------------------------------

toolFailed :: String -> IOError
toolFailed tname = userError ("tool " ++ tname ++ " failed")



