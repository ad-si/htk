{- #########################################################################

MODULE        : Signal
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha

DESCRIPTION   : Encapsulation of OS signals. Signals are converted to
                asynchronous events, that can be send as notifications
                to interactors.

                A signal manager thread is set up as a dispatcher of
                signals.                


   ######################################################################### -}


module Signal (
        emitSignal,
        signaled,

        sigTTIN,sigTTOU,sigCONT,sigFPE,sigILL,sigABRT,
        sigINT, sigTSTP, sigQUIT, sigKILL, sigHUP, sigPIPE,
        sigCHLD, sigALRM, sigSEGV, sigSTOP, sigUSR1,sigUSR2

        ) where

import System(ExitCode(ExitSuccess))
import qualified IOExts(unsafePerformIO)

import Concurrency
import Interaction
import EventBroker
import SIMClasses
import InfoBus
import PosixUtil (Fd(..))
import qualified Posix
import Posix(sigTTIN,sigTTOU,sigCONT,sigFPE,sigILL,sigABRT,
                sigINT, sigTSTP, sigQUIT, sigKILL, sigHUP, sigPIPE,
                sigCHLD, sigALRM, sigSEGV, sigSTOP, sigUSR1,sigUSR2)
import ChildProcess(readLine)
import ThreadWait
import Object
import Debug(debug)

-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data SignalDispatcher =  
        SignalDispatcher
                ObjectID 
                (PVar ToolStatus)
                (Posix.Fd,Posix.Fd) 
                (EventBroker ())

                
-- --------------------------------------------------------------------------
--  Signal Interpreter
-- --------------------------------------------------------------------------

fetchSignalDispatcher :: SignalDispatcher
fetchSignalDispatcher = IOExts.unsafePerformIO (startSignalDispatcher)


startSignalDispatcher :: IO SignalDispatcher
startSignalDispatcher = do {
        oid <- newObject;
        pv <- newPVar Nothing;
        ed <- newEventBroker;
        (r,w) <- Posix.createPipe;
        (let sdisp = (SignalDispatcher oid pv (r,w) ed) in do {
                setSignalDispatcherState sdisp;         
                forkIO (dispatchSignal sdisp);
                registerTool sdisp;     
                return sdisp
                }) 
} where setSignalDispatcherState :: SignalDispatcher -> IO ()
        setSignalDispatcherState x @ (SignalDispatcher _ _ (r,w) _) = 
                _ccall_ SetSignalFD w
-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object SignalDispatcher where
        objectID (SignalDispatcher oid _ _ _) = oid

instance Destructible SignalDispatcher where
        destroy (SignalDispatcher _ pv (r,w) _) =
                changeVar pv (\st ->
                        case st of
                           Nothing -> do {
                                try (Posix.fdClose r);
                                try (Posix.fdClose w);
                                return (Just (Posix.Exited ExitSuccess))
                                }
                           _ -> return st
                        )
        destroyed _ = inaction                          -- TBD


instance Tool SignalDispatcher where
        getToolStatus (SignalDispatcher _ pv _ _) = getVar pv


instance SingleInstanceTool SignalDispatcher where
        getToolInstance = return fetchSignalDispatcher
        

-- --------------------------------------------------------------------------
--  Event Instance
-- --------------------------------------------------------------------------

instance EventDesignator (SignalDispatcher, Int) where
        toEventID (sdisp,sig) = EventID (objectID sdisp) (show sig)


-- --------------------------------------------------------------------------
--  Emit Signal
-- --------------------------------------------------------------------------

emitSignal :: Posix.Signal -> IO ()
emitSignal sig = do {
        sdisp @ (SignalDispatcher _ pv _ ed) <- getToolInstance; 
        st <- getVar pv;
        (case st of
                Nothing -> dispatch ed (sdisp,sig) () done
                _ -> raise (toolFailed "Signal Dispatcher")
         )

}

-- --------------------------------------------------------------------------
--  Signal Dispatcher Thread
-- --------------------------------------------------------------------------

dispatchSignal :: SignalDispatcher -> IO ()
dispatchSignal sdisp @ (SignalDispatcher _ pv (r,w) ed) = do {
        waitForInputFd r;
        sig <- readLine r "";
        (let sign = (read sig) :: Int in do {
                dispatch ed (sdisp,sign) () done;
                dispatchSignal sdisp
                })
}


-- --------------------------------------------------------------------------
--  Signal Event
-- --------------------------------------------------------------------------

signaled :: Posix.Signal -> IA ()
signaled sig = awaitEvent brk ev Notice (regSig sig) (unregSig sig)
 where  sdisp@(SignalDispatcher _ _ _ brk) = fetchSignalDispatcher
        ev    = toEventID (sdisp,sig)
        regSig :: Posix.Signal -> IO ()
        regSig signo = do {
                debug ("registering " ++ show signo ++ "\n");
                _casm_ ``signal(%0,SendSignalFD);'' signo;
                done
                }
        unregSig :: Posix.Signal -> IO ()
        unregSig signo = do {
                debug ("unregistering " ++ show signo ++ "\n");
                _casm_ ``signal(%0,SIG_DFL);'' signo;
                done
                }

