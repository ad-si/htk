{- #########################################################################

MODULE        : GUIWish
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : GUI configured Wish Interpreter. 
                


   ######################################################################### -}


module GUIWish (
        ChildProcess,
        ToolStatus(..),
        
        Object(..),
        Destructible(..),
        Tool(..),
        UnixTool(..),
        CommandTool(..),

        Wish(..),
        newWish,

        TclCmd,
        TclScript,
        execScript,
        evalScript,

        setTclVar,
        getTclVar

        ) where

import SIM

import ChildProcess
import Object
import Resources
import Posix(signalProcess,sigKILL)

import WBFiles
import Debug(debug)

-- --------------------------------------------------------------------------
--  Wish Dispatcher
-- --------------------------------------------------------------------------

data Wish = Wish (Dispatcher EventInfo)


-- --------------------------------------------------------------------------
-- Tcl Commands
-- --------------------------------------------------------------------------

type TclCmd = String

type TclScript = [TclCmd]


-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Object Wish where
        objectID (Wish t)               = objectID t

instance Tool Wish where
        getToolStatus (Wish t)          = getToolStatus t


instance Destructible Wish where
   destroy (Wish t) = 
      do
         debug "Wish.destroy 1"
         destroy t
         debug "Wish.destroy 2"

   destroyed (Wish t) = destroyed t


instance UnixTool Wish where
        getUnixProcessID (Wish t)       = getUnixProcessID t


instance CommandTool Wish where
        evalCmd cmd wish                = evalScript [cmd] wish
        execCmd cmd wish                = execScript [cmd] wish
        execOneWayCmd cmd (Wish t)      = execOneWayCmd cmd t

                        
-- --------------------------------------------------------------------------
--  Wish Interpreter
-- --------------------------------------------------------------------------

newWish :: IO Wish
newWish = do {
        msq <- newMsgQueue;
        tname <- getWBToolFilePath "wish";      
        intrp <- newDispatcher tname [] finalizer (dispatch msq);
        forkIO (dispatcher msq intrp); 
        execOneWayCmd tkConvertTkValueProc intrp;
        execOneWayCmd tkDeclEvalScript intrp;
        execOneWayCmd tkDeclExitProc intrp;
        execOneWayCmd tkDeclScaleProc intrp;
        execOneWayCmd tkDeclButtonProc intrp;
        execOneWayCmd tkSafeGetConfigProc intrp;
        execOneWayCmd "wm withdraw ." intrp;
        return (Wish intrp)
} where finalizer ctool = execOneWayCmd "evS {Exit}" ctool
        dispatch msq str _ = sendIO msq str
        tkDeclEvalScript = 
                "proc evS x { set status [catch {eval $x} res]; set val [ConvertTkValue $res]; if {$status == 0} {puts stdout \"OK $val\"} else {puts stdout \"ER $val\"}; flush stdout}"

        tkDeclExitProc = "proc Exit {} { puts stdout \"EX\"; flush stdout}"

        tkDeclScaleProc = "proc Scaled {wid pos} { puts stdout [concat {EV } $wid  { SS } $pos]; flush stdout}"

        tkDeclButtonProc = "proc Clicked wid { puts stdout [concat {EV } $wid {CL }]; flush stdout}"

        tkSafeGetConfigProc = "proc SafeGetConfig cmd { set val [eval $cmd]; regsub -all \\n $val \"\\\\n\" res; return $res}"

        tkConvertTkValueProc = "proc ConvertTkValue val {regsub -all \\n $val \"\\\\n\" res; return $res}"



-- --------------------------------------------------------------------------
-- Command Script Execution and Evaluation
-- --------------------------------------------------------------------------

execScript :: TclScript -> Wish -> IO ()
execScript [] wish = done
execScript cmd wish = evalScript cmd wish >> done


evalScript :: TclScript -> Wish -> IO String
evalScript cmd (Wish ctool) = evalCmd ("evS {" ++ (showS cmd) ++ "}") ctool
        where showS :: [TclCmd] -> String
              showS [] = " "
              showS [c] = c
              showS (c : cl) = c ++ " ; " ++ (showS cl)


-- --------------------------------------------------------------------------
-- Tcl Variables
-- --------------------------------------------------------------------------

setTclVar :: GUIValue a => String -> a -> Wish -> IO ()
setTclVar name v wish = execScript cmds wish
        where cmds = [  "global " ++ name,
                        "set " ++ name ++ " " ++ val 
                     ]
              val = (show . toGUIValue) v


getTclVar :: GUIValue a => String -> Wish -> IO a
getTclVar name wish = evalScript [cmd] wish >>= creadTk
        where cmd = "global " ++ name ++ "; set res $" ++ name


-- --------------------------------------------------------------------------
-- Event Dispatching 
-- --------------------------------------------------------------------------

dispatcher :: MsgQueue String -> Dispatcher EventInfo -> IO ()
dispatcher msq disp = do {
        msg <- receiveIO msq;
        dispatchTk msg disp;
        dispatcher msq disp
        }


dispatchTk :: String -> Dispatcher EventInfo -> IO ()
dispatchTk ('O' : 'K' : tl) disp = 
        sendReply (Right (drop 1 tl)) disp
dispatchTk ('E' : 'V' : str) disp = 
        dispatch disp (ObjectID (read oid), eid) (parseInfo fields) done
        where (oid : eid : fields) = words str
dispatchTk ('E' : 'R' : tl) disp = 
        sendReply ((Left . userError . (drop 1)) tl) disp
dispatchTk ('E' : 'X' : _) disp = do {
        tk <- getUnixProcessID disp;
        signalProcess sigKILL tk;
        deadlock
        }
dispatchTk _ _ = error "Fatal Wish Error: illegal reply from wish"


-- --------------------------------------------------------------------------
-- GUIEvent Instance 
-- --------------------------------------------------------------------------

instance EventDesignator (Object.ObjectID, [Char]) where
        toEventID (oid,patid) = EventID oid patid
