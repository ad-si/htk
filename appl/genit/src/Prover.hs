{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - Encapsulation of a PGIP-compliant prover.
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 -}

module Prover(
       -- send commands to prover
       -- all of these throw DynExn
       sendCmd, -- :: ProverCmd -> IO ()
       evalCmd, -- :: ProverCmd -> IO MarkupText 
       evalCmdT, -- :: ProverCmd -> IO (ObjType, MarkupText)
       getPrfState, -- :: IO (Maybe PrfStateId)x
       getOpns,  -- :: [ObjType] -> IO [Opn]
       getProofOpns, -- :: [ObjType]-> IO [ProofOpn]
       -- getTypes, -- :: IO [ObjType]
       getTypeIcon, -- :: ObjType -> IO Image

       start, -- :: FilePath-> IO 
       stop, -- :: IO () -- via shutdown from InfoBus ?
       sendINT, -- :: IO () 
) where


import FiniteMap
import IOExts
import Posix
import Char
import Exception(Exception(..))
import Dynamics (toDyn)

import Computation
import Concurrent
import Destructible
import Events hiding(send)
import Channels
import Synchronized
import Spawn
import ReferenceVariables
import ChildProcess

import HTk(AbstractWidget(NONE)) 
import Image
import MarkupText

import XGitTypes
import PGIP

-- This module is structured in two parts:
-- Part I implements a type Prover, representing one PGIP compliant prover;
-- Part II wraps this up into the interface above.

-- ----------------------------------------------------------------------
--
-- Part I: The type Prover, plus its operations
--
-- ----------------------------------------------------------------------

-- A request is given by an mvar into which the answer is to be put. 
-- The answer may be an error (to be raised in the calling thread).
type Requests = MVar (Answer ProverAns)

-- Data structure representing a prover.
-- In particular, we keep track here of the prover's proof state.
data Prover = Prover { proc :: ChildProcess,
                       req  :: Channel Requests,
		       conf  :: Ref (ProverConfig),
	               killHandler :: IO(),
		       prfState :: Ref (Maybe PrfStateId)
                     }

instance Synchronized Prover where
   synchronize (Prover {prfState = p}) = synchronize p

instance Destroyable Prover where
   destroy (Prover{proc= p, killHandler= r}) = r >> destroy p

-- Return a new prover.
-- For each prover, we spawn a thread which listens to input from that
-- prover. If we send the prover a command, we wait for the reader
-- thread to read the answer and pass it back to us.
newProver :: FilePath -> IO Prover
newProver fp = 
  do t<-  newChildProcess fp [linemode False, chunksize 50000]
     r<-  newChannel
     conf <- newRef mtConfig
     kill<- spawn (reader t r conf)
     x <- newRef (Nothing:: Maybe PrfStateId)
     return Prover {proc= t, req= r, killHandler= kill, prfState= x,
                    conf= conf}

-- The reader thread.
-- If we read something we cannot parse, we flush all outstanding
-- requests since we're not sure wether this is a malformed answer or
-- spurious prover output. 
reader :: ChildProcess-> Channel Requests-> Ref ProverConfig-> IO ()
reader t reqchan conf = 
  do catch (do m <- readMsg t
               case PGIP.parseMsg m of
	          Just (ProverAns ans) -> do ma <- receiveIO reqchan
		        	             putMVar ma ans	  
		  Just (ProverConf (GUIConf c)) -> 
                     changeRef conf (changeGUIConfig c)
                  Nothing -> do flush reqchan
		                log "Cannot parse command.")
           (\e-> do flush reqchan
                    log ("Exception "++ show e++ " raised."))
     reader t reqchan conf where
        log msg = putStrLn ("Prover: " ++ msg)
	-- This should get all requests queued in the channel,
	-- and send them a warning exception as an answer.
	flush reqchan =
	   do mvars <- getAllQueued (receive reqchan)
	      mapM (\m-> putMVar m (Left (DynException (toDyn (ExnWarning "Unexpected answer from prover (command flushed)."))))) mvars
 
-- Send a command to the prover. If we send a state-changing command
-- (Goal and Restore) we have the prover save its state before we 
-- pass on the command.
send :: Prover-> ProverCmd-> IO (Maybe ObjType, MarkupText)
send prv (cmd@GoalCmd{}) =     saveState prv >> send' prv cmd
send prv (cmd@RestoreCmd {}) = saveState prv >> send' prv cmd
send prv cmd =                 send' prv cmd

  
send' :: Prover-> ProverCmd-> IO (Maybe ObjType, MarkupText)
send' prv cmd = 
  do mv <- newEmptyMVar :: IO (MVar (Answer ProverAns))
     -- send command to prover, put request into answer channel
     -- must synchronize so mvars in req-channel are always in correct order
     synchronize prv (do sendMsg (proc prv) (PGIP.showCmd cmd) 
                         sendIO  (req prv) mv)
     -- wait for answer
     ans <- readMVar mv >>= propagate
     case ans of 
       PrfState id sd -> do setRef (prfState prv) id
                            return (Just ProofT, sd)
       PrfStatus s -> return (Nothing, s)
       PrfTerm ot s-> return (Just (UserT ot), s)


saveState :: Prover-> IO ()
saveState prv =
  do s<- getState prv
     case s of Just s  -> send prv (CloseCmd s) >> done
               Nothing -> done

getState :: Prover-> IO (Maybe PrfStateId)
getState prv = getRef (prfState prv)

-- ----------------------------------------------------------------------
--
-- Configuring the prover.
--
-- ----------------------------------------------------------------------

-- These methods configure the appearance of a PGIP-encapsulated prover's gui.
data ProverConfig = ProverConfig {
     prvName   :: String,
     prvOps    :: FiniteMap [ObjType] [Opn],
     prvPrfOps :: FiniteMap [ObjType] [ProofOpn],
     prvTypes  :: [ObjType],
     prvIcon   :: FiniteMap ObjType (IO Image)
     }

-- The empty configuration
mtConfig =
  ProverConfig {prvName = "",
                prvOps  = emptyFM,
                prvPrfOps = emptyFM,
		prvTypes  = [TextT, ProofT, SubstT],
		prvIcon   = emptyFM}

-- changing the configuration
changeGUIConfig :: GUIConf-> ProverConfig-> ProverConfig
changeGUIConfig (NewType{PGIP.name= n, icon= i}) conf =
  conf{prvTypes= (UserT n): prvTypes conf,
       prvIcon = addToFM (prvIcon conf) (nameToType n) 
                          (newImage NONE [imgData GIF i])}
changeGUIConfig (NewOpn{src= s, trg= t, PGIP.cmd= c, PGIP.name= n}) conf =
  conf{prvOps= addToFM_C (++) (prvOps conf) (map nameToType s)
                       [Opn{opname= n, target= t,
		            opcmd= parseCmdString c}]}
changeGUIConfig (NewPrfOpn{src= s, PGIP.cmd= c, PGIP.name= n}) conf =
  conf{prvPrfOps= addToFM_C (++) (prvPrfOps conf)(map nameToType s) 
                          [ProofOpn{popname= n,
			            prfcmd = fst . (parseCmdString c)}]}

-- Hack: hardwired names for builtin types. 
-- (We should rather extend the DTD here.)
nameToType :: String-> ObjType
nameToType "Text"  = TextT
nameToType "Proof" = ProofT
nameToType "Subst" = SubstT
nameToType t       = UserT t 


-- Takes a string with substitution patterns %1 %2 
-- and substitutes the name of the n-th object into %n    
parseCmdString :: String-> [Obj]-> (ProverCmd, String)               
parseCmdString str objs = 
  let substr = substInStr str (map XGitTypes.name objs)
  in  (ProverCmd substr, substr)


-- Takes a string with substitution patterns %1 %2 
-- and substitutes the n-th string into %n    
substInStr :: String-> [String]-> String
substInStr str substs =
  parse str (substN substs) where
    parse :: String-> (Int-> String)-> String
    parse []  _ = []
    parse [c] _ = [c]
    parse ('%':c:r) subst =    
       if isDigit c then
          let (restnum, rest)= span isDigit r
              num = read (c:restnum)
          in  (subst num) ++ (parse rest subst)
       else if c == '%' then '%':(parse r subst)
            else '%':c:(parse r subst)
    parse (c:r) subst = c:(parse r subst)

    substN :: [String]-> Int-> String
    substN xs n = foldl (\s (m, y)-> if n== m then y else s) "" (zip [1..] xs)


-- ----------------------------------------------------------------------
--
-- Part II: The export interface
--
-- ----------------------------------------------------------------------


prover :: IORef (Maybe Prover)
prover = IOExts.unsafePerformIO (newIORef Nothing)

getProver :: IO Prover
getProver = 
  do p<- readIORef prover
     case p of Just p-> return p
               Nothing-> panic "Prover not initialised."

getPrfState :: IO (Maybe PrfStateId)
getPrfState = getProver >>= getState

sendCmd :: ProverCmd -> IO ()
sendCmd cmd = getProver >>= \prv-> send prv cmd >> done

evalCmd :: ProverCmd-> IO MarkupText
evalCmd cmd = getProver >>= \prv-> send prv cmd >>= (return . snd)

evalCmdT :: ProverCmd-> IO (Maybe ObjType, MarkupText)
evalCmdT cmd = getProver >>= \prv-> send prv cmd

start :: FilePath -> IO ()
start fp = do p<- newProver fp
              writeIORef prover (Just p)

stop :: IO ()
stop = getProver >>= destroy

sendINT :: IO ()
sendINT = 
  do p<- getProver 
     pid <- getUnixProcessID (proc p)
     Posix.signalProcess Posix.sigINT pid

getProverConfig :: IO ProverConfig
getProverConfig = getProver >>= getRef . conf

-- get all operations of this source type
getOpns      :: [ObjType]-> IO [Opn]
getOpns src = do c<- getProverConfig
	         return (lookupWithDefaultFM (prvOps c) [] src)

-- get all proof operations of this source type
getProofOpns :: [ObjType]-> IO [ProofOpn]
getProofOpns src = do c<- getProverConfig 
	              return (lookupWithDefaultFM (prvPrfOps c) [] src)

-- needed? 
getTypes :: IO [ObjType]
getTypes = do c<- getProverConfig
              return (prvTypes c)

-- get an icon for that type
getTypeIcon  :: ObjType-> IO Image
getTypeIcon ot = do c<- getProverConfig
	            lookupWithDefaultFM (prvIcon c) defaultIcon ot

-- the default icon. Should be a "?" (but isn't :-)
defaultIcon :: IO Image
defaultIcon = newImage NONE [imgData GIF "R0lGODlhDAAMAMIAAICAgP//AP///wAAANjY2NjY2NjY2NjY2CH5BAEKAAEALAAAAAAMAAwAAAMoGLrc8G0BQUGctD4b5viZAAxdGI7lIHyqSGKmm66sDJvopm9kwP6sBAA7"]





