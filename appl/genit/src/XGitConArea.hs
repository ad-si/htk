{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 - This module: Construction area and history handling.
 -}

module XGitConArea where

-- import IOExts (unsafePerformIO, readIORef, writeIORef, newIORef)
import Exception (throwDyn)
import IO (ioeGetErrorString)
import FiniteMap
import qualified System (getEnv)

-- import Dynamics

import ReferenceVariables
import HTk

-- import DialogWin
-- import FileDialog

import GenGUI (Item,content)

-- import qualified PGIP 
import PGIP (ProverCmd(..), PrfStateId)
import XGitTypes
import Prover
import XGitState
import XGitSimpleOps (resolveOpn)
                   
-- ----------------------------------------------------------------------
--
-- The Construction Area 
--
-- ----------------------------------------------------------------------  

-- open object in construction area (checks wether con.area open &c)
openInConArea :: Item Obj -> IO ()
openInConArea it = 
  case content it of 
    obj@(ConstrObj{}) -> 
      do co <- getRef conArea
         case co of 
	    Just c-> do sendCmd (RestoreCmd (sid c))
	                setConArea obj
            Nothing -> status "Another object is already open."
    _ -> status "Not a proof." 

-- handle objects being dropped into construction area
droppedIntoConArea :: [Item Obj]-> IO ()
droppedIntoConArea its =
  do cobj <- getConArea 
     let objs= map content its
     pos <- resolveOpn getProofOpns objs
     case pos of
        Just (ProofOpn{prfcmd=prfcmd}) 
                           -> doConAreaCmd cobj (prfcmd objs) objs
        Nothing -> status "No command for that gesture."

-- execute one prover command
execProverCmd :: ProverCmd-> [Obj]-> IO ()
execProverCmd cmd args = getConArea >>= \co-> doConAreaCmd co cmd args

-- execute one prover command (with possible args) on the construction object
doConAreaCmd :: Obj-> ProverCmd-> [Obj]-> IO ()
doConAreaCmd cobj pcmd args =
  do nu_v <- evalCmd pcmd
     ins<- getRef historyInsert
     let History{past= p, future= fut}= hist cobj
         nu= cobj{hist= History{past  = (cmd cobj):p,
	 	                future= if ins then fut else []},
	          val= Just nu_v, 
		  cmd= Command pcmd (map oid args)}
	       -- TBD: Keep track of dependencies here!
	       --      We just forget about the future-- this may cause
               --      problems if other objects depend on that, in particular
               --      if they depend on the finished construction object.
     setConArea nu         

-- execute "QED" command (i.e. close proof)
-- Takes the name of the thm as argument
execConAreaQED :: String-> IO ()
execConAreaQED thmnm = 
  do co <- getConArea
     (ot, nu_v) <- evalCmdT (QedCmd thmnm)
     let t= case ot of 
                Nothing-> error "Prover did not return type.\n (Prover may be misconfigured?)"
		Just ot-> ot
     clearConArea
     newBasicObj nu_v (QedCmd thmnm) t thmnm [co]

-- close construction area: save state, then clear
closeConArea :: IO ()
closeConArea =
  do co <- getConArea 
     sendCmd (CloseCmd (sid co)) 
     clearConArea


-- clear construction area (no save state)
clearConArea :: IO ()
clearConArea =
  do setRef conArea Nothing
     -- clearTextArea
     -- clearCmdLine
     -- if history window open, close that as well                

getConArea :: IO Obj
getConArea =
  do co<- getRef conArea 
     case co of 
        Just cobj -> return cobj
        Nothing   -> throwDyn (ExnStatus "No proof currently opened.")

-- set the construction area to this object, and sync display
-- does not check wether con area already open or this is construction
-- object. The construction object may be out of date.
-- It does not sync with the prover; this is done in openInConArea above.
setConArea :: Obj-> IO ()
setConArea nu = 
  do setRef conArea (Just nu)
     case val nu of
        Just v  -> putStrLn "Hello" -- updateTextArea 
        Nothing -> done -- clearTextArea -- outdated/before start of history 
     -- updateCommandLine (head (past (history nu))) -- display last command
     -- udpate history window, if open

-- ----------------------------------------------------------------------  
-- 
-- Handling the history. 
--
-- ----------------------------------------------------------------------  

histForward :: IO ()
histForward =
  do cobj@ConstrObj{hist= h, val=v, cmd=c} <- getConArea
     case h of
            History{past= p, future= []}-> 
                throwDyn (ExnInfo "At the end of history -- cannot go forward.") 
            History{past= p, future= (fc@(Command fcmd fids)):r}->
                -- try to replay future
                do args <- mapM lookupObj fids 
                   if any isOutdated args then 
                         do status "Cannot replay history -- arguments outdated."
		            setConArea(cobj{hist= History{past= c:p,
		 		                          future= r},
                                            val= Nothing, cmd= fc})
                      else 
		         do nu_v <- evalCmd fcmd
			    -- every operation which is part of the history
                            -- produces a construction object, so we don't
                            -- have to check for ConstrT here.
                            setConArea(cobj{hist= History{past= c:p,
				                         future= r}, 
                                            cmd= fc, val= Just nu_v})
    

histBackward :: IO ()
histBackward =
  -- Backward is handled by the UndoCmd of the prover
  do cobj@ConstrObj{hist=h, cmd=c} <- getConArea
     case h of
             History{past= []}-> 
                  throwDyn (ExnInfo "At the start of history.")
             History{past= pc:ps, future= f}-> 
	       do v<- evalCmd UndoCmd
                  setConArea(cobj{hist= History{past= ps,
				                future= c:f}, 
                                  cmd= pc, val= Just v})
    

clearFuture :: IO ()
clearFuture =
  -- UtilWin.confirm (doWithConArea 
  do cobj@ConstrObj{hist= h}<- getConArea
     setConArea (cobj{hist= h{future= []}})

