{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 - This module: state handling.
 -}

module XGitState where

import Maybe(isJust,isNothing,fromJust)
import List(find)
import IOExts (unsafePerformIO, readIORef, writeIORef, newIORef)
import IO (ioeGetErrorString)
import FiniteMap
import qualified System (getEnv)

import ReferenceVariables

import GenGUI

-- import qualified PGIP 
import PGIP (ProverCmd(..), PrfStateId)
import XGitTypes
import Prover

-- ----------------------------------------------------------------------
-- 
-- XGit state
-- 
-- ----------------------------------------------------------------------

-- The export interface (for the state)
--
-- It would be appropriate not to export the value of the XObj's, but 
-- creating another data structure seems too much bother...

data XState = XState { objs       :: [Obj],
		       constrArea :: Maybe Obj,
		       histIns    :: Bool}

-- The state contains all known obects.
objState :: Ref (FiniteMap ObjId Obj)
objState = IOExts.unsafePerformIO (newRef emptyFM) 

-- The construction area:
conArea ::Ref (Maybe Obj)
conArea = IOExts.unsafePerformIO (newRef Nothing)

-- Flag indicating wether new operations are inserted into history,
-- or delete the future.
historyInsert :: Ref Bool 
historyInsert = IOExts.unsafePerformIO (newRef False)

-- Return the state 
getState :: IO XState
getState =
  do ca <- getRef conArea
     hi <- getRef historyInsert     
     objs <- getRef objState
     return XState{objs= map snd (fmToList objs),
                   constrArea= ca, histIns= hi}
     -- plus the actual GenGUI state! 

-- add a list of objects to the state:
-- * give objects new id, renaming references to old ids
-- * recalculate _all_ values
-- * if an error occurs on recalculation, keep depending objects
--   but have them outdated; in a construction object, stop history at
--   that point.
addToState :: [Obj]-> FiniteMap ObjId Obj -> IO (FiniteMap ObjId Obj)
addToState objs state = error "Not implemented yet"


-- ----------------------------------------------------------------------
-- 
-- GenGUI interface
-- 
-- ----------------------------------------------------------------------

instance CItem Obj where
  getIcon ob = getTypeIcon (typ ob)
  getName ob = return Name{full= name ob, short = \n-> cut n (name ob)}
               where cut n str = 
                      let l= length str
                      in  if l>n then take n (drop ((l-n) `div` 2) str)
                             else str
                      -- cut the middle part of the name  (and why not)

-- ----------------------------------------------------------------------  
-- 
-- Handling the object state: introduce new objects, change values
--
-- ----------------------------------------------------------------------  

-- introduce a new derived object
newDerivedObj :: ObjVal-> ProverCmd-> ObjType-> String-> [Obj]-> IO ()
newDerivedObj v cmd t nm args =
   newObject(DerivedObj{val= Just v, name= nm, typ= t, 
                        cmd= Command cmd (map oid args)})

-- introduce a new construction object
newConstrObj :: ObjVal-> ProverCmd-> PrfStateId-> [Obj]-> String-> IO ()
newConstrObj v cmd prfid args nm =
   newObject(ConstrObj{cmd= Command cmd (map oid args), sid= prfid,
                       val= Just v, name= nm, typ= ProofT,
		       hist= History{future= [], -- no future in England's dreaming
			              past= []}}) 

-- introduce a new basic object
newBasicObj :: ObjVal-> ProverCmd-> ObjType-> String-> [Obj]-> IO ()
newBasicObj v cmd t nm args =
  newObject(BasicObj{val= Just v, name= nm, typ= t,
                     cmd= Command cmd (map oid args)}) 

-- introduce a new object: id it, put it on the desktop, add it to to state
newObject :: Obj-> IO ()
newObject obj =
  do id<- newObjId
     let nu_obj = obj{oid= id}
     -- newItem nu_obj 
     updObjNoDep nu_obj

-- update a single object without checking dependencies,
-- or create a new object (which by definition doesn't have dependencies)
updObjNoDep :: Obj-> IO ()
updObjNoDep ob = 
   changeRef objState (\fm-> addToFM fm (oid ob) ob)

-- Missing:
-- updVal :: Obj-> v-> IO ()


-- dependency handling:
-- get id of all objects this object depends on
dependsOn :: Obj-> [ObjId]
dependsOn (BasicObj {cmd= Command _ oids}) = oids
dependsOn (DerivedObj{cmd= Command _ oids}) = oids
dependsOn (ConstrObj{hist= History{past= p}, cmd= Command _ oids}) = 
	           concat (map (\ (Command _ oid) -> oid) p) ++ oids

-- ... and vice versa: get id of all objects depending on this
dependencies :: ObjId-> FiniteMap ObjId Obj
                                      -> FiniteMap ObjId Obj
dependencies oid objs =
  filterFM (\_ dob-> any ((==) oid) (dependsOn dob)) objs

-- global version of that
allDependents :: Obj-> IO [Obj]
allDependents ob = 
  do fm<- getRef objState 
     return (map snd (fmToList (dependencies (oid ob) fm)))

{-
-- lookup objects in a list (needed?)
getObjFromId :: [Obj]-> ObjId-> Maybe Obj
getObjFromId obs i = find (((==) i). oid) obs
-}

-- global lookup - assumes id will be found
lookupObj :: ObjId-> IO Obj
lookupObj oid = do fm <- getRef objState
	           return (fromJust (lookupFM fm oid))


-- ----------------------------------------------------------------------  
-- 
-- Status Messages
--
-- ----------------------------------------------------------------------  

-- Handling status messages. We keep track of older ones, so we
-- can redisplay them (if they flash by too quickly).
oldMsgs :: Ref [String]
oldMsgs = IOExts.unsafePerformIO (newRef [])

status :: String-> IO ()
status msg =
  do putStrLn ("STATUS: "++ msg) -- setStatus msg
     changeRef oldMsgs (\m-> m++ [msg])

-- TBD: What about the length of the messages?
-- TBD: set up a watchdog here, which keeps track of the status
-- messages. If we are idle too long, display commercials in status
-- line. Sell advertising space and get incredibly rich. 

prevStatusMsg :: IO ()
prevStatusMsg = 
  do msgs<- getRef oldMsgs
     case msgs of  []     -> putStrLn ("STATUS: No previous message.") -- GenGUI.setStatus "No previous message."
                   (p:ps) -> do putStrLn ("STATUS: "++ p) -- GenGUI.setStatus p 
                                setRef oldMsgs ps
