{- -----------------------------------------------------------------------
 -
 - XGit - Basic types and classes
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 -}

module XGitTypes where

import Exception (throwDyn, catchDyn)
import Dynamics

import MarkupText(MarkupText)
import Maybe 
import IOExts (unsafePerformIO, readIORef, writeIORef, newIORef)

import PGIP (ProverCmd, ProverAns, CmdId, PrfStateId)
                         
-- Object values are markup text, as far as XGit is concerned
type ObjVal = MarkupText

-- Types of objects.

data ObjType = UserT String -- arbitrary user-defined types
             | ProofT | TextT | SubstT -- proofs, texts, substitutions
             deriving (Eq, Ord)

-- An operation (or proof operation) essentially represents something
-- which for given objects returns a proof command. 

-- It has a name, a target type, and for given objects
-- returns a command line plus the name of the resulting object.
-- Note that we can only return user-defined types, in particular
-- not ProofT; for this, see ProofOpn below.
-- An operation also has a source type, which is given implicitly by
-- getOpns in Prover.
data Opn = Opn { opname :: String,
                 opcmd  :: [Obj]-> (ProverCmd, String),
		 target :: String }
         | GoalOpn { opname :: String,
	             thy    :: String,
		     goal   :: String }

-- Proof commands (tactics and the back command)
data ProofOpn = ProofOpn { popname :: String,
                           prfcmd  :: [Obj]-> ProverCmd
			 }              

-- The following are our objects
-- * basic objects are given by an identifier (e.g. Isabelle's
--   theories or theorems). They may depend on an external object,
--   wherein they are defined; this means we have to load the external
--   object into the tool before we can access the basic object.
--   They may also depend on other objects, e.g. theorems which have
--   been proven within the system will depend on the proof.
-- * derived objects are given by applying commands
-- * construction objects are given by applying sequences of commands

data Obj = BasicObj { oid  :: ObjId,
	              val  :: Maybe ObjVal,
		      name :: String,
		      typ  :: ObjType,
		      cmd  :: Command }
		      -- ext :: [ExtObjs]
	 | DerivedObj { oid  :: ObjId,
	                val  :: Maybe ObjVal,
		        name :: String,
			typ  :: ObjType,
   		        cmd  :: Command }
         | ConstrObj { oid   :: ObjId,
		       val   :: Maybe ObjVal,
		       sid   :: PrfStateId,
		       name  :: String,
 		       typ   :: ObjType,
		       cmd   :: Command,
		       hist  :: History }

-- A history is a past and a future, given as a list of commands.
-- Note we keep the past the wrong way around (i.e. last first), for
-- easier manipulation.
data History  = History { past   :: [Command],
                          future :: [Command]}

-- Finally, a command is given by a prover cmd, plus a list of object
-- ids used in the prover command (later possibly a focus?)
data Command  = Command ProverCmd [ObjId]
        

-- The object id's serve to give fast equality on objects
-- (obviously, this depends on always giving fresh id's)
newtype ObjId = ObjId Int deriving (Eq, Ord)

instance Show ObjId where
   showsPrec _ (ObjId n) r = "v"++ (show n) ++ r

newObjId :: IO ObjId
newObjId = do c<- readIORef r
              writeIORef r (c+ 1)
              return (ObjId c)
           where r = IOExts.unsafePerformIO (newIORef 0)

instance Eq Obj where 
   o1 == o2 = oid o1 == oid o2

isOutdated :: Obj-> Bool
isOutdated o = isNothing (val o) -- william, it was really nothing

getVal :: Obj-> ObjVal
getVal o = 
  case val o of Just v -> v 
                Nothing -> throwDyn (ExnWarning "An object has been found out of date (rather unexpectedly, I may add.") 
       


-- Errors and Warnings: Exception handling.
--
-- They can be thrown anywhere during the execution of a command, and
-- are caught in one place; this way, we have one central place to do
-- the error handling.
data XGitExn = ExnError String
             | ExnWarning String
             | ExnInfo String
	     | ExnStatus String

-- This piece of code straight from Dr. Russell:
instance HasTyCon XGitExn where
   tyCon _ = mkTyCon "XGit" "xGitExn"

panic :: String-> a
panic msg = throwDyn (ExnError ("Internal Error - please report it as a bug!\n[Debug data: "++ msg++ "]"))

