{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - $Source$
 - $Release$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 -}

module XGit where


import Maybe(isJust,isNothing,fromJust)
import List(find)
import Exception (throwDyn, catchDyn)
import IOExts (unsafePerformIO, readIORef, writeIORef, newIORef)
import ReferenceVariables
import HTk
import Dynamic
import FiniteMap
import GenGUI

-- -----------------------------------------------------------------------------

-- Temp:
type MarkupText = String

-- ----------------------------------------------------------------------
-- 
-- Basic types and classes
-- 
-- ----------------------------------------------------------------------


-- 1. The general setup: 

-- A command is a command line which is sent to the tool, plus the id
-- of the objects used in it.
data Command = Command Cmdline [ObjId]

-- An command line is just a string.
newtype Cmdline = Cmdline String 

-- Can't just derive, or we'd have "Cmdline .." in front...
instance Show Cmdline where
   showsPrec _ (Cmdline s) r= s ++ r
instance Read Cmdline where
   readsPrec p b = let bs = (readsPrec p b) ::[(String, String)] 
                   in  map (\(s, r)-> (Cmdline s, r)) bs

-- Object values are markup text, as far as XGit is concerned
newtype ObjVal = ObjVal MarkupText

-- Types

-- Types are either tool defined, or builtin. The latter comprise
-- proofs, texts and substitutions. We might add more later, and
-- it would also be desirable to have the tool add predefined
-- types... 
data ObjType = UserType String 
             | ProofT | TextT | SubstT

-- construction types: can be edited in the construction area 
isConstrType :: ObjType-> Bool
isConstrType ProofT = True
isConstrType _      = False

-- stateful types: onyl the value matters
isStatefulType :: ObjType-> Bool
isStatefulType TextT  = True
isStatefulType SubstT = True
isStatefulType _      = False

-- An operation is a command line, plus the name and type of the
-- resulting object.
data Opn = Opn { opcmd :: Cmdline,
                 defName :: String,
		 optyp :: ObjType }

instance Show Opn where
  showsPrec _ Opn{ opcmd= c } r = show c ++ r

-- The following are our objects
-- * basic objects are given by an identifier (e.g. Isabelle's
--   theories or theorems). They may depend on an external object,
--   wherein they are defined; this means we have to load the external
--   object into the tool before we can access the basic object.
-- * stateful objects never get outdated (examples are texts, or
--   substitions (lists of pairs of strings). In turn, dependencies to
--   stateful objects are not recorded (if a text is used for something,
--   only the momentary value of the text matters).
-- * derived objects are given by applying commands
-- * construction objects are given by applying sequences of commands

data Obj = BasicObj { oid  :: ObjId,
	              val  :: Maybe ObjVal,
		      name :: String,
		      typ  :: ObjType }
	 | DerivedObj { oid  :: ObjId,
	                val  :: Maybe ObjVal,
		        name :: String,
			typ  :: ObjType,
   		        cmd  :: Command}
         | ConstrObj { oid   :: ObjId,
		       val   :: Maybe ObjVal,
		       name  :: String,
 		       typ   :: ObjType,
		       cmd   :: Command,
		       hist  :: History}

-- A history is a past and a future, given as a list of commands, plus
-- intermediate states.
-- Note we keep the past the wrong way around (i.e. last first), for
-- easier manipulation.
data History  = History { past   :: [(Command, Maybe ObjVal)],
                          future :: [(Command, Maybe ObjVal)]}
        
-- The object id's serve to give fast equality on objects
-- (obviously, this depends on always giving fresh id's)
newtype ObjId = ObjId Int deriving (Eq, Ord)

instance Show ObjId where
   showsPrec _ (ObjId n) r = "v"++ (show n) ++ r

newObjId :: IO ObjId
newObjId = do c<- readIORef r
              writeIORef r c
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
       

-- A tool (all PGWin instances are instance of this tool)

-- These are responses to a command
data Response v = OK v   -- command succeeded, return value is v
	        | Error MarkupText    -- e.g. "ERROR", "no unifier"
                | Warning MarkupText  -- e.g. "command aborted"
		deriving (Eq, Show)

-- A tool can evaluate commands, and resolve drag&drop
class Tool t where
   eval        :: t-> Cmdline-> IO (Response ObjVal) -- evaluate a command
   getOpns     :: t-> [Obj]-> IO [Opn]  
                  -- get all possible opns, and the name for the new object
	          -- XGit will resolve ambiguties (by a pop-up menu)
		  -- also used for the unary operations pop-up menu
		  -- (In future, we may want to distinguish between
		  -- "get all possible opn's" and "get useful operations")
   getTypeIcon :: t-> ObjType-> IO Image
                  -- get icon for type


-- 3. Miscellenea 

-- Exceptions.
--
-- They can be thrown anywhere during the execution of a command, and
-- are caught in one place; this way, we have one central place to do
-- the error handling.
data XGitExn = ExnError String
             | ExnWarning String
             | ExnInfo String

msgOfExn :: XGitExn -> String
msgOfExn (ExnError e)   = e
msgOfExn (ExnWarning e) = e
msgOfExn (ExnInfo e)    = e

instance Typeable XGitExn where
    typeOf ex = typeOf (msgOfExn ex) -- you gotta wonder about them Typeables...

panic :: String-> a
panic msg = throwDyn (ExnError ("Internal Error - please report it as a bug!\n[Debug data: "++ msg++ "]"))

-- ----------------------------------------------------------------------
-- 
-- XGit state
-- 
-- ----------------------------------------------------------------------

-- The state contains all known obects.
objState :: Ref (FiniteMap ObjId Obj)
objState = IOExts.unsafePerformIO (newRef emptyFM) 

-- Das Tool: wrap it up in a datatype
data ToolD = forall t. Tool t=> ToolD t

instance Tool ToolD where
   eval (ToolD t) = eval t
   getOpns (ToolD t) = getOpns t
   getTypeIcon (ToolD t)= getTypeIcon t

-- wrap up the datatype in a reference
newtype Toolref = Toolref (Ref ToolD)

instance Tool Toolref where
   eval (Toolref tr) c = getRef tr >>= \t-> eval t c
   getOpns (Toolref tr) o = getRef tr >>= \t-> getOpns t o
   getTypeIcon (Toolref tr) ot = getRef tr>>= \t-> getTypeIcon t ot

-- This is the reference:
tool :: Toolref
tool = Toolref (IOExts.unsafePerformIO (newRef (ToolD (()))))

instance Tool () where -- it's either this or a Ref (Maybe ...)

setTool :: Tool t=> t-> IO ()
setTool t = let Toolref tr= tool in setRef tr (ToolD t)
          

-- Miscellenea

-- The construction area:
conArea ::Ref (Maybe Obj)
conArea = IOExts.unsafePerformIO (newRef Nothing)

-- Flag indicating wether new operations are inserted into history,
-- or delete the future.
historyInsert :: Ref Bool 
historyInsert = IOExts.unsafePerformIO (newRef False)



-- 
-- The export interface (for the state)
--
-- It would be appropriate not to export the value of the XObj's, but 
-- creating another data structure seems to much bother...

data XState = XState { objs       :: [Obj],
		       constrArea :: Maybe Obj,
		       histIns    :: Bool}

getState :: IO XState
getState =
  do ca <- getRef conArea
     hi <- getRef historyInsert     
     objs <- getRef objState
     return XState{objs= map snd (fmToList objs),
                   constrArea= ca, histIns= hi}

start    :: XState-> IO ()
start (XState{objs= obs, constrArea= ca, histIns= hi}) =
  do let fm= addToState obs emptyFM 
     setRef objState fm
     setRef conArea ca
     setRef historyInsert hi
     -- TBD: set up the threads here.

-- Missing:
-- changeVal :: Obj c v => Obj-> v-> IO ()

-- ----------------------------------------------------------------------
-- 
-- GenGUI interface
-- 
-- ----------------------------------------------------------------------

instance CItem Obj where
  getIcon ob = getTypeIcon tool (typ ob)
  getName ob = return Name{full= name ob, short = \n-> cut n (name ob)}
               where cut n str = 
                      let l= length str
                      in  if l>n then take n (drop ((l-n) `div` 2) str)
                             else str
                      -- cut the middle part of the name  (and why not)
  

{- react to GenGUI-events: 

  doubleClickInNotepad   - open in Construction Area (if not already open) 
  droppedOnItemInNotepad - trigger operation
  rightClickInNotepad    - pop-up menu
  droppedInArea          - trigger operation in construction area

-}

-- ----------------------------------------------------------------------  
-- 
-- Simple object operations: drag & drop and the pop-up menu
--
-- ----------------------------------------------------------------------  

-- drop items on a single item
droppedOn :: Item Obj-> [Item Obj]-> IO ()
droppedOn it drops =
  let args= map content (it:drops)
  in  do opn <- resolveOpn args
         case opn of Just op -> singleOpn op args
                     Nothing -> status "No operation for that gesture."      
                        
-- execute a single operation (with args)
singleOpn :: Opn-> [Obj]-> IO ()
singleOpn _ [] = done -- clearly daft
singleOpn (opn@Opn{opcmd= c, optyp= ty}) args@(ob:_) =
  if isStatefulType ty then
  -- special case: executing a single operation on a stateful object
  -- we only change the state of the object
     do nuv<- execCmd c
        updObjNoDep (ob{val= Just nuv}) -- TBD: check depencies!
  else do nuv<- execCmd c
          if isConstrType ty then newConstrObj nuv opn (map oid args)
	                     else newDerivedObj nuv opn (map oid args)

-- Standard operations: show, rename, delete
showObj :: Obj-> IO ()
showObj ob =
  case val ob of 
    Just v-> putStrLn "Displaying." -- UtilWin.display (name ob) v
    Nothing -> putStrLn "Object out of date." -- UtilWin.info (name ob ++ "is currently out of date.")
    		    -- TBD: add a link to show the history, perhaps show
                    --      type as well.	

renameObj :: Obj-> IO ()
renameObj ob =
  do nu <- {- UtilWin.enterLine ("Renaming "++ ob)
                             "Please enter new name:"
			     (name ob) -} return "*new name*"
     updObjNoDep (ob{name= nu})
     -- udpate icon!
     status ("Name has been changed to \""++nu++"\".")

deleteObj :: Obj-> IO ()
deleteObj ob = done
   -- TBD: put deleted objects into a "clipboard", so we can undelete
   --      them.
   -- Note we only garbage collect on save, to allow undeletion

-- The unary operations pop-up menu
-- The menu is composed "on the fly", meaning we rebuild it every time
-- we receive a right-click from gengui. This allows us easier event
-- handling, since the event can have appropriate reaction built in.
popupMenu :: Widget w=> w-> Position-> Item Obj-> IO ()
popupMenu w wh it =
  do m   <- createMenu w False []
     sev <- stdOps (content it) m
     cev <- unaryOps (content it) m
     popup m wh (Nothing::Maybe HTk) 
     spawnEvent (sev +> cev) -- +> destroyed m) <-- that doesn't work
     done                       

-- The object-specific unary operations for the pop-up menu
-- This get passed  a menu, where the operations are inserted,
-- and it returns a composed event which is the action of the menu
-- (i.e. if we sync on it, it waits for the menu to occur, and takes
--  appropriate action)
unaryOps :: Obj-> Menu -> IO (Event ())
unaryOps ob menu =	
  if isOutdated ob then
     return (always done)
     else do ops <- getOpns tool [ob]
	     if not (null ops) then 
                do createMenuSeparator menu [] 
                   menuevents <- 
		     mapM (\opn-> do b<- createMenuCommand menu 
                                                           [text (show opn)]
                                     c<- clicked b 
                                     return (c >>> singleOpn opn [ob])) ops
                   return (choose menuevents)
                else return (always done)

-- The standard operations for an object.
-- For signature, see above.
stdOps :: Obj-> Menu-> IO (Event ())
stdOps obj menu =
  do b<- createMenuCommand menu [text "Show"]
     showev <- clicked b
     b<- createMenuCommand menu [text "Delete"]
     delev  <- clicked b
     b<- createMenuCommand menu [text "Rename"]
     renev  <- clicked b
     b<- createMenuCommand menu [text "Update"]
--   disable if up-to-date!
--   updev  <- clicked b
     return   ((showev >>> showObj obj)
            +> (delev >>> deleteObj obj)
            +> (renev >>> renameObj obj))
--          +> (updev >>>))

                                     
                   
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
      do co  <- getRef conArea
         if isNothing co then setConArea obj
            else status "Another object is already open."
    _ -> status "Not a proof." 

-- handle objects being dropped into construction area
droppedIntoConArea :: [Item Obj]-> IO ()
droppedIntoConArea its =
  doWithConArea 
    (\cobj-> do let objs= map content its
                opn <- resolveOpn (cobj:objs)
                case opn of
                   Just op -> execConAreaOpn op cobj objs
                   Nothing -> status "No command for that gesture.")

-- execute one command (with poss. args) on the current construction object
-- (exported)
conAreaCmd :: Cmdline-> [Obj]-> IO ()
conAreaCmd c args = 
  doWithConArea (\co-> let opn = Opn {opcmd= c, defName= name co,
                                      optyp= typ co}
                       in execConAreaOpn opn co args)

-- execute a command 
execConAreaOpn :: Opn-> Obj-> [Obj]-> IO ()
execConAreaOpn op cobj args =
  do nu_v <- execCmd (opcmd op)
     if isConstrType (optyp op) then 
        do ins<- getRef historyInsert
           let History{past= p, future= fut}= hist cobj
               nu= cobj{hist= History{past= p++ [(cmd cobj, val cobj)],
				      future= if ins then fut else []},
	                val= Just nu_v, cmd= Command (opcmd op) (map oid args)}
	       -- TBD: Keep track of dependencies here!
	       --      We just forget about the future-- this may cause
               --      problems if other objects depend on that, in particular
               --      if they depend on the finished construction object.
           setConArea nu         
        else do closeConArea 
                newDerivedObj nu_v op (map oid args)


-- close construction area
closeConArea :: IO ()
closeConArea =
  do setRef conArea Nothing
     -- clearTextArea
     -- clearCmdLine
     -- if history window open, close that as well                

doWithConArea :: (Obj-> IO())-> IO ()
doWithConArea action = 
  do co<- getRef conArea 
     case co of 
        Just cobj -> action cobj
                        -- update con area
        Nothing   -> status "No proof currently opened."

-- set the construction area to this object, and sync display
-- does not check wether con area already open or this is construction
-- object. The construction object may be out of date.
setConArea :: Obj-> IO ()
setConArea nu = 
  do setRef conArea (Just nu)
     case val nu of
        Just (ObjVal v) -> putStrLn v -- updateTextArea 
        Nothing -> done -- clearTextArea -- outdated/before start of history 
     -- updateCommandLine (head (past (history nu))) -- display last command
     -- udpate history window, if open

-- TBD: this needs to set the state of the prover in some fashion--
--      we _must_ make sure that the state of the prover corresponds
--      to what is displayed from the construction object here.
--      Perhaps get rid of caching for construction objects? 		      

-- ----------------------------------------------------------------------  
-- 
-- Handling the history. 
--
-- ----------------------------------------------------------------------  

histForward :: IO ()
histForward =
  doWithConArea 
     (\cobj@ConstrObj{hist= h, val=v, cmd=c}->
	  case h of
            History{past= p, future= []}-> 
                throwDyn (ExnInfo "At the end of history -- cannot go forward.") 
            History{past= p, future= (fc@(Command fcmd fids), Nothing):r}->
                -- future out of date -- try to replay
                do args <- mapM lookupObj fids 
                   if any isOutdated args then 
                         do status "Cannot replay history -- arguments outdated."
		            setConArea(cobj{hist= History{past= (c, v):p,
		 		                          future= r},
                                            val= Nothing, cmd= fc})
                      else 
		         do nu_v <- execCmd fcmd
			    -- every operation which is part of the history
                            -- produces a construction object, so we don't
                            -- have to check for endConstr here..
                            setConArea(cobj{hist= History{past= (c, v):p,
				                         future= r}, 
                                            cmd= fc, val= Just nu_v})
            History{past= p, future= (fc, fv):fs}-> 
	        -- future up-to-date -- can use cache
                setConArea (cobj{hist= History{past= (c, v):p, future= fs},
				 cmd=fc, val= fv})
     )

histBackward :: IO ()
histBackward =
  doWithConArea
    (\cobj@ConstrObj{hist=h, val=v, cmd=c}->
	  case h of
             History{past= []}-> 
                  throwDyn (ExnInfo "At the start of history.")
             History{past= (pc, pv):ps, future= f}-> 
                  setConArea(cobj{hist= History{past= ps,
				                future= (c,v):f}, 
                                  cmd= pc, val= pv})
		  -- here, replaying doesn't make sense
    )

clearFuture :: IO ()
clearFuture =
  -- UtilWin.confirm (doWithConArea 
  (doWithConArea 
       (\cobj@ConstrObj{hist= h}-> setConArea (cobj{hist= h{future= []}})))


-- ----------------------------------------------------------------------  
-- 
-- Handling the object state: introduce new objects, 
--
-- ----------------------------------------------------------------------  

-- introduce a new derived object
newDerivedObj :: ObjVal-> Opn-> [ObjId]-> IO ()
newDerivedObj v opn args =
   newObject(DerivedObj{val= Just v, name= defName opn, typ= optyp opn,
		        cmd= Command (opcmd opn) args})

-- introduce a new construction object
newConstrObj :: ObjVal-> Opn-> [ObjId]-> IO ()
newConstrObj v opn args =
   newObject(ConstrObj{cmd= Command (opcmd opn) args, val= Just v, 
                       name= defName opn,
		       hist= History{future= [], -- there's no future 
			                          -- in England's dreaming
			              past= []}, typ= optyp opn}) 

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


-- dependency handling:
-- get id of all objects this object depends on
dependsOn :: Obj-> [ObjId]
dependsOn (BasicObj {}) = []
dependsOn (DerivedObj{cmd= Command _ oids}) = oids
dependsOn (ConstrObj{hist= History{past= p}, cmd= Command _ oids}) = 
	           concat (map (\((Command _ oid), _) -> oid) p) ++ oids

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


-- add a list of objects to the state:
-- * give objects new id, renaming references to old ids
-- * recalculate _all_ values
-- * if an error occurs on recalculation, keep depending objects
--   but have them outdated; in a construction object, stop history at
--   that point.
addToState :: [Obj]-> FiniteMap ObjId Obj
                                         -> FiniteMap ObjId Obj
addToState objs state = error "Not implemented yet"


-- ----------------------------------------------------------------------  
-- 
-- Utilitilities
--
-- ----------------------------------------------------------------------  


-- Handling status messages. We keep track of older ones, so we
-- can redisplay them (if they flash by too quickly).
oldMsgs :: Ref [String]
oldMsgs = IOExts.unsafePerformIO (newRef [])

status :: String-> IO ()
status msg =
  do putStrLn ("STATUS: "++ msg) -- GenGUI.statusmsg msg
     changeRef oldMsgs (\m-> m++ [msg])

-- TBD: What about the length of the messages?
-- TBD: set up a watchdog here, which keeps track of the status
-- messages. If we are idle too long, display commercials in status
-- line. Sell advertising space and get incredibly rich. 

prevStatusMsg :: IO ()
prevStatusMsg = 
  do msgs<- getRef oldMsgs
     case msgs of  []     -> putStrLn ("STATUS: No previous message.") -- GenGUI.status "No previous message."
                   (p:ps) -> do putStrLn ("STATUS: "++ p) -- GenGUI.status p 
                                setRef oldMsgs ps

-- Utilities

-- Execute one command line, raise any errors into the error handling monad. 
execCmd:: Cmdline-> IO ObjVal
execCmd c = 
  do a<- eval tool c
     case a of 
       OK v      -> return v
       Error e   -> throwDyn (ExnError e)
       Warning w -> throwDyn (ExnWarning w)

-- Resolve drag&drop - also ensures no argument is outdated.
resolveOpn :: [Obj]-> IO (Maybe Opn)
resolveOpn args = 
  if all (not . isOutdated) args then 
        do ops <- getOpns tool args
           case ops of 
               [] -> return Nothing
               opn:_ -> return (Just opn)
	                -- Post a pop-up menu here to resolve amibiguities.
     else throwDyn (ExnInfo "One or more arguments are out of date.")
