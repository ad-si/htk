{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 - This module: Simple operations (drag&drop and the pop-up menu)
 -}

module XGitSimpleOps where

import IOExts
import Exception (throwDyn)
import FiniteMap

-- import Dynamics

import ReferenceVariables
import HTk

-- import DialogWin
-- import FileDialog

import GenGUI (Item, content)

-- import qualified PGIP 
import PGIP (ProverCmd(..), PrfStateId)
import XGitTypes
import Prover
import XGitState

-- ----------------------------------------------------------------------  
-- 
-- Simple object operations: drag & drop and the pop-up menu
--
-- ----------------------------------------------------------------------  

-- Resolve drag&drop - also ensures no argument is outdated.
resolveOpn :: ([ObjType]-> IO[a])-> [Obj]-> IO (Maybe a)
resolveOpn get args = 
  if all (not . isOutdated) args then 
        do ops <- get (map typ args)
           case ops of 
               [] -> return Nothing
               opn:_ -> return (Just opn)                           
     else throwDyn (ExnInfo "One or more arguments are out of date.")


-- drop items on a single item
droppedOn :: Item Obj-> [Item Obj]-> IO ()
droppedOn it drops =
  let args= map content (it:drops)
  in  do opn <- resolveOpn getOpns args
         case opn of Just op -> singleOpn op args
                     Nothing -> status "No operation for that gesture."      
                        
-- execute a single operation (with args)
singleOpn :: Opn-> [Obj]-> IO ()
singleOpn (Opn{opcmd= getCmdNm, target= ty}) args =
  do let (cmd, nm)= getCmdNm args
     nuv<- evalCmd cmd
     newDerivedObj nuv cmd (UserT ty) nm args
singleOpn (GoalOpn{thy= thy, goal= goal}) args =
  do let gc= GoalCmd thy goal
     nuv<- evalCmd gc
     sid<- getPrfState
     case sid of
       Just s-> newConstrObj nuv gc s args "New Proof"
       Nothing-> panic "No PrfStateId after Goal"

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
  case typ ob of
    UserT _ -> 
      if isOutdated ob then
        return (always done)
        else do ops <- getOpns [typ ob]
	        if not (null ops) then 
                   do createMenuSeparator menu [] 
                      menuevents <- 
		        mapM (\opn-> do b<- createMenuCommand menu 
                                                          [text (opname opn)]
                                        c<- clicked b 
                                        return (c >>> singleOpn opn [ob])) ops
                      return (choose menuevents)
                   else return (always done)
    TextT -> return (always done)  -- edit text
    ProofT -> return (always done) -- open in con area !
                                   -- export proof? 
    SubstT -> return (always done) -- edit substitution
    
 

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


