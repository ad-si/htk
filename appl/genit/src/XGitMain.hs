{- -----------------------------------------------------------------------
 -
 - XGit - the extended generic interface toolkit
 -
 - $Source$
 - $Revision$ of $Date$ 
 - Author: cxl (Last modification: $Author$)
 -
 - Main module: starting and event handling.
 -}

module XGitMain where

import Exception hiding (catch)
import IOExts (unsafePerformIO, readIORef, writeIORef, newIORef)
import IO (ioeGetErrorString)
import FiniteMap
import qualified System (getEnv)

import Dynamics

import GenGUI
import ReferenceVariables
import HTk

import DialogWin
import FileDialog

import GenGUI

-- import qualified PGIP 
import PGIP (ProverCmd(..), PrfStateId)
import XGitTypes
import Prover

import XGitState
import XGitSimpleOps
import XGitConArea


-- Start XGit, in a given state. How to provide an initial empty state
-- is, for the moment, left as an exercise to the avid reader. 
-- (Hint: use empty lists!)
start    :: HTk-> XState-> IO ()
start main (XState{objs= obs, constrArea= ca, histIns= hi}) =
  do -- Starting XGit proceeds in three stages: 
     -- First, start the prover (so it can launch while the GUI is coming up)
     -- Second, start GenGUI and the rest of the GUI, and set up the
     -- threads handling all the events from the interface. 
     -- Third, reconstruct the XGit state. This may take while.

     -- Start prover
     catch (Prover.start "") {- ARGH! -} handleExn -- We should abort here, really.
 
     gee<- newGenGUI Nothing -- TBD: reimport GenGUI state here as well! 

     (ggev, _) <- bindGenGUIEv gee

     -- create the main menu
     -- (This should go into a separate function, but then we'd need
     --  return all the "menu-clicked" events; this way, we can
     --  combine them below into events as by the threads which handle
     --  thum.)

     let geemen = genGUIMainMenu gee

     -- the "File" menu
     m <- createMenu main False [] 
     fileMenu <- createMenuCascade geemen [menu m, text "File "]
     loadev <- createMenuCommand m [text "Load File"] >>= clicked
     createMenuSeparator m []
     savev  <- createMenuCommand m [text "Save"] >>= clicked
     quitev <- createMenuCommand m [text "Save & Quit"] >>= clicked
     abrtev <- createMenuCommand m [text "Abort"] >>= clicked
{-     
     -- the "Edit" menu
     m <- createMenu main False [] -- this is a new "m"!
     editMenu <- createMenuCascade geemen [menu m, text "Edit "]

     -- more to come
 -}    

     -- Now set up the threads: 
     --  one "main" thread handling GenGUI events and most menu events
     --  one thread handling the "interrupt prover" button
     --  one thread for the "load file"
     --  one thread for "help" (to come :-)
     -- We set up the threads here, so during state reconstruction 
     -- we have the interrupt button at our disposal
     handle (ggev >>>= handleGG geemen
             +> savev >>> done -- save whereto?
             +> quitev >>> done
             +> abrtev >>> doAbort)
     handle (loadev >>> handleLoad)
     -- handle (intev >>> Prover.sendINT)

     -- Reconstruct the XGit state     
     fm<- catch (addToState obs emptyFM) (\e-> handleExn e >> (return emptyFM))
                  -- This stops when we encounter an error-- not good.
     setRef objState fm
     setRef conArea ca
     setRef historyInsert hi

     bindSimple main Destroy >>= sync . fst -- wake me up when everything's over
  where

  -- spawn a thread, repeatedly sync on event, handle exceptions
  handle :: Event ()-> IO ()
  handle ev = spawn (forever (catch (sync ev) handleExn)) >> done

  -- handle GenGUI events                           
  handleGG :: Widget w=> w-> GenGUIEvent Obj-> IO ()
  handleGG _ (Dropped (trg, drp)) = droppedOn trg drp
  handleGG _ (Doubleclick it)     = openInConArea it
  handleGG _ (Rightclick [])      = done -- shouldnae happen
  handleGG w (Rightclick (it:_))  = popupMenu w (100, 100) it
--  handleGG (DroppedInArea drp)  = droppedInConArea drp
  handleGG _ _                    = done
  
  -- handle the load event by bringing up the file dialog
  handleLoad :: IO ()
  handleLoad = 
    do dir   <- System.getEnv "HOME" -- very temp. 
                                     -- Better: remember opened directory.
       selev <- fileDialog "Open file" dir
       file  <- sync selev
       case file of Just fp -> status ("Selected " ++ fp) 
                    _ -> status ("File selection cancelled.")
     
  doAbort :: IO () 
  -- pretty drastic. We should stop the prover gracefully, and
  -- wait for it to terminate. 
  doAbort = confirm "All your love and labour will be lost if you abort now.\nAre you sure you want that?" (destroy main)  
  

  
-- The exception handler.
handleExn :: IOError -> IO ()
handleExn (DynException d)= 
  case (fromDyn d) of
       Just x -> handleXGitExn x
       Nothing -> panic ("Unrecognised dynamic exception: "++ show d)
  where
    handleXGitExn :: XGitExn-> IO ()
    handleXGitExn (ExnError m)  = newErrorWin m [] 
    handleXGitExn (ExnWarning m)= newWarningWin m []
    handleXGitExn (ExnInfo m)   = newAlertWin m []
    handleXGitExn (ExnStatus s) = status s
handleExn (IOException ioe)   = 
  newErrorWin ("I/O Error: "++ show ioe) []
handleExn (AssertionFailed a) = panic ("Assertion failed: "++ a)
handleExn other = panic ("Unexpected exception: "++ show other)
                  -- or just drop them silently? 
 

confirm :: String-> IO ()-> IO ()
confirm str act = 
   do yes <- newConfirmWin str []
      when yes act
