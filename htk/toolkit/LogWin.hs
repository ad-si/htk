-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module LogWin (

  LogWin(..),
  createLogWin,

  HasFile(..),

  writeLogWin

) where

import HTk
import Core
import ScrollBox
import FileDialog
import System

                
-- -----------------------------------------------------------------------
-- Type
-- -----------------------------------------------------------------------

data LogWin = LogWin Toplevel Editor (IO ())


-- -----------------------------------------------------------------------
-- Commands
-- -----------------------------------------------------------------------

createLogWin :: [Config Toplevel] -> IO LogWin
createLogWin cnf =
  do
    win <- createToplevel cnf
    b <- newVFBox win [relief Groove, borderwidth (cm 0.05) ]
    pack b []
    mb <- createMenu win False []
    filecasc <- createMenuCascade mb [text "File"]
    mfile <- createMenu win False []
    filecasc # menu mfile
    savecmd <- createMenuCommand mfile [text "Save"]
    clickedsavecmd <- clicked savecmd
    quitcmd <- createMenuCommand mfile [text "Quit"]
    clickedquitcmd <- clicked quitcmd
    win # menu mb
    (sb, ed)  <- newScrollBox b (\par -> newEditor par [bg "white"]) []
    pack sb []
    death <- newChannel
    let listen :: Event ()
        listen = (clickedsavecmd >> always (saveLog ed) >> listen) +>
                 (clickedquitcmd >>> destroy win) +>
                 receive death
    spawnEvent listen
    return (LogWin win ed (syncNoWait (send death ())))


saveLog :: Editor -> IO ()
saveLog ed =
  do
    homedir <- getEnv "HOME"
    selev <- fileDialog "Open file" homedir
    file  <- sync selev
    case file of
      Just fp -> try (writeTextToFile ed fp) >> done
      _ -> done


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------


-- -----------------------------------------------------------------------
-- Events
-- -----------------------------------------------------------------------

--type LogAction = Top -> Editor -> IO ()

                
-- -----------------------------------------------------------------------
--  Log Menu (User Dialog)
-- -----------------------------------------------------------------------

{-
newMenuBar :: Box -> IO (MenuButton LogAction)
newMenuBar b = do {
        b2 <- newHBox [relief Raised, 
                borderwidth (cm 0.05), 
                bg "grey",
                fill Horizontal,
                parent b];
        mb <- newMenuButton [text "File",bg "grey",parent b2];
        mn <- newPulldownMenu mb [tearOff On];
        newButton [text "Save ...", handler saveLog,parent mn];
        newButton [text "Quit", handler quitLog,parent mn];
        return mb
} where handler c = command (\() -> return c)
        quitLog :: LogAction
        quitLog win _ = destroy win

        saveLog :: LogAction
        saveLog win ed = do
                pwin <- newPromptWin "Enter File Name" "" [modal True]
                forkDialog pwin (\fnm -> incase fnm (writeTextToFile ed))
-}
                
-- -----------------------------------------------------------------------
-- LogFile
-- -----------------------------------------------------------------------

instance GUIObject LogWin where
  toGUIObject (LogWin win _ _) = toGUIObject win
  cname _ = "LogWin"

instance Destroyable LogWin where
  destroy (LogWin win _ death) = death >> destroy win


-- -----------------------------------------------------------------------
-- Write Log
-- -----------------------------------------------------------------------

writeLogWin :: LogWin -> String -> IO ()
writeLogWin (LogWin _ ed _) str =
  do
    try (insertText ed EndOfText str)
    moveto Vertical ed 1.0
    done
