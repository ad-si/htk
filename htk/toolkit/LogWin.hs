
-- | A simple log window.
module LogWin (

  LogWin(..),
  createLogWin,

  HasFile(..),

  writeLogWin

) where

import System.IO.Unsafe
import ReferenceVariables
import HTk
import Core
import ScrollBox
import FileDialog
import System

                
-- -----------------------------------------------------------------------
-- Type
-- -----------------------------------------------------------------------

-- | The @LogWin@ datatype.
data LogWin = LogWin Toplevel Editor (IO ())


-- -----------------------------------------------------------------------
-- Commands
-- -----------------------------------------------------------------------

-- | Creates a new log window and returns a handler.
createLogWin :: [Config Toplevel] 
   -- ^ the list of configuration options for this log window.
   -> IO LogWin
   -- ^ A log window.
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


pathRef :: Ref String
pathRef = unsafePerformIO $ getEnv "HOME" >>= newRef
{-# NOINLINE pathRef #-}

saveLog :: Editor -> IO ()
saveLog ed =
  do selev <- fileDialog "Open file" pathRef
     file  <- sync selev
     case file of
       Just fp -> try (writeTextToFile ed fp) >> done
       _ -> done


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject LogWin where
  -- | Internal.
  toGUIObject (LogWin win _ _) = toGUIObject win
  -- | Internal.
  cname _ = "LogWin"

-- | A log window can be destroyed.
instance Destroyable LogWin where
  -- | Destroys a log window.
  destroy (LogWin win _ death) = death >> destroy win


-- -----------------------------------------------------------------------
-- Write Log
-- -----------------------------------------------------------------------

-- | Writes into the log window.
writeLogWin :: LogWin 
   -- ^ the concerned log window.
   -> String 
   -- ^ the text to write to the log window.
   -> IO ()
   -- ^ None.
writeLogWin lw@(LogWin _ ed _) str =
  do
    try (insertText ed EndOfText str)
    moveto Vertical ed 1.0
    done
