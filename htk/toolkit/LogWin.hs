{- #########################################################################

MODULE        : LogWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic log window.


   ######################################################################### -}


module LogWin (
        LogWin(..),
        newLogWin,

        GUIObject(..),
        HasFile(..),

        writeLogWin

        ) where

import Concurrency
import HTk
import Separator
import Button
import PulldownMenu
import Space
import Editor
import ScrollBar
import PromptWin
import DialogWin
import ScrollBox
import Interaction()
import Debug(debug)

                
-- --------------------------------------------------------------------------
--  Type
-- --------------------------------------------------------------------------

data LogWin = LogWin Window (Editor String) 

                
-- --------------------------------------------------------------------------
--  Commands (Layout)
-- --------------------------------------------------------------------------

newLogWin :: [Config Window] -> IO LogWin
newLogWin confs = do
        b <- newVBox [flexible,relief Groove, borderwidth (cm 0.05) ]
        mb <- newMenuBar b
        ed <- newEditor [flexible,bg "white"]
        sb <- newScrollBox ed [flexible,parent b]
        win <- window b confs
        mbe <- getTrigger mb
        controller win (const (logEvents win ed mbe))
        return (LogWin win ed)
 where  logEvents win ed mbe = mbe >>>= \c -> c win ed 



                
-- --------------------------------------------------------------------------
--  Events
-- --------------------------------------------------------------------------

type LogAction = Window -> Editor String -> IO ()

                
-- --------------------------------------------------------------------------
--  Log Menu (User Dialog)
-- --------------------------------------------------------------------------

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

                
-- --------------------------------------------------------------------------
--  LogFile
-- --------------------------------------------------------------------------

instance GUIObject LogWin where
        toGUIObject (LogWin win _) = toGUIObject win
        cname _ = "LogWin"

instance Destructible LogWin where
        destroy (LogWin win _) = destroy win
        destroyed (LogWin win _) = destroyed win
                
-- --------------------------------------------------------------------------
--  Write Log
-- --------------------------------------------------------------------------

writeLogWin :: LogWin -> String -> IO ()
writeLogWin (LogWin _ ed) str = do {
        try (insertText ed EndOfText str);
        moveto Vertical ed 1.0;
        done
}


