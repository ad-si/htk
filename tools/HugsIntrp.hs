{- #########################################################################

MODULE        : HugsIntrp
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : alpha
DESCRIPTION   : An encapsulation of Hugs as a simple background tool.


   ######################################################################### -}

module HugsIntrp (
        HugsIntrp(..),
        newHugsIntrp,
        hugsFailed

        ) where

import WB
import Expect
import Debug(debug)



-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data HugsIntrp = HugsIntrp Expect Mutex


-- --------------------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------------------

newHugsIntrp :: FilePath -> [Config PosixProcess] -> IO HugsIntrp
newHugsIntrp fname confs = do 
   exp <- newExpect fname confs
   mtx <- newMutex
   matchUntilPrompt exp
   return (HugsIntrp exp mtx)


matchUntilPrompt :: Expect -> IO Bool
matchUntilPrompt exp = do 
   es <- newEventStream
   become (es::EventStream Bool) (
           matchLine exp     >>> return True
        +> match exp "^.*> " >>> do {become es (inaction::IA Bool); return False}
        +> destroyed exp     >>> do {become es (inaction::IA Bool); raise hugsFailed}
       )
   while (receiveIO es) id

                
-- --------------------------------------------------------------------------
--  Instance Declarations
-- --------------------------------------------------------------------------

instance Object HugsIntrp where
   objectID (HugsIntrp exp _) = objectID exp

instance CommandTool HugsIntrp where
   evalCmd cmd (HugsIntrp exp mtx) = 
        synchronize mtx (exp # evalCmd cmd)
   execOneWayCmd cmd (HugsIntrp exp mtx) = 
        synchronize mtx (exp # execOneWayCmd cmd)

instance Destructible HugsIntrp where
   destroy (HugsIntrp exp _)   = destroy exp
   destroyed (HugsIntrp exp _) = destroyed exp

instance Tool HugsIntrp where
   getToolStatus (HugsIntrp exp _) = getToolStatus exp

instance UnixTool HugsIntrp where 
   getUnixProcessID (HugsIntrp exp _) = getUnixProcessID exp

instance Synchronized HugsIntrp where
        synchronize (HugsIntrp _ mtx) = synchronize mtx

-- --------------------------------------------------------------------------
-- Errors
-- --------------------------------------------------------------------------

hugsFailed :: IOError 
hugsFailed = toolFailed "hugs"


