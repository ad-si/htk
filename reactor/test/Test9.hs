module Main (
        main 
        ) 
where

import qualified IOExts(performGC)

import SIM
import Expect
import Debug(debug)

main :: IO ()
main =  do {
        newHugs;

        h <- newHugs;

        a <- h # evalCmd "[1..100]\n";
        print (map succ (read a::[Int]));

        IOExts.performGC;

        newCollectibleObj;


        IOExts.performGC;

        delay(secs 1.0);
        print "shutting down";
        shutdown;
        print "done";

        }


data Hugs = Hugs Expect BSem


newHugs :: IO Hugs
newHugs = do 
   exp <- newExpect "hugs" []
   mtx <- newBSem
   while ( sync (
           matchLine exp     >>> return True
        +> match exp "^Prelude> " >>> return False
        +> destroyed exp     >>> raise (toolFailed "hugs")
        )) id
   return (Hugs exp mtx)

{-
newHugs :: IO Hugs
newHugs = do 
   exp <- newExpect "hugs" []
   mtx <- newLockedBSem
   cobj <- newCollectibleObj
   cobj # destructor (print "destroying hugs" >> exp # destroy)
   interactor (\iact -> 
           matchLine exp     >>> done
        +> match exp "^.*> " >>> do {release mtx; stop iact}
        +> destroyed exp     >>> do {release mtx; raise (toolFailed "hugs")}
        )
   return (Hugs exp mtx cobj)

-}

{-

data Hugs = Hugs Expect Mutex CollectableObj

newHugs :: IO Hugs
newHugs = do 
   exp <- newExpect "hugs" []
   mtx <- newMutex
   cobj <- newCollectibleObj
   cobj # destructor (print "destroying hugs" >> exp # destroy)
   es <- newEventStream;
   interaction es (
           matchLine exp     >>> return True
        +> match exp "^.*> " >>> do {interaction es inaction; return False}
        +  destroyed exp     >>> do {interaction es inaction; raise (toolFailed "hugs")
        )
   while (receiveIO es) id;
   return (Hugs exp mtx cobj)
   } 

-}

instance CommandTool Hugs where
   evalCmd cmd (Hugs exp mtx) = synchronize mtx (do {
      exp # execCmd cmd;
      r <- sync(
               matchLine exp 
            +> matchEOF exp >>> raise (toolFailed "Hugs")
            );
      sync (match exp "^.*> ");
      return r
      })
   execOneWayCmd = execCmd

instance Destructible Hugs where
   destroy (Hugs exp _)   = destroy exp
   destroyed (Hugs exp _) = destroyed exp

instance Tool Hugs where
   getToolStatus (Hugs exp _) = getToolStatus exp

instance UnixTool Hugs where 
   getUnixProcessID (Hugs exp _) = getUnixProcessID exp

