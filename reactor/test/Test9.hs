module Main (
   main 
   ) where

import qualified IOExts(performGC)

import SIM
import Expect
import Debug(debug)

main :: IO ()
main =  
   do
      hugs <- newHugs
      a <- evalCmd "[1..100]\n" hugs
      debug (map succ (read a::[Int]))
   
      IOExts.performGC
      newCollectibleObj     
      IOExts.performGC
      
      delay(secs 1.0)
      debug "shutting down"
      shutdown
      debug "done"      
      


data Hugs = Hugs Expect BSem

newHugs :: IO Hugs
newHugs = 
   do 
      exp <- newExpect "hugs" [linemode False]
      mtx <- newBSem
      while ( sync (
            match exp ("(.*\n)*Prelude>",1::Int) >>> return False
         +> match exp ".*\n" >>> return True
         +> match exp ".*" >>> return True
         +> destroyed exp     >>> raise (toolFailed "hugs")
         )) id
      return (Hugs exp mtx)

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

