module Main (
   main 
   ) where

import qualified IOExts(performGC)

import Object
import SIM
import Expect
import RegularExpression
import Debug(debug)

main :: IO ()
main =  
   do
      hugs <- newHugs
      answer <- evalCmd "[1..3000]\n" hugs
      let resultList = (read answer) :: [Int]
      putStr (show resultList) 
   
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
            match exp ("\\`(.*\n)*Prelude> \\'",1::Int) >>> return False
         +> match exp ""      >>> return True
         +> destroyed exp     >>> raise (toolFailed "hugs")
         )) id
      let hugs = Hugs exp mtx
      registerTool hugs
      return hugs

instance CommandTool Hugs where
   evalCmd cmd (Hugs exp mtx) = synchronize mtx (
      do
         execCmd cmd exp
         result <- readWholeLine exp  ( \ _ -> raise (toolFailed "hugs"))
         return result
         )
   execOneWayCmd = execCmd

instance Object Hugs where
   objectID (Hugs exp _) = objectID exp

instance Destructible Hugs where
   destroy (Hugs exp _)   = destroy exp
   destroyed (Hugs exp _) = destroyed exp

instance Tool Hugs where
   getToolStatus (Hugs exp _) = getToolStatus exp

instance UnixTool Hugs where 
   getUnixProcessID (Hugs exp _) = getUnixProcessID exp

