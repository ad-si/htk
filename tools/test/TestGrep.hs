{- #########################################################################

MODULE        : Main
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Main (
        main    
        ) where


import WB
import SIM
import Grep
import System
import Char
import Debug(debug)

main = 
   do
      args <- getArgs
      fname <- filename args
      t <- grep "^import" fname []
      inter <- newInterActor (\iact ->
         receiveNextLine t |>>= 
            (\mstr -> do
               case mstr of 
                  Nothing -> 
                     do 
                        putStr "\n"
                        stop iact
                  (Just str) -> putStr ((modname str) ++ " ")
               )           
        )
      sync(destroyed inter)
   where 
      filename [fname] = return fname
      filename _  = 
         do
            putStr "impchase: argument must be a file name\n"
            try(shutdown)
            exitWith (ExitFailure 1) 
      modname = (takeWhile isAlphaNum) . (dropWhile isSpace) . (drop 6)
