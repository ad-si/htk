{- #########################################################################

MODULE        : Diff
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : diff encapsulation. 


   ######################################################################### -}


module Diff (
   Object(..),
   Tool(..),
   
   FilePath,
   DiffSourceObj(..),
   Diff
   
   ) where

import Debug(debug)

import Expect

import DialogWin
import LogWin

import WB

-- --------------------------------------------------------------------------
--  Classes
-- --------------------------------------------------------------------------

class DiffSourceObj o1 o2 where
   newDiff :: o1 -> o2 -> [Config PosixProcess] -> IO Diff

-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Diff = Diff Expect LogWin
                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

instance DiffSourceObj FilePath FilePath where
   newDiff fname1 fname2 confs = 
      do    
         difffp <- getWBToolFilePath "diff"
         exp <- newExpect difffp confs'
         win <- newLogWin [text "diff"]
         let dtool = Diff exp win
         interactor (\iact ->
               matchLine exp >>>= 
                  (\ line ->
                     writeLogWin win (line ++ "\n")
                     )
            +> matchEOF exp  >>> stop iact
               )
         return dtool
      where 
         confs' = confs ++ [arguments ["-C","5",fname1,fname2]]



-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Diff where
   objectID (Diff tool _) = objectID tool

instance Tool Diff where
   getToolStatus (Diff tool _) = getToolStatus tool

instance UnixTool Diff where
   getUnixProcessID (Diff tool _) = getUnixProcessID tool

instance Destructible Diff where
   destroy (Diff tool win) =  
      do
         destroy tool
         try(destroy win)
         done
   destroyed (Diff tool win) = destroyed win




