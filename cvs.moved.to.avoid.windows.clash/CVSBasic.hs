{-# This module encapsulates calling CVS.  Thus it wraps up the
    arguments nicely; it does not try to do anything with the output.
    It returns an Expect object corresponding to the called 
    CVS command.
    #-}
module CVSBasic(
   callCVS, -- :: GlobalOptions -> CVSCommand -> IO Expect

   GlobalOptions(..),
   
   ) where

import WBFiles
import Computation

import ChildProcess
import Expect

data GlobalOptions =
   GlobalOptionsSimple {
      workingDir :: String,
      cvsRoot :: String
      } -- this also includes the "-f" option, meaning don't read .cvsrc
        -- and "-q" making CVS "somewhat quiet".  

compileGlobalOptions :: GlobalOptions -> [Config PosixProcess]
compileGlobalOptions(
      GlobalOptionsSimple{workingDir=workingDir,cvsRoot=cvsRoot}) = [
   workingdir workingDir,
   arguments [
      "-fqd",
      cvsRoot   
      ]
   ]
 
data CVSCommand =
      UpdateSimple {
         revision :: String,
         files :: [String]
         }
   |  CommitSimple {
         revision' :: Maybe String,
         files :: [String]
         } -- we set a log message "X"
   |  LogSimple {
         file :: String
         }

compileCVSCommand :: CVSCommand -> [Config PosixProcess]
compileCVSCommand command =
      [appendArguments (compile command)]
   where
      compile :: CVSCommand -> [String]
      compile(UpdateSimple{revision=revision,files=files}) = [
         "update",
         "-r",
         revision
         ] ++ files
      compile(CommitSimple{revision'=revision',files=files}) = [
         "commit", 
         "-m",
         "X"
         ] ++ 
         (case revision' of
            Nothing -> []
            Just revision -> [
               "-r",
               revision
               ]
            ) ++ files
      compile(LogSimple{file=file}) = [
         "log",
         file
         ]

callCVS :: GlobalOptions -> CVSCommand -> IO Expect
callCVS globalOptions command =
   do
      cvsFilePath <- getWBToolFilePath "cvs"
      expect <-
         newExpect
            cvsFilePath
            (  compileGlobalOptions globalOptions ++
               compileCVSCommand command
               )
      return expect 
         
         
