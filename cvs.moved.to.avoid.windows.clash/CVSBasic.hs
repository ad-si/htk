{-# This module encapsulates calling CVS.  Thus it wraps up the
    arguments nicely; it does not try to do anything with the output.
    It returns an Expect object corresponding to the called 
    CVS command.
    #-}
module CVSBasic(
   callCVS, -- :: GlobalOptions -> CVSCommand -> IO Expect

   CVSCommand(..),
   GlobalOptions(..), 
      -- things common to a sequence of CVS commands,
      -- such as the working directory and the repository.
   CVSFile(..), -- alias for file.  A newtype for String
   CVSVersion(..) -- alias for version.  Ditto.
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

newtype CVSFile = CVSFile String deriving Show

newtype CVSVersion = CVSVersion String deriving Show
 
data CVSCommand =
      UpdateSimple {
         revision :: CVSVersion,
         files :: [CVSFile]
         }
   |  CommitSimple {
         revision' :: Maybe CVSVersion,
         files :: [CVSFile]
         } -- we set a log message "X"
   |  LogSimple {
         file :: CVSFile
         }
   |  Add { -- this needs to be done for all new files.
         file :: CVSFile
         }

compileCVSCommand :: CVSCommand -> [Config PosixProcess]
compileCVSCommand command =
      [appendArguments (compile command)]
   where
      compile :: CVSCommand -> [String]
      compile(UpdateSimple{revision=CVSVersion version,files=files}) = [
         "update",
         "-r",
         version
         ] ++ castFiles files
      compile(CommitSimple{revision'=revision',files=files}) = [
         "commit", 
         "-m",
         "X"
         ] ++ 
         (case revision' of
            Nothing -> []
            Just(CVSVersion version) -> [
               "-r",
               version
               ]
            ) ++ (castFiles files)
      compile(LogSimple{file=CVSFile file}) = [
         "log",
         file
         ]
      compile(Add{file=CVSFile file}) = [
         "add",
         file
         ]

      castFiles :: [CVSFile] -> [String]
      castFiles files =
         map (\ (CVSFile file) -> file) files

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
         
         

