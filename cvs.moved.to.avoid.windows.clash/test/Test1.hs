{- This test file tests the functions in CVSHigh.hs.
   CVSROOT should be set to the repository location;
   the current directory should contain the CVS files
   in question.  The program is interactive and accepts
   lines of the form

add [File]
update [File] [Version]
commit [File]
commit [File] [Version]
checkout [File]
list [File]
    It outputs the response (just using show).
    -}
module Main(main) where

import System
import Directory

-- importing AtomString and CVSTypes shouldn't really be necessary, but
-- seem to be necessary to persuade GHC 4.08 that CVSVersion is an instance
-- of Show.
import AtomString 

import CVSTypes
import CVSHigh

main :: IO ()
main =
   do
      directory <- getCurrentDirectory
      repository <- getEnv "CVSROOT"
      cvsLoc <- newCVSLoc repository directory
      commandLoop cvsLoc

showOut :: Show a => IO a -> IO ()
showOut action =
   do
      result <- action
      putStr ((show result) ++ "\n")
      
commandLoop :: CVSLoc -> IO ()
commandLoop loc = 
   do
      command <- getLine
      case words command of
         ["add",file] -> showOut (cvsAdd loc (CVSFile file))
         ["commit",file,version] -> 
            showOut (cvsCommit loc (CVSFile file) (Just(CVSVersion version)))
         ["commit",file] ->
            showOut (cvsCommit loc (CVSFile file) Nothing)
         ["update",file,version] ->
            showOut (cvsUpdate loc (CVSFile file) (CVSVersion version))
         ["list",file] -> showOut(cvsListVersions loc (CVSFile file))
         ["checkout",file] -> showOut(cvsCheckout loc (CVSFile file))
         _ -> putStr("Couldn't parse: "++command++"\n")
      commandLoop loc
