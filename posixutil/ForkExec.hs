{- The purpose of this module is to interface to the C program forkExec.c. 

   The function is called by ChildProcess.newChildProcess.  For background
   see the "Implementation Note" at the start of the definition of that 
   function.  -}
module ForkExec(
   forkExec
   ) where

import Posix

import CString
import MarshalArray
import Ptr
import CTypes


foreign import ccall unsafe "forkExec.h fork_exec" innerForkExec
   :: CString -> Ptr CString -> Ptr CString  -> Ptr Int -> IO ProcessID
-- Even if GHC should in future allow "threadsafe" 
-- DO NOT use it here.  See the "Implementation Note" in 
-- ChildProcess.newChildProcess to see why.

forkExec :: String -> [String] -> Maybe [(String,String)]
   -> Fd -> Fd -> Fd  
   -> IO (Maybe ProcessID)
forkExec path args envOpt stdInFd stdOutFd stdErrFd =
   do
      processId <- withCString path (\ pathCString ->
         withCStrings0 (path:args) (\ argPtr ->
            withEnv envOpt (\ envPtr ->
               withArray (map fdToInt [stdInFd,stdOutFd,stdErrFd])
                     (\ fdsArr ->
                  innerForkExec pathCString argPtr envPtr fdsArr
                  )
               )
            )
         )
      return (if processId == 0 then Nothing else Just processId)

---
-- Marshall a number of Strings into an array of CStrings terminated by
-- a null pointer
withCStrings0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStrings0 strings apply = withCStrings0Acc strings apply []
   where
      withCStrings0Acc (first : rest) apply acc =
         withCString first (\ firstCString ->
            withCStrings0Acc rest apply (firstCString : acc))
      withCStrings0Acc [] apply acc =
         withArray0 nullPtr (reverse acc) apply

---
-- Marshall the environment
withEnv :: Maybe [(String,String)] -> (Ptr CString -> IO a) -> IO a
withEnv Nothing apply = apply nullPtr
withEnv (Just env) apply =
   let
      strs = map (\ (name,setting) -> name ++ ('=':setting)) env
   in
      withCStrings0 strs apply

