{-

MODULE        : Debug
AUTHOR        : George Russell
		University of Bremen
DATE          : 2000
DESCRIPTION   : This module provides a uniform interface for debugging
                purposes.  In final versions of this module it would
                be best to make the debug function do nothing and
                force it to be inlined.

   ######################################################################### 
-}

module Debug(
  debug, -- show something to log file if debugging is turned on.

  debugAct, 
  -- If an action fails print out a message before
  -- propagating message.  
  (@:),
  -- inline version of debugAct

  -- The following functions work whether debugging is turned on or
  -- not, and are intended to be used when the debugging facility
  -- itself is causing strange effects . . .
  alwaysDebug,
  alwaysDebugAct,
  (@@:),
  ) where
import IO
import qualified IOExts(unsafePerformIO)
import qualified Concurrent
import Dynamic
import Exception

import WBFiles

openDebugFile :: IO (Maybe Handle)
openDebugFile =
   do
      debugFileName <- getDebugFileName
      IO.catch 
         (openFile debugFileName WriteMode >>= return . Just)
         (\ _-> return Nothing)

debugFile = IOExts.unsafePerformIO openDebugFile
{-# NOINLINE debugFile #-} 

#ifdef DEBUG
debug :: Show a => a -> IO()
debug s = 
   case debugFile of 
     Just f  -> IO.hPutStrLn f (show s)>> IO.hFlush f
     Nothing -> return ()

debugAct :: String -> IO a -> IO a
debugAct mess act =
   do
      res <- tryAllIO act
      case res of
         Left error ->
            do
               debug ("Debug.debug caught "++mess)
               throw error
         Right success -> return success

      
#else

debug :: Show a => a -> IO()
debug _ = return ()

debugAct :: String -> IO a -> IO a
debugAct _ act = act

{-# inline debug #-}
{-# inline debugAct #-}

#endif

(@:) = debugAct

alwaysDebug :: Show a => a -> IO()
alwaysDebug s = 
   case debugFile of 
     Just f  -> IO.hPutStrLn f (show s)>> IO.hFlush f
     Nothing -> return ()

alwaysDebugAct :: String -> IO a -> IO a
alwaysDebugAct mess act =
   do
      res <- tryAllIO act
      case res of
         Left error ->
            do
               alwaysDebug ("AlwaysDebug.debug caught "++mess)
               throw error
         Right success -> return success

(@@:) = alwaysDebugAct



      
