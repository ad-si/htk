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
  newId, -- get a new unique integer from a global variable
  
  ) where
import IO
import qualified IOExts(unsafePerformIO)
import qualified Concurrent
import Dynamic

#ifdef DEBUG
debug :: Show a => a -> IO()
debug s = 
   case debugFile of 
     Just f  -> IO.hPutStrLn f (show s)>> IO.hFlush f
     Nothing -> return ()

debugFile :: Maybe Handle
debugFile = 
   IOExts.unsafePerformIO 
     (catch (openFile "/tmp/uniform.DEBUG" WriteMode >>= return . Just)
            (\_-> return Nothing))
#else

debug :: Show a => a -> IO()
debug _ = return ()

{-# inline debug #-}

#endif

newId :: IO Int
newId =
   do
      next <- Concurrent.takeMVar idSource
      Concurrent.putMVar idSource (next+1)
      return next

idSource :: Concurrent.MVar Int
idSource = IOExts.unsafePerformIO(Concurrent.newMVar 0)

