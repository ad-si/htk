{-

MODULE        : Debug
AUTHOR        : George Russell
		University of Bremen
DATE          : 1999
DESCRIPTION   : This module provides a uniform interface for debugging
                purposes.  In final versions of this module it would
                be best to make the debug function do nothing and
                force it to be inlined.

   ######################################################################### 
-}

module Debug(debug) where
   import IO
   import qualified IOExts(unsafePerformIO)

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
