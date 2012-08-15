{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |
-- MODULE        : Debug
-- AUTHOR        : George Russell
-- University of Bremen
-- DATE          : 2000
-- DESCRIPTION   : This module provides a uniform interface for debugging
--              purposes.  In final versions of this module it would
--              be best to make the debug function do nothing and
--              force it to be inlined.
--
-- #########################################################################

module Util.Debug(
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

  debugString, -- Send a string to the debug file.  This differs from
     -- debug, in that debug will Haskell-escape the string and add
     -- a newline, while just writes to the file with no interpretation.
  (@@:),


  wrapError, -- :: String -> a -> a
     -- If debugging is on, transforms value so that when evaluated, if
     -- the evaluation calls an error call, the given String is prepended
     -- to the evaluation.
  ) where
import System.IO as IO

import System.IO.Unsafe
import Control.Exception as Exception

import Util.WBFiles

openDebugFile :: IO (Maybe Handle)
openDebugFile =
   do
      debugFileName <- getDebugFileName
      Exception.catch (
         do
             handle <- openFile debugFileName WriteMode
             hSetBuffering handle NoBuffering
             return (Just handle)
         )
         (\ (_ :: IOException) -> return Nothing)

debugFile = unsafePerformIO openDebugFile
debugFile :: Maybe Handle
{-# NOINLINE debugFile #-}

#ifdef DEBUG
debugString s =
               case debugFile of
                  Just f -> IO.hPutStr f s
                  Nothing -> return ()

debug s =
               case debugFile of
                  Just f  -> IO.hPutStrLn f (show s)
                  Nothing -> return ()

debugAct mess act =
               do
                  res <- Exception.try act
                  case res of
                     Left error ->
                        do
                           debug ("Debug.debug caught "++mess)
                           throw (error :: SomeException)
                     Right success -> return success

#else
debugString _ = return ()

debug _ = return ()
{-# inline debug #-}

debugAct _ act = act
{-# inline debugAct #-}
#endif

-- | show something to log file if debugging is turned on.
debug :: Show a => a -> IO()

-- | Send a string to the debug file.  This differs from
-- debug, in that debug will Haskell-escape the string and add
-- a newline, while just writes to the file with no interpretation.
debugString :: String -> IO ()

-- | If an action fails print out a message before
-- propagating message.
debugAct :: String -> IO a -> IO a

(@:) :: String -> IO a -> IO a
(@:) = debugAct


-- | always show something to the log file
alwaysDebug :: Show a => a -> IO()
alwaysDebug s =
   case debugFile of
      Just f  -> IO.hPutStrLn f (show s)
      Nothing -> return ()

-- | always print out a message if action fails.
alwaysDebugAct :: String -> IO a -> IO a
alwaysDebugAct mess act =
   do
      res <- Exception.try act
      case res of
         Left error ->
            do
               alwaysDebug ("AlwaysDebug.debug caught "++mess)
               throw (error :: SomeException)
         Right success -> return success

(@@:) :: String -> IO a -> IO a
(@@:) = alwaysDebugAct

wrapError :: String -> a -> a
#ifdef DEBUG
wrapError str value = unsafePerformIO (wrapErrorIO str value)
#else
wrapError str value = value
#endif

wrapErrorIO :: String -> a -> IO a
wrapErrorIO str value =
   Exception.catch (value `seq` return value)
      (\ mess -> error (str ++ ":"++ show (mess :: ErrorCall)))


