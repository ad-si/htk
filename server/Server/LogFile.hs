-- | This module deals with reading and writing a log file containing
-- entries which are instances of HasBinary.
module Server.LogFile(
   LogFile,
   openLog,
   openLog1,
   writeLog,
   readLog,
   ) where

import System.IO

import Util.WBFiles
import Util.Computation
import Util.BinaryAll
import Util.IOExtras

newtype LogFile a = LogFile Handle

-- | Reads the entries in a LogFile.
openLog :: HasBinary a IO => String -> IO (LogFile a,[a])
openLog logFileName =
   do
      (logFile,as) <- openLog1 logFileName
      return (logFile,reverse as)

-- ! Reads the entries in a LogFile, returning them in reverse order.
openLog1 :: HasBinary a IO => String -> IO (LogFile a,[a])
openLog1 logFileName =
   do
      fpath <- getServerFile logFileName
      handle <- openFile fpath ReadWriteMode
      items <- readLogItems handle []
      return (LogFile handle,items)

-- | Reads the entries in a LogFile, without opening it for writing.
readLog :: HasBinary a IO => String -> IO [a]
readLog logFileName =
   do
      fpath <- getServerFile logFileName
      handle <- openFile fpath ReadMode
      items <- readLogItems handle []
      hClose handle
      return (reverse items)



-- The second argument is an accumulating parameter.
readLogItems :: HasBinary a IO => Handle -> [a] -> IO [a]
readLogItems handle as =
   do
      pos1 <- hGetPosn handle
      aOpt <- catchEOF (hRead handle)
      case aOpt of
         Nothing -> -- EOF
            do
               pos2 <- hGetPosn handle
               unless (pos1 == pos2)
                  (do
                     putStrLn
                        "Restarting server: incomplete commit discarded"
                     hSetPosn pos1
                  )
               return as -- this is how we normally end.
         Just a -> readLogItems handle (a : as)



writeLog :: HasBinary a IO => LogFile a -> a -> IO ()
writeLog (LogFile handle) a =
   do
      hWrite handle a
      hFlush handle
