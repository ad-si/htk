{- This module deals with reading and writing a log file containing
   entries which are instances of HasBinaryIO. -}
module LogFile(
   LogFile,
   openLog,
   openLog1,
   writeLog,
   ) where

import IO

import WBFiles
import Computation
import BinaryIO
import IOExtras

newtype LogFile a = LogFile Handle

-- | Reads the entries in a LogFile.
openLog :: HasBinaryIO a => String -> IO (LogFile a,[a])
openLog logFileName =
   do
      (logFile,as) <- openLog1 logFileName
      return (logFile,reverse as)

-- ! Reads the entries in a LogFile, returning them in reverse order.
openLog1 :: HasBinaryIO a => String -> IO (LogFile a,[a])
openLog1 logFileName =
   do
      fpath <- getServerFile logFileName
      handle <- openFile fpath ReadWriteMode
      items <- readLogItems handle []
      return (LogFile handle,items)
   where
      -- The second argument is an accumulating parameter.
      readLogItems :: HasBinaryIO a => Handle -> [a] -> IO [a]
      readLogItems handle as =
         do
            pos1 <- hGetPosn handle
            aWEOpt <- catchEOF (hGetWE handle)
            case aWEOpt of
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
               Just aWE -> 
                  case fromWithError aWE of
                     Left mess ->
                        error (
                           "Server could not restarted due to parse error in "
                           ++ logFileName ++ ": " ++ mess)
                     Right a ->
                        do
                           readLogItems handle (a : as)



writeLog :: HasBinaryIO a => LogFile a -> a -> IO ()
writeLog (LogFile handle) a =
   do
      hPut handle a
      hFlush handle