module FdRead(
   fdWriteLn, -- :: Posix.Fd -> String -> IO ()
   fdWritePrim, -- :: Posix.Fd -> ByteArray Int -> Int -> IO ()
   -- The last integer is the length.
   fdMkReadLn, -- :: Posix.Fd -> IO FdBuffer
   fdReadLn, -- :: FdBuffer -> IO ()
   FdBuffer,
   ) where

import IO
import ByteArray
import Concurrent
import Posix

import Computation

fdWritePrim :: Posix.Fd -> ByteArray Int -> Int -> IO ()
fdWritePrim fd array len =
   do
      -- copied from source of PosixIO.lhs
      rc  <- _ccall_ write fd array len
      if rc == len
         then 
            done
         else
            ioError(userError("Error writing to wish"))

fdWriteLn :: Posix.Fd -> String -> IO ()
fdWriteLn fd str =
   do
      let 
         toWrite = str ++ "\n"
         len = length toWrite
      nWritten <- fdWrite fd toWrite
      if nWritten < fromIntegral len
         then
            ioError(userError ("Writing "++str++" only "++
               (show nWritten) ++ " bytes written"))
         else
            done

data FdBuffer = FdBuffer {
   fD :: Fd,
   buffer :: MVar String
   }

fdMkReadLn fD =
   do
      buffer <- newMVar ""
      return (FdBuffer {
        fD = fD,
        buffer = buffer
        })

-- start with buffer empty and MVar empty.  If count
refreshBuffer :: FdBuffer -> IO ()
refreshBuffer (FdBuffer {fD = fD,buffer = buffer}) =
   do
      threadWaitRead (fdToInt fD)
      (str,count) <- fdRead fD 1000
      putMVar buffer str
      done

fdReadLn :: FdBuffer -> IO String
fdReadLn buffer = fdR buffer []
   where
      fdR fdB@(FdBuffer {buffer = buffer}) acc =
         do
            contents <- takeMVar buffer
            let
               split "" = Nothing
               split ('\n':str) = Just ("",str)
               split (c:str) = case split str of
                  Nothing -> Nothing
                  Just (first,str) -> Just (c:first,str)
            case split contents of
               Just (line,rest) ->
                  do
                     putMVar buffer rest
                     return (acc++line)
               Nothing -> 
                  do
                     refreshBuffer fdB
                     fdR fdB (acc++contents)