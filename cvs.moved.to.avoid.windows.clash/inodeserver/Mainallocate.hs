{- The allocate server listens on port 11394 for new connections.
   It allocates new CVSFiles and new CVSVersions.
   To get a new CVSFile, send it a blank line.  To get a new
   CVSVersion to branch off an old one, send it the pair
   (CVSFile,CVSVersion), and you get a CVSVersion suitable to
   be used for cvsCommit.
   -}
module Main(main) where

import IO

import Exception
import Concurrent

import Computation
import IOExtras
import Debug(debug)

import Event
import EV
import SocketEV

import CVSTypes

import Allocate

handleClient :: (MVar AllocateState) -> Handle -> IO ()
handleClient allocateStateMVar handle =
   do
      nextLine <- catchEOF(hGetLine handle)
      case nextLine of
         Nothing -> return ()
         Just nextRequest ->
            do
               toWrite <- case nextRequest of
                  "" -> -- newCVSFile
                     do
                        allocateState <- takeMVar allocateStateMVar
                        let
                           (newAllocateState,cvsFile) = 
                              newCVSFile allocateState
                        putMVar allocateStateMVar newAllocateState
                        return (show cvsFile)
                  cvsFileVersion -> 
                     do
                        let 
                           (cvsFile :: CVSFile,cvsVersion :: CVSVersion) = 
                              read cvsFileVersion
                        allocateState <- takeMVar allocateStateMVar
                        let
                           (newAllocateState,newCvsVersion) = 
                              newCVSVersion allocateState cvsFile cvsVersion
                        putMVar allocateStateMVar newAllocateState
                        return (show newCvsVersion)
               hPutStrLn handle toWrite
               handleClient allocateStateMVar handle

mainLoop :: MVar AllocateState -> (EV(HostDesc,PortDesc,Handle)) -> IO ()
mainLoop allocateStateMVar newClientEV =
   do
      (_,_,handle) <- sync(newClientEV)
      forkIO (handleClient allocateStateMVar handle)
      mainLoop allocateStateMVar newClientEV

main :: IO ()
main =
   do
      allocateStateMVar <- newMVar initialAllocateState
      newClientEV <- listenEV (11394::Int)
      mainLoop allocateStateMVar newClientEV



