{- The iNodeServer listens on port 11394 for new connections and
   sends them new file names when they send a line.  This is used
   by CVSDB.hs to generate new file names for CVS data.  
   The file names lie in current directory in which this program is run.
   -}
module Main(main) where

import Concurrent
import Directory

import Computation
import Debug(debug)

import Event
import EV
import FileEV
import SocketEV

import NewINode

handleClient :: INodeSource -> HandleEV -> IO ()
handleClient iNodeSource handle =
   do
      debug "waiting for request"
      nextRequest <- sync(readFileEV handle)
      debug "request received"
      nextNode <- newINode iNodeSource
      debug "node computed"
      writeFileEV handle nextNode
      handleClient iNodeSource handle

mainLoop :: INodeSource -> (EV(HostDesc,PortDesc,HandleEV)) -> IO ()
mainLoop iNodeSource newClientEV =
   do
      (_,_,handle) <- sync(newClientEV)
      debug "client got"
      forkIO (handleClient iNodeSource handle)
      mainLoop iNodeSource newClientEV

main :: IO ()
main =
   do
      prefix <- getCurrentDirectory
      iNodeSource <- mkINodeSource prefix
      debug "iNodeSource"
      portDesc <- makePort (11394::Int)
      newClientEV <- listenEV portDesc
      debug "listening"
      mainLoop iNodeSource newClientEV
