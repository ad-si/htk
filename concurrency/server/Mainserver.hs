{- The echo server is used for the reactor for communicating threads.
   It simply listens to port 11393 (a randomly chosen number).
   Everything a client wishes to close it MUST send a single
   line containing just the null character.  Until then, everything
   sent by any client is echoed to all connected clients.
   -}
module Main(main) where

import Concurrent
import Posix

import Object

import EV
import Event
import FileEV
import SocketEV
import Debug(debug)




main :: IO ()
main = 
   do
      installHandler sigPIPE Ignore Nothing
      newClientEv <- listenEV (11393::Int)
      let 
         newClient = newClientEv >>>=
            (\ (_,_,handleEV) -> return handleEV)
      listener newClient []

data Client =
   Client {
      oID :: ObjectID,
      handle :: HandleEV
      }

listener :: EV HandleEV -> [Client] -> IO ()
-- First argument is source of new clients
-- Second argument is current client list.
-- listener function should never return.
listener newClient clientList =
   sync(
         newClient >>>=
            (\ handle ->
               do
                  oID <- newObject
                  listener newClient 
                     ((Client {oID=oID,handle=handle}):clientList)
               )  
      +> (choose
            (map
               (\ (Client{oID=oID,handle=handle}) ->
                  readFileEV handle >>>=
                     (\ line ->
                        if line == "\0"
                           then -- get rid of this client
                              do
                                 debug "server:Kill client"
                                 closeFileEV handle
                                 let 
                                    newClientList =
                                       filter
                                          (\ (Client{oID=oID2}) ->
                                             oID /= oID2
                                             )
                                          clientList
                                 listener newClient newClientList
                           else
                              do -- write line to all clients
                                 debug ("server:"++line)
                                 sequence_
                                    (map
                                       (\ Client{handle=handle} ->
                                          writeFileEV handle line
                                          )
                                       clientList
                                       )
                                 listener newClient clientList
                        )    
                  )
               clientList
               )   
            )  
      )      



