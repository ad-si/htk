{- This module contains the code which actually processes and
   distributes XML requests. -}
module MMiSSDoXml(
   doXml, -- :: Handle -> IO ()
   ) where

import IO

import Control.Concurrent.MVar

import Text.XML.HaXml.Xml2Haskell

import Computation

import PasswordFile


import MMiSSCallServer
import MMiSSCheckOutCommit
import MMiSSGetPut

import MMiSSRequest
import MMiSSSessionState
import MMiSSMessages

doXml :: Handle -> User -> IO ()
doXml handle user =
   do
      state <- newSessionState
      
      error "TBD"

      hPutStrLn handle "Not implemented yet!"
      hClose handle

-- Fall-out when everything goes wrong.
signalError :: Handle -> Messages_status -> String -> IO ()
signalError handle status mess =
   do
      let
         messages = Messages 
            (Messages_Attrs {
               messagesStatus = NonDefault status
               })
            [Messages_Error (Error mess)]

         response = Response messages Nothing

      writeResponse handle response

-- --------------------------------------------------------------------------
-- Reading and writing XML. 
-- --------------------------------------------------------------------------

writeResponse :: Handle -> Response -> IO ()
writeResponse = hPutXml

readResponse :: Handle -> IO Request
readResponse = hGetXml