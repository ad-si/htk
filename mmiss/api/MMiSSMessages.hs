{- This is the module which provides the MessFns value to be given to
   Messages.setMessFns, and other basic functionality for accessing the
   MMiSSSessionState.
   -}
module MMiSSMessages(
   apiMessFns, -- :: MMiSSSessionState -> MessFns
   getMessages, -- :: MMiSSSessionState -> IO Messages
   signalPanic, -- :: MMiSSSessionState -> IO ()
   signalFailure, -- :: MMiSSSessionState -> IO ()
   ) where

import Control.Concurrent.MVar

import Text.XML.HaXml.Xml2Haskell

import Messages

import MMiSSRequest
import MMiSSSessionState

-- --------------------------------------------------------------------------
-- The Message Functions.
-- --------------------------------------------------------------------------

apiMessFns :: MMiSSSessionState -> MessFns
apiMessFns (state @ (MMiSSSessionState mVar)) =
   let
      alertFn mess = addMessage (Messages_Alert (Alert mess))
      warningFn mess = addMessage (Messages_Warning (Warning mess))
      messageFn mess = addMessage (Messages_Message (Message mess))
      errorFn mess = 
         do
            addMessage (Messages_Error (Error mess))
            signalFailure state

      confirmFn mess =
         do
            errorFn ("API Bug: asked to confirm " ++ mess)
            return False

      addMessage :: Messages_ -> IO ()
      addMessage message =
         modifyMVar_ mVar
            (\ state0 ->
               let
                  (Messages atts messages0) = messages state0
                  messages1 = message : messages0
               in
                  return (state0 {
                     messages = Messages atts messages1
                     })
               )
   in
      MessFns {
         alertFn = alertFn,
         errorFn = errorFn,
         warningFn = warningFn,
         confirmFn = confirmFn,
         messageFn = messageFn,
         htkPres = False
         }

-- --------------------------------------------------------------------------
-- Accessing the messages in the MMiSSSessionState
-- --------------------------------------------------------------------------

-- | Get and clear messages.
getMessages :: MMiSSSessionState -> IO Messages
getMessages (MMiSSSessionState mVar) =
   modifyMVar mVar
      (\ state0 ->
         let
            (Messages atts mess0) = messages state0
     
            messages1 = Messages (Messages_Attrs {
               messagesStatus = Default Messages_status_success}) []

            state1 = state0 {messages = messages1}

            messagesToReturn = Messages atts (reverse mess0)
         in
            return (state1,messagesToReturn)
         )
      
signalPanic :: MMiSSSessionState -> IO ()
signalPanic = setStatus Messages_status_panic

signalFailure :: MMiSSSessionState -> IO ()
signalFailure = setStatus Messages_status_fail

setStatus :: Messages_status -> MMiSSSessionState -> IO ()
setStatus status1 (MMiSSSessionState mVar) =
   modifyMVar_ mVar
      (\ state0 ->
         let
            (Messages _ mess0) = messages state0
            messages1 = Messages 
               (Messages_Attrs {messagesStatus = NonDefault status1})
               mess0
         in
            return (state0 {messages = messages1})
         )
   