{- #########################################################################

MODULE        : SAP
AUTHOR        : Einar W. Karlsen, 
                Walter Norzel 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2

DESCRIPTION   : This is SAP (Service Access Points) in OPAL style, 
                better known as RPC. 

                T. Frauenstein, W. Grieskamp, P. Pepper, M. Südholt: 
                Communicating Functional Agents and their Application to 
                Graphical User Interfaces, In Proceedings of the 2nd 
                International Conference on Perspectives of System Informatics, 
                Novosibirsk, LNCS,  Springer Verlag, 1996

                SAP's of type (SAP a ()) are similar to channels! 


An SAP value encodes a remote procedure that can be called.
                
   ######################################################################### -}


module SAP (
        SAP,
        newSAP,

        call,
        callIO,
        oneWayCall,

        accept,
        provide

        ) where

import Thread
import Variable
import Selective

import Debug(debug,(@:))


-- --------------------------------------------------------------------------
-- Type and Commands
-- --------------------------------------------------------------------------

data RPC a b = RPC a (MVar (Answer b))
-- RPC presumably stands for "remote procedure call".

newtype SAP a b = SAP  (Channel (RPC a b)) deriving Eq
-- An SAP a b is a way of encoding a remote procedure taking a to b.
-- When you want to call it, you send an 
--   RPC (argument) (placeholder for result)

-- --------------------------------------------------------------------------
-- SAP Construction
-- --------------------------------------------------------------------------

newSAP  :: IO (SAP a b)
newSAP = 
   do
      ch <- newChannel
      return(SAP ch)

-- --------------------------------------------------------------------------
-- Service Call
-- --------------------------------------------------------------------------

call :: SAP a b -> a -> EV b
call (SAP ch) val = 
-- call procedure
   event ( 
      do
         var <- newEmptyMVar
         return (
            send ch (RPC val var) >>> 
              do 
                 resultOrError <- takeMVar var
                 -- the exception is also raised by the service provider
                 propagate resultOrError
            )
      )

callIO :: SAP a b -> a -> IO b
callIO sap arg = sync(call sap arg)

oneWayCall :: SAP a b -> a -> EV ()
oneWayCall  (SAP ch)  val = 
   event ( 
      do 
         var <- newEmptyMVar
         return (send ch (RPC val var))
      )

-- --------------------------------------------------------------------------
-- Service Accept
-- --------------------------------------------------------------------------

accept :: SAP a b -> (a -> IO b) -> EV b
accept sap fun = 
   provide sap (\a -> fun a >>= \res -> return (res,res))
-- See provide.

-- --------------------------------------------------------------------------
-- Service Provision
-- --------------------------------------------------------------------------

provide :: SAP a b -> (a -> IO (b,c)) -> EV c
-- attaches an event (which must then be repeatedly synced on) to service
-- the event.
provide (SAP ch) fun = 
   receive ch >>>= 
      \ (RPC val var) -> 
         do
            res <- try (fun val)
            case res of
               (Left excp) -> 
                  do
                     "40" @: putMVar var (Left excp) 
                     raise excp
               (Right (cl,sv)) -> 
                  do 
                     "41" @: putMVar var (Right cl)
                     return sv


