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

                
   ######################################################################### -}


module SAP (
        SAP,
        newSAP,

        call,
        oneWayCall,

        accept,
        provide

        ) where

import Thread
import Variable
import Selective

import Debug(debug)


-- --------------------------------------------------------------------------
-- Type and Commands
-- --------------------------------------------------------------------------

data RPC a b = RPC a (MVar (Answer b))

newtype SAP a b = SAP  (Channel (RPC a b)) deriving Eq


-- --------------------------------------------------------------------------
-- SAP Construction
-- --------------------------------------------------------------------------

newSAP  :: IO (SAP a b)
newSAP = newChannel >>= return . SAP


-- --------------------------------------------------------------------------
-- Service Call
-- --------------------------------------------------------------------------

call :: SAP a b -> a -> EV b
call (SAP ch) val = event ( do {
        var <- newEmptyMVar;
        return (send ch (RPC val var) >>> (takeMVar var >>= propagate))
        })

oneWayCall :: SAP a b -> a -> EV ()
oneWayCall  (SAP ch)  val = event ( do {
        var <- newEmptyMVar;
        return (send ch (RPC val var))
        })


-- --------------------------------------------------------------------------
-- Service Accept
-- --------------------------------------------------------------------------

accept :: SAP a b -> (a -> IO b) -> EV b
accept sap fun = provide sap (\a -> fun a >>= \res -> return (res,res))


-- --------------------------------------------------------------------------
-- Service Provision
-- --------------------------------------------------------------------------

provide :: SAP a b -> (a -> IO (b,c)) -> EV c
provide (SAP ch) fun = receive ch >>>= \(RPC val var) -> do {
        res <- try (fun val);
        case res of
                (Left excp) -> do {putMVar var (Left excp); raise excp}
                (Right (cl,sv)) -> do {putMVar var (Right cl); return sv}
        }

