{- #########################################################################

MODULE        : QSem
AUTHOR        : Einar W. Karlsen,
                Walter Norzel 
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2

DESCRIPTION   : The Quantity semaphore. 

                
   ######################################################################### -}

{- This module doesn't seem to be used anywhere. -}
module QSem (
        module Lock,

        QSem,
        newQSem,
        signalQSem,
        waitQSem,

        illegalResourceSize     
) where


import Thread
import BSem
import Variable
import Lock

import Debug(debug,(@:))


-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data QSem = QSem (MVar (Int, [(Int, BSem)])) deriving Eq


-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Lock QSem where
        release sem  = signalQSem sem 1
        acquire sem  = waitQSem   sem 1


-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

newQSem :: Int -> IO QSem
newQSem avail = newMVar (avail, []) >>= return . QSem
 
waitQSem :: QSem -> Int -> IO ()
waitQSem (QSem sem) cont | (cont < 1) = raise illegalResourceSize
waitQSem (QSem sem) cont = do {
        (avail, blocked) <- takeMVar sem;
        if avail > cont then 
                "20" @: putMVar sem (avail-cont, [])
        else do {
                block <- newBSem;
                "21" @: putMVar sem (avail, blocked ++ [(cont,block)]);
                acquire block
                }
}


signalQSem :: QSem -> Int -> IO ()
signalQSem (QSem sem) cont | (cont < 1) = raise illegalResourceSize
signalQSem (QSem sem) cont = do {
        (avail, blocked) <- takeMVar sem;
        (avail', blocked') <- free (avail+cont) blocked;
        "22" @: putMVar  sem (avail', blocked')
} where free avail []                    = return (avail, [])
        free avail ((req,block):blocked) | avail > req = do {
                release block; 
                free (avail-req) blocked 
                }
        free avail ((req,block):blocked) = do {
                (avail', blocked') <- free avail blocked;
                return (avail', (req,block):blocked')
                }


-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------

illegalResourceSize :: IOError
illegalResourceSize = userError "Semaphore: Illegal resource size"


