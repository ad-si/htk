{- #########################################################################

MODULE        : Buffer
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : A bounded buffer implemented, as it is commonly the case
                in a synchronised setting, by a buffer process, e.g. in
                
                J. H. Reppy: Higher-Order Concurrency, Ph. D. Thesis, 
                Department of Computer Science, Cornell University, 1992 


   ######################################################################### -}


module Buffer (
        HasReceiveEV(..),
        HasReceiveIO(..),
        HasSendIO(..),

        Buffer,
        newBuffer

) where

import Concurrency
import Queue

import Debug(debug)


-- --------------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------------

data Buffer a = Buffer (Channel a) (Channel a)

-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance Eq (Buffer a) where
        (Buffer _ ch1) == (Buffer _ ch2) = ch1 == ch2


instance HasReceiveEV Buffer messageType where
        receive (Buffer _ getch) = receive getch

instance HasSendIO Buffer messageType where
        sendIO (Buffer putch _) val = sync(send putch val)


-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------

newBuffer :: Maybe Int -> IO (Buffer a)
newBuffer max = do {
        putch <- newChannel; 
        getch <- newChannel; 
        forkIO (buffer max emptyQ putch getch);
        return (Buffer putch getch);
        }

buffer :: Maybe Int -> Queue a -> Channel a -> Channel a -> IO ()
buffer max q putch getch = sync (
        unlessEV (isFullQ max q) (
                receive putch >>>= \v -> buffer max (insertQ q v) putch getch)
   +>   unlessEV (isEmptyQ q) (
                send getch (headQ q) >>> buffer max (tailQ q) putch getch)
   )
   where isFullQ Nothing _ = False
         isFullQ (Just max) q = lengthQ q == max        

