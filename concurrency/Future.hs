{- #########################################################################

MODULE        : Future
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : As it says: Futures as they are known from Multi-List,
                but in a monadic framework.
                
                Very similar to promises actually, but 
                unlike promises, futures do not return the result once,
                but any time a client request it (provided of course,
                that the future has finished). 


   ######################################################################### -}



module Future (
   HasReceiveEV(..),
   HasReceiveIO(..),
   
   Future,
   newFuture
   
   ) where

import Thread
import Selective
import Debug(debug,(@:))


-- --------------------------------------------------------------------------
-- Handle
-- --------------------------------------------------------------------------

data Future  a = Future  (Channel (Answer a)) deriving Eq

-- --------------------------------------------------------------------------
-- Instances
-- --------------------------------------------------------------------------

instance HasReceiveEV Future messageType where
   receive (Future ch) = receive ch >>>= propagate

-- --------------------------------------------------------------------------
-- Commands and Events
-- --------------------------------------------------------------------------

newFuture :: IO a -> IO (Future a)
newFuture beh = 
   do
      ch <- newChannel
      forkIO (
         do 
            ans <- try beh
            forever (sendIO ch ans)
         )
      return (Future ch)
        
