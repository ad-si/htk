{- #########################################################################

MODULE        : Main
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module Main (
        main    
        ) where


import SIM
import Regex

import Concurrency
import Interaction
import InterActor
import FiniteMap
import SIMClasses(Destructible(..))
import Debug(debug)


main = done

-- --------------------------------------------------------------------------
--  Data Type
-- --------------------------------------------------------------------------

data EventLoopII = EventLoopII {
        eventstream :: EventStream (), 
        threadid    :: MVar (Maybe ThreadID),
        sap         :: SAP  (IO ()) ()
        }

instance HasBinding EventLoopII () where
        bind el ev = do {
                tid <- getThreadID;
                srv <- getVar (threadid el);
                if Just tid == srv then
                        bind es ev
                else 
                        sync (call (sap el) (bind es ev))
                } where es = eventstream el
                        
--      unbind (EventLoopII es) = unbind es

newEventLoopII :: IO EventLoopII
newEventLoopII = do
        es <- newEventStream
        mv <- newMVar Nothing
        sap <- newSAP
        return (EventLoopII es mv sap)


enterEventLoopII :: EventLoopII -> IO ()
enterEventLoopII el = forkIOnull (
        forever (catch (sync ev) (const (become es (inaction::IA ()))))
        )
 where ev =    receive es 
            +> provide (sap el) (\c -> return ((),c)) >>>= id
       es = eventstream el
                        

