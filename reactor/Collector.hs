{- #########################################################################

MODULE        : Collector
AUTHOR        : Einar W. Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : beta

DESCRIPTION   : A collector for foreign objects that allow the destruction
                routine to be a haskell behaviour rather than a C-function.

CAVEAT        : Since GHC does not provide a concept of daemon threads, 
                we must terminate the collector by hand rather than having
                it GC'ed when no longer needed. 

                Termination is achieved by registering the collector
                as a tool.              

                Implementation should change as soon as it becomes
                possible to call Haskell from C: then there would
                be no need for the collector thread any more.


   ######################################################################### -}


module Collector (
        Object(..),

        Destructible(..),
        Tool(..),
        SingleInstanceTool(..),

        Collectible(..),

        Collector,
        CollectibleObj,
        newCollectibleObj

        ) where

import qualified IOExts(unsafePerformIO)
import Addr
import Foreign

import Concurrency
import FiniteMap
import SIMClasses
import InfoBus
import qualified Posix
import System
import Object
import ChildProcess(readLine)
import Debug(debug)
import ThreadWait

-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class Collectible o where
        getCollectibleObj :: o -> CollectibleObj
        destructor        :: Destructor -> Config o
        destructor destr o =  do {
                changeVar' pv (\(st, fm) -> (st,addToFM fm addr destr));
                return o
                } where Collector _ pv _ _ = collector
                        CollectibleObj addr _ = getCollectibleObj o
        

instance Collectible CollectibleObj where
        getCollectibleObj = id


-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Collector =  
        Collector
                ObjectID 
                (PVar CST)
                Posix.Fd                                -- read end
                Posix.Fd                                -- write end
        
type CST = (ToolStatus,FiniteMap Addr Destructor)


-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Collector where
        objectID (Collector oid _ _ _) = oid

instance Destructible Collector where
        destroy (Collector _ pv rfd wfd) = do {
                addrs <- updVar' pv (\(st, fm) ->
                          ((Just (Posix.Exited ExitSuccess), fm), keysFM fm));
                foreach addrs (\addr -> (_ccall_ SendCollectorFD addr)::IO ());
                done
                }
        destroyed _ = inaction
        

instance Tool Collector where
        getToolStatus (Collector _ pv _ _) = getVar pv >>= return . fst


instance SingleInstanceTool Collector where
        getToolInstance = return collector
        

-- --------------------------------------------------------------------------
-- Syntactic Domains
-- --------------------------------------------------------------------------

data CollectibleObj = CollectibleObj Addr ForeignObj 

type Destructor =  IO ()

instance Eq CollectibleObj where
        (CollectibleObj addr1 _) == (CollectibleObj addr2 _) = addr1 == addr2


-- --------------------------------------------------------------------------
-- Create Collectible Object
-- --------------------------------------------------------------------------

newCollectibleObj :: IO CollectibleObj 
newCollectibleObj = do {
        addr <- (_ccall_ NewAddr) :: IO Addr;
        destr <- (_ccall_ AddrSendCollectorFD) :: IO Addr;
        fobj <- makeForeignObj addr destr;
        cobj <- return (CollectibleObj addr fobj);
--      destructor done cobj;
        return cobj
} 

                
-- --------------------------------------------------------------------------
--  Collector Interpreter
-- --------------------------------------------------------------------------

collector :: Collector
collector = IOExts.unsafePerformIO (initCollector)

initCollector = do {
        oid <- newObject;
        pv <- newPVar (Nothing, emptyFM);
        (rfd,wfd) <- Posix.createPipe;
        clt <- return (Collector oid pv rfd wfd);
        _ccall_ SetCollectorFD wfd;             
        forkIO (collectObjects clt);
        registerTool clt;       
        return clt
}

-- --------------------------------------------------------------------------
--  Collector Dispatcher Thread
-- --------------------------------------------------------------------------

collectObjects :: Collector -> IO ()
collectObjects clt @ (Collector _ pv rfd wfd) = do {
        waitForInputFd rfd;
        str <- readLine rfd ""; 
        addr <- return (intToAddr (read str));
        debug ("collecting " ++ str); 
        destr <- updVar' pv (\(tst,fm) -> ((tst,delFromFM fm addr),lookupWithDefaultFM fm done addr));
        try destr;
        _ccall_ FreeAddr addr;
        (tst,fm) <- getVar pv;
        case tst of
                (Just _) | isEmptyFM fm -> do {
                        try (Posix.fdClose rfd);
                        try (Posix.fdClose wfd);
                        debug "Collector Finished";
                        done
                        }

                _ -> collectObjects clt
}
