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

Documented on section 8.4.3 of Einar's thesis

Of course now (a) all threads except the main thread are daemon threads;
(b) 

It's necessary to understand collector.c/collector.h to understand this
module.  The problem is that GHC (when the code was written) required
finalisers for foreign objects to be C functions, but had no way
of calling Haskell from C.  So the strategy is to pass the C function
"SendCollectorFD" as a finaliser; this C function does nothing except
write the address to be finalised to a special pipe.

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
-- Collectible Objects
-- --------------------------------------------------------------------------

data CollectibleObj = CollectibleObj Addr ForeignObj 
-- You need one of these for each object to be Collect'ed.  
-- The idea is this.  You call newCollectibleObj to get an
-- object.  This generates a new CollectibleObj which you
-- reference from your object.  When GHC wants to deallocate
-- this object, it will try to deallocate the foreign object,
-- which will mean calling SendCollectorFD with the Addr of the
-- object.  We remember Addr also as a key for the Collector's
-- finite map.
newCollectibleObj :: IO CollectibleObj 
newCollectibleObj = 
   do
      addr <- (_ccall_ NewAddr) :: IO Addr
      destr <- (_ccall_ AddrSendCollectorFD) :: IO Addr
      -- this is the C destructor (which writes the address it is given
      -- to a pipe).
      fobj <- makeForeignObj addr destr
      return (CollectibleObj addr fobj)

instance Eq CollectibleObj where
   (CollectibleObj addr1 _) == (CollectibleObj addr2 _) = addr1 == addr2

-- --------------------------------------------------------------------------
-- The Collector
-- --------------------------------------------------------------------------

-- There is just one collector, set up by initCollector
data Collector =  
   Collector
      ObjectID -- unique id, necessary to make Collector an
               -- instance of Object.  This means for example
               -- we can use registerTool in InfoBus on it.
      (PVar CST) 
      Posix.Fd -- _read_ Addr values for objects to be collected from here.
      Posix.Fd -- This is the write end of the channel.  We keep it
               -- around just so we can close it eventually.
        
data CST = CST ToolStatus (FiniteMap Addr (IO ()))
-- The CST contains the map from object addresses to their
-- destructor

-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class Collectible o where
   getCollectibleObj :: o -> CollectibleObj
   destructor :: IO () -> Config o
   -- Computation.Config w = w -> IO w
   -- In other words "destructor" defines a destructor action
   -- for this particular object.
   -- I can find no other definitions of this method.
   destructor destr o =
      do
         changeVar' 
            pvar 
            (\ (CST status fm) -> CST status (addToFM fm addr destr))
         return o
      where
         Collector _ pvar _ _ = collector
         CollectibleObj addr _ = getCollectibleObj o
        
instance Collectible CollectibleObj where
   getCollectibleObj = id

-- --------------------------------------------------------------------------
--  Set the single unique collector up
-- --------------------------------------------------------------------------

collector :: Collector
collector = IOExts.unsafePerformIO (initCollector)

initCollector = 
   do
      oid <- newObject
      pv <- newPVar (CST Nothing  emptyFM) -- no status, no destructors known
      (rfd,wfd) <- Posix.createPipe;
      let clt = Collector oid pv rfd wfd
      _ccall_ SetCollectorFD wfd  
      forkIO (collectObjects clt) 
      -- set off the thread that processes the destruction requests.
      registerTool clt 
      -- this means it will get destroyed when shutdown()
      -- is called.
      return clt

-- This runs all the time and does the object destruction.
-- When we want it to stop, IE "destroy" is called on Collector, 
-- we set the state part of the collector PVar to something and 
-- send all the messages along the destruct
-- pipe (so they will also get destroyed).
collectObjects :: Collector -> IO ()
collectObjects clt @ (Collector _ pvar rfd wfd) = 
   do
      waitForInputFd rfd
      str <- readLine rfd "" -- get the next line
      addr <- return (intToAddr (read str)) -- address to be destroyed
      debug ("collecting " ++ str); 
      destructor <- 
         updVar' 
            pvar 
            (\ (CST state map) ->
               let
                  destructor = lookupWithDefaultFM map done addr
                  -- done can happen if we call "destroy" on the collector
                  -- and simultaneously objects with destruction methods
                  -- defined get garbage collected.
                  newMap = delFromFM map addr
               in
                  (CST state newMap,destructor)
               )

      try destructor
      _ccall_ FreeAddr addr

      -- See if "destroy" has been called on the Collector.
      CST state map <- getVar pvar
      case state of
         (Just _) -- yes
            | isEmptyFM map -> -- no more objects left to destroy
            do
               try (Posix.fdClose rfd)
               try (Posix.fdClose wfd)
               debug "Collector Finished"
         _ -> collectObjects clt -- loop back.

-- --------------------------------------------------------------------------
--  Tool Instances
-- --------------------------------------------------------------------------

instance Object Collector where
   objectID (Collector oid _ _ _) = oid

instance Destructible Collector where
   -- destroy needs to be read in conjuction with collectObjects.
   -- We need to set the state element of the PVar to Just (something)
   -- and send all registered objects to the destruct channel.
   destroy (Collector _ pvar rfd wfd) = 
      do
         addrs <- -- get the address of destructed objects, and set the
                  -- state element.
            updVar' 
               pvar 
               (\ (CST state map) ->
                  let
                     addrs = keysFM map
                  in
                     ((CST (Just (Posix.Exited ExitSuccess)) map) , addrs)
                  )
         foreach 
            addrs 
            (\addr -> (_ccall_ SendCollectorFD addr)::IO ())

   destroyed _ = 
      error "We couldn't be bothered to define Collector.destroyed"

instance Tool Collector where
   getToolStatus (Collector _ pvar _ _) = 
      do
         CST status _ <- getVar pvar
         return status

instance SingleInstanceTool Collector where
   getToolInstance = return collector
        




