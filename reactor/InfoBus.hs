{- #########################################################################

MODULE        : InfoBus
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 
   InfoBus implements the shutdown command.  This destroys all the
   things registered via registerTool and not
   subsequently registered via deregisterTool.  Tools are identified
   by ObjectId's.


   ######################################################################### -}


module InfoBus (        
   registerTool,
   deregisterTool,
   shutdown   
   ) where


import Concurrent
import FiniteMap
import qualified IOExts(unsafePerformIO,performGC)

import Computation
import Dynamics
import Object
import Debug(debug)

import Destructible

-- --------------------------------------------------------------------------
--  Tool Manager State
-- --------------------------------------------------------------------------

type ToolManager = MVar Tools

type Tools = FiniteMap ObjectID (IO ())


-- --------------------------------------------------------------------------
--  Fetch Tool Manager State
-- --------------------------------------------------------------------------

toolmanager :: ToolManager
toolmanager = IOExts.unsafePerformIO (newMVar emptyFM)


-- --------------------------------------------------------------------------
--  Client Commands
-- --------------------------------------------------------------------------

registerTool :: (Object t, Destroyable t) => t -> IO ()
registerTool t = 
   do 
      map <- takeMVar toolmanager 
      putMVar toolmanager (addToFM map (objectID t) (destroy t))
      done          


deregisterTool :: (Object t) => t -> IO ()
deregisterTool t =  
   do 
      let oid = objectID t
      try( -- ignore exceptions if they occur.  I don't see how they can
           -- actually.
         do
            map <- takeMVar toolmanager
            putMVar toolmanager (delFromFM map oid)
         )
      done

shutdown :: IO ()
shutdown = 
   do
      map <- takeMVar toolmanager
      let cmds = eltsFM map
      putMVar toolmanager emptyFM
      foreach cmds (\cmd -> try cmd)
      IOExts.performGC







