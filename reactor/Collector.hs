{- #########################################################################

MODULE        : Collector
AUTHOR        : George Russell
DESCRIPTION   : A collector for foreign objects that allow the destruction
                routine to be a haskell behaviour rather than a C-function.

Documented on section 8.4.3 of Einar's thesis.

Now however we can (and are forced to) simplify this to fit with
the new GHC Foreign Function Interface.

   ######################################################################### 
-}


module Collector (
   Destructible(..),
   Collectible(..),
   
   CollectibleObj,
   newCollectibleObj
   
   ) where

import qualified IOExts(unsafePerformIO)
import Addr
import Foreign
import System
import qualified Storable

import Concurrency
import SIMClasses
import InfoBus
import ChildProcess(readLine)
import Debug(debug)

-- --------------------------------------------------------------------------
-- Collectible Objects
-- --------------------------------------------------------------------------

data CollectibleObj = CollectibleObj ForeignObj (PVar(IO ()))
-- You need one of these for each object to be Collect'ed. 
-- The Addr in the ForeignObj points to a space of size 1, the only 
-- important thing being that this is always different.  The PVar contains 
-- the
-- destructor action for this object.

newCollectibleObj :: IO CollectibleObj 
newCollectibleObj = 
   do
      storageVar <- newPVar done 
      let 
         whenDone =
            changeVar storageVar
               (\ action ->
                  do
                     try action
                     return done
                  )
      foreignObj <- newForeignObj nullAddr whenDone
      return (CollectibleObj foreignObj storageVar)

instance Eq CollectibleObj where
   (CollectibleObj foreignObj1 _) == (CollectibleObj foreignObj2 _) = 
      foreignObj1 == foreignObj2

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
         setVar pvar destr
         return o 
      where
         CollectibleObj _ pvar = getCollectibleObj o
        
instance Collectible CollectibleObj where
   getCollectibleObj = id

        




