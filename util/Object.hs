{- #########################################################################

MODULE        : Object
AUTHOR        : Einar Karlsen,  George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : Objects, basically entities with a unique ID. Used later on
                to tag 1) widgets, 2) interactors, 3) graphs etc. 

                Currently, its just a simple integer, with all the inherent
                limitations. A more general solution would be to use a
                world wide unique string generated using e.g. CORBA.


   ######################################################################### -}


module Object (

        ObjectID(..),
        Object(..),
        newObject
        ) where

import IOExts(unsafePerformIO)
import Concurrent
import Computation

import Debug(debug)

-- --------------------------------------------------------------------------
-- Class Object
-- --------------------------------------------------------------------------

newtype ObjectID = ObjectID Int

class Object o where
        objectID :: o -> ObjectID


-- --------------------------------------------------------------------------
-- Instance Object
-- --------------------------------------------------------------------------

instance Eq ObjectID where
        (ObjectID o1) == (ObjectID o2) = o1 == o2

instance Ord ObjectID where
        (ObjectID o1) <= (ObjectID o2) = o1 <= o2

instance Show ObjectID where
    showsPrec d (ObjectID n) r = showsPrec d n r

instance Read ObjectID where
    readsPrec p b =
      case reads b of
        [] -> []
        ((v,xs):_) ->[(ObjectID v,xs)]

-- --------------------------------------------------------------------------
-- New Object Identifier
-- --------------------------------------------------------------------------

newObject :: IO ObjectID
newObject = do {
        n <- takeMVar objectid;
        putMVar objectid (succ n);
        return (ObjectID n)
        }       

objectid :: MVar Int
objectid = unsafePerformIO(newMVar 0)


