{- #########################################################################

MODULE        : Object
AUTHOR        : Einar Karlsen,  completely rewrittenn by George
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION       : 0.2
DESCRIPTION   : Objects, basically entities with a unique ID. Used later on
                to tag 1) widgets, 2) interactors, 3) graphs etc. 

We now use C (in object.c).  This means it should be faster.  Also it gets
rid of an unsafePerformIO, which has already caused problems with Linux GHC.

   ######################################################################## -}


module Object (
   ObjectID(..),
   Object(..),
   newObject, -- generates a unique object
   newInt -- generates a unique integer.
   ) where

import Debug(debug)

-- --------------------------------------------------------------------------
-- Class Object
-- --------------------------------------------------------------------------

newtype ObjectID = ObjectID Int deriving (Eq,Ord)

class Object o where
   objectID :: o -> ObjectID

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

foreign import ccall unsafe "object.h next_object_id" newInt :: IO Int

newObject :: IO ObjectID
newObject = 
   do
      nextInt <- newInt
      return(ObjectID nextInt)

