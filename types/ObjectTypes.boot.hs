{- This file is intended as a skeleton of the ObjectTypes module, in which
   we define just enough to generate a valid .hi-boot file containing
   the WrappedObjectType.
   -}
module ObjectTypes(
   WrappedObjectType(..),
   ObjectType, -- opaque class so we don't need to define much
   ) where

data WrappedObjectType = forall objectType object .
   ObjectType objectType object => WrappedObjectType objectType

-- We try to mirror the class structure, but only put 1 function in
-- each one to make the class non-trivial.

class (HCV objectType,HCV object) =>
   ObjectType objectType object
      | objectType -> object, object -> objectType where
   bar :: objectType -> object

-- meant to mirror HasCodedValue.  We at least keep the kinds the same.
class HCV value where 
   foo :: value -> String


