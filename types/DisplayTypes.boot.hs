{- This file is intended as a skeleton of the DisplayTypes module, in which
   we define just enough to generate a valid .hi-boot file containing
   the WrappedDisplayType.
   -}
module DisplayTypes(
   WrappedDisplayType(..),
   DisplayType, -- object class so we don't have to define much
   ) where

data WrappedDisplayType = forall displayType .
   DisplayType displayType => WrappedDisplayType displayType

-- We try to mirror the class structure, but only put 1 function in
-- each one to make the class non-trivial.

class HCV displayType => DisplayType displayType where
   baz :: displayType -> String

-- meant to mirror HasCodedValue.  We at least keep the kinds the same.
class HCV value where 
   foo :: value -> String
