-- | Objects with a name and an icon (used for several purposes).
module CItem (

  CItem(..)

) where

import HTk
import Name


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

-- | Objects with a name and an icon.
class Eq c => CItem c where
  -- Gets the object\'s name.
  getName :: c -> IO Name
  -- Gets the object\'s icon.
  getIcon :: c -> IO Image
