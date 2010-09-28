-- | Objects with a name and an icon (used for several purposes).
module HTk.Toolkit.CItem (

  CItem(..)

) where

import HTk.Toplevel.HTk
import HTk.Toolkit.Name


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

-- | Objects with a name and an icon.
class Eq c => CItem c where
  -- Gets the object\'s name.
  getName :: c -> IO Name
  -- Gets the object\'s icon.
  getIcon :: c -> IO Image
