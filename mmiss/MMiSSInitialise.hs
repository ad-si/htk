{- Initialise MMiSS Repository -}
module MMiSSInitialise(
   mmissInitialise, -- :: IO Repository,
   ) where

import Computation(done)

import VersionDB
import Initialisation
import View

import MMiSSRegistrations

mmissInitialise :: IO Repository
mmissInitialise =
   do
      doMMiSSRegistrations -- register MMiSS types
      repository <- initialiseGeneral viewInitialisations
          -- do standard initialisations
      return repository

-- viewInitialisations does nothing now
viewInitialisations :: View -> IO ()
viewInitialisations view = done