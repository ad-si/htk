{- Initialise MMiSS Repository -}
module MMiSSInitialise(
   mmissInitialise, -- :: IO Repository,
   ) where

import VersionDB
import Initialisation

import MMiSSRegistrations
import MMiSSObjects

mmissInitialise :: IO Repository
mmissInitialise =
   do
      doMMiSSRegistrations -- register MMiSS types
      repository <- initialiseGeneral MMiSSObjects.initialiseObjectTypes
          -- do standard initialisations
      return repository
