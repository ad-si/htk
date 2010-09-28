-- | Initialise MMiSS Repository
module MMiSS.Initialise(
   mmissInitialise, -- :: IO Repository,
   ) where

import Util.Computation(done)

import Server.HostsPorts

import Types.VersionDB
import Types.Initialisation
import Types.View

import MMiSS.Registrations

mmissInitialise :: (?server :: HostPort) => IO Repository
mmissInitialise =
   do
      doMMiSSRegistrations -- register MMiSS types
      repository <- initialiseGeneral viewInitialisations
          -- do standard initialisations
      return repository

-- viewInitialisations does nothing now
viewInitialisations :: View -> IO ()
viewInitialisations view = done
