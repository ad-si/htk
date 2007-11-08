-- | This module introduces the functions available to the server to access
-- the database.
--
-- NB.  At various points we assume that these functions are only used
-- in a single-threaded way.  The server code should ensure this.
module SimpleDBServer(
   SimpleDB, -- SimpleDB.  There should be only one of them per program.
   openSimpleDB, -- :: VersionState -> IO SimpleDB
      -- Initialise database, reading backup off disk if necessary.

   -- SimpleDBCommand/Response are the types of queries and responses
   -- to the DB.
   -- Each is an instance of Show.
   SimpleDBCommand(..),
   SimpleDBResponse(..),
   VersionInformation(..),

   ChangeData, -- = Either ICStringLen (ObjectVersion,Location)


   Diff(..),
      -- used in SimpleDBResponse to encode the difference between a version
      -- and (presumably, earlier) versions.

   -- Location of objects.
   Location,
   -- The specialLocation1 and specialLocation2 are preallocated locations,
   -- they will never be allocated by newLocation.
   specialLocation1, -- :: Location
   specialLocation2, -- :: Location

   -- Type of versions.  These versions are global, that is they refer to
   -- collections of objects, not individual objects.
   ObjectVersion,
   -- firstVersion points to the first version that will be
   -- allocated.
   firstVersion, -- :: Version

   querySimpleDB,
      -- :: User -> SimpleDB -> SimpleDBCommand -> IO SimpleDBResponse
   ) where

import SimpleDBTypes
import OpenSimpleDB
import QuerySimpleDB
import LocationAllocation
import VersionInfo
