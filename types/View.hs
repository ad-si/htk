{- A View represents a particular global version in the repository,
   AS STORED LOCALLY.  Thus the files in a View can be updated,
   and eventually committed to make a new version.

   This file is compiled quite early on, so it is important it
   doesn't use too much.
   -}
module View(
   View,
   newView, -- :: Repository -> IO View
   getRepository, -- :: View -> Repository

   -- The View contains objects cached by location.  We don't
   -- specify the type of these objects, but actually it will
   -- be (Versioned (something)).
   resolveLocation, -- :: Typeable result 
      -- => View -> Location -> IO result -> IO result
   ) where



import Dynamics
import Registry

import VersionDB

-- ----------------------------------------------------------------------
-- The View type
-- ----------------------------------------------------------------------

data View = View {
   repository :: Repository,
   cache :: UntypedLockedRegistry Location
   }
   
-- ----------------------------------------------------------------------
-- Operations on it
-- ----------------------------------------------------------------------

newView :: Repository -> IO View
newView repository =
   do
      cache <- newRegistry
      return (View {
         repository = repository,
         cache = cache
         })

getRepository :: View -> Repository
getRepository = repository

resolveLocation :: Typeable result => View -> Location -> IO result 
   -> IO result
resolveLocation (View{cache = cache}) location getNewItem =
   transformValue cache location
      (\ itemOpt -> 
         do
            newItem <- case itemOpt of
               Just oldItem -> return oldItem
               Nothing -> getNewItem
            return (Just newItem,newItem)
         )
               
