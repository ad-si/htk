{- Links are references to objects in the repository, which are instances
   of HasCodedValue, and so can be used as attributes of other objects
   in the repository. -}
module Link(
   Link,  
      -- A Link x (should) point to an object of type x.
      -- It is the responsibility of the person reading the link
      -- to make sure that x has the right type, otherwise
      -- the resolve function will create trouble when we
      -- attempt to read the link in.

      -- A Link should always be considered in the context of the
      -- containing View; it should never be used with another view.
   resolve, -- :: Typeable x => View -> Link x -> IO (Versioned x)

   mkLink, -- :: Typeable x => View -> x -> IO (Link x)

   topLinks, -- :: Typeable x => Repository -> [(View,Link x)]
      -- Extract the links corresponding to the repository's 
      -- initialLocation (which had better all have the same type!)
   ) where

import Dynamics

import VersionDB
import CodedValue
import View
import Versioning

-- ----------------------------------------------------------------
-- The basic datatype and simple instances
-- ----------------------------------------------------------------

data Link x = 
      Present (Versioned x)
   |  Absent (VersionedPtr x)


link_tyCo :: TyCon
link_tyCo = mkTyCon "Link" "Link"

instance HasTyCon1 Link where
   tyCon1 _ = link_tyCo

-- ----------------------------------------------------------------
-- Creating new Links.
-- ----------------------------------------------------------------

resolve :: (HasCodedValue x,Typeable x) => View -> Link x -> IO (Versioned x)
resolve view (Present versioned) = return versioned
resolve view (Absent versionedPtr) =
   resolveLocation view (toLocation versionedPtr) (restore view versionedPtr)
   
mkLink :: Typeable x => View -> x -> IO (Link x)
mkLink view x =
   do
      versioned <- newVersioned view x
      return (Present versioned)

topLinks :: Typeable x => Repository -> IO [(View,Link x)]
topLinks repository =
   do
      topVersions <- listVersions repository initialLocation
      mapM
         (\ objectVersion ->
            do
               view <- newView repository
               return (view,
                  Absent (mkVersionedPtr initialLocation objectVersion))
            )
         topVersions

-- ----------------------------------------------------------------
-- Instance of HasCodedValue
-- ----------------------------------------------------------------

instance HasCodedValue x => HasCodedValue (Link x) where
   encodeIO (Absent versionedPtr) codedValue view =
      encodeIO versionedPtr codedValue view
   encodeIO (Present versioned) codedValue view =
      do
         versionedPtr <- store view versioned 
         encodeIO versionedPtr codedValue view
   decodeIO = mapDecodeIO Absent
