{- Versioning is the general code for checking in and out
   versioned objects. -}
module Versioning(
   Versioned, -- a "Versioned x" is an x under version control.
   VersionedPtr, -- a "VersionedPtr x" is a pointer to an x under
      -- version control in the view.
      -- A VersionedPtr is an instance of HasCodedValue.

   newVersioned, -- :: View -> x -> Versioned x
   store, -- :: HasCodedValue x => View -> Versioned x -> IO (VersionedPtr x)
   restore, -- :: HasCodedValue x => View -> VersionedPtr x -> IO (Versioned x)

   getCurrent, -- :: Versioned x -> IO x
   -- The following two functions allow us to update
   -- the contents of a Versioned object.
   update, -- :: Versioned x -> x -> IO ()
   dirty, -- :: Versioned x -> IO ()
   -- dirty indicates that the object has been changed without
   -- updating the value.  This is only useful when the value
   -- itself refers to something that needs to be re-stored.

   ) where

import Concurrent

import AtomString

import VersionDB
import View
import CodedValue
import CodedValueStore

-- Invariants:
--   The MVar locks the Versioned x.
--   When there is an ObjectVersion in the MVar,
--   that means the x in the MVar is exactly stored by
--   that version in the view.
data Versioned x =
   Versioned {
      location :: Location, -- Location in the view.
      status :: MVar (x,Status)
      }

data Status = 
      UpToDate ObjectVersion -- This object stored and up-to-date
   |  Dirty ObjectVersion -- This object stored, but since modified
   |  Virgin -- Object not stored.

data VersionedPtr x = VersionedPtr Location ObjectVersion


-- ---------------------------------------------------------- 
-- Creating new versioned objects
-- ---------------------------------------------------------- 

newVersioned :: View -> x -> IO (Versioned x)
newVersioned view x =
   do
      location <- newLocation (getRepository view)
      status <- newMVar (x,Virgin)
      return(Versioned{
         location = location,
         status = status
         })         

-- ---------------------------------------------------------- 
-- Storing and receiving objects in the view
-- ---------------------------------------------------------- 

store :: HasCodedValue x => View -> Versioned x -> IO (VersionedPtr x)
store view (Versioned {location = location,status = status}) =
   do
      xv <- takeMVar status
      let
--         doStore :: HasCodedValue x 
--            => x -> Maybe ObjectVersion -> IO (VersionedPtr x)
         doStore x version' =
            do
               xCodedValue <- doEncodeIO x view
               objectSource <- toObjectSource xCodedValue
               version <- commit (getRepository view) objectSource 
                  location version'
               putMVar status (x,UpToDate version) 
               return (VersionedPtr location version)
      case xv of
         (_,UpToDate version) -> 
            -- An up-to-date copy of this object is stored.
            do
               putMVar status xv
               return (VersionedPtr location version)
         (x,Dirty version) -> doStore x (Just version)
         (x,Virgin) -> doStore x Nothing 

restore :: HasCodedValue x => View -> VersionedPtr x -> IO (Versioned x)
restore view (VersionedPtr location version) =
   do
      (str :: String) <- retrieveString (getRepository view) location version
      x <- doDecodeIO (fromString str) view
      status <- newMVar (x,UpToDate version)
      return (Versioned {
         location = location,
         status = status
         }) 


-- ---------------------------------------------------------- 
-- Accessing Versioned objects
-- ---------------------------------------------------------- 


getCurrent :: Versioned x -> IO x
getCurrent (Versioned {status = status}) =
   do
      (x,stat) <- readMVar status
      return x

update :: Versioned x -> x -> IO ()
update (Versioned {status = status}) newX =
   do
      (_,stat) <- takeMVar status
      putMVar status (newX,
         case stat of
            Virgin -> Virgin
            UpToDate version -> Dirty version
            Dirty version -> stat
         )

dirty :: Versioned x -> IO ()
dirty (Versioned {status = status}) =
   do
      (oldX,stat) <- takeMVar status
      putMVar status (oldX,
         case stat of
            Virgin -> Virgin
            UpToDate version -> Dirty version
            Dirty version -> stat
         )

-- ---------------------------------------------------------- 
-- VersionedPtr as an instance of HasCodedValue
-- ---------------------------------------------------------- 

instance HasCodedValue (VersionedPtr x) where
   encodeIO = mapEncodeIO (\ (VersionedPtr x y) -> (Str x,Str y))
   decodeIO = mapDecodeIO (\ (Str x,Str y) -> (VersionedPtr x y))
