{- This module defines links, which are pointers to objects in the repository.
   Thus they can be part of other objects. -}
module Link(
   -- Links.
   -- A Link x is a pointer to an object of type x.  Links are made
   -- instances of HasCodedValue, which means they
   -- can themselves be stored in the repository, for example as attributes
   -- of other objects.
   Link, -- instance of Eq,Ord.

   topLink, -- :: Link x
      -- This link points to the "top object".  This needs to be
      -- created 

   setTopLink, -- :: HasCodedValue x => View -> x -> IO (Versioned x)
      -- This initialises the top link to a particular value.  It should
      -- only be used once, in setting up the repository.

   setOrGetTopLink, -- :: HasCodedValue x => View -> IO x -> IO (Versioned x)
      -- setOrGetTopLink is somewhat safer than setTopLink and initialises the
      -- top object, if that hasn't already been done, via the supplied action.
      -- Otherwise it (harmlessly) returns the existing object.

   -- Versioned values
   -- A Versioned x is a box containing an actual x which can be
   -- stored in the repository.  The x needs to be an instance of 
   -- HasCodedValue.
   Versioned,

   createObject, -- :: HasCodedValue x => View -> x -> IO (Versioned x) 
   -- This is used for creating a completely new object.

   deleteLink, -- :: HasCodedValue x => View -> Link x -> IO ()
   -- This deletes an object from the View.
   -- NB.  It is very important to make sure that the object is not later
   -- dereferenced by fetchLink or readLink, or you will simply get a crash.
   -- 
   -- The LinkManager.deleteLinkedObject takes care of this for any links
   -- it stores.

   cloneLink, 
      -- :: HasCodedValue x => View -> Link x -> View -> Link x -> IO ()
      -- cloneLink view1 link1 view2 link2
      -- *requires* that (a) link1 is not changed in view1; (b) link2 does
      -- not exist in view2.  It creates a copy of link1 in view2, as link2.

   newEmptyObject, -- :: HasCodedValue x => View -> IO (Versioned x)
   -- This creates an object with no contents as a stop-gap so you can
   -- create a link to it, EG for constructing circular lists.
   -- WARNING - updateObject must be used to put in an actual value,
   -- before readObject is done on this Versioned value.  This must also be
   -- done before any commitView, unless the object is deleted via deleteLink.

   updateObject, -- :: HasCodedValue x => View -> x -> Versioned x -> IO ()
   -- This replaces the x value inside an object by a new value, and
   -- marks it to be stored anew.
   dirtyObject, -- :: HasCodedValue x => View -> Versioned x -> IO ()
   -- This marks the x value to be stored anew, even though it hasn't
   -- been changed.  This is needed, for example, when x points to a file,
   -- which has been updated on the side.
   readObject, -- :: HasCodedValue x => View -> Versioned x -> IO x
   -- get the current contents of the object.


   makeLink, -- :: HasCodedValue x => View -> Versioned x -> IO (Link x)
   -- makeLink is used to create a link to an object.
   fetchLink, -- :: HasCodedValue x => View -> Link x -> IO (Versioned x)
   -- look up a link to an object in the repository.
   readLink, -- :: HasCodedValue x => View -> Link x -> IO x
   -- Does fetchLink and readObject in one go.
   writeLink, -- :: HasCodedValue x => View -> Link x -> x -> IO ()
   -- Does fetchLink and updateObject in one go.
   createLink, -- :: HasCodedValue x => View -> x -> IO (Link x)
   -- Does createObject and makeLink in one go.
   newEmptyLink, -- :: HasCodedValue x => View -> IO (Link x)
   -- Like newEmptyObject; similar considerations apply.

   absolutelyNewLink, -- :: HasCodedValue x => Repository -> IO (Link x)
   -- Allocate a new link but don't put it in any view.

   dirtyLink, -- :: HasCodedValue x => View -> Link x -> IO ()
   -- Does fetchLink and dirtyObject in one go.

   fetchLinkWE, -- :: HasCodedValue x => View -> Link x 
      -- -> IO (WithError(Versioned x))
      -- Like fetchLink but should not crash for deleted links.
   readLinkWE, -- :: HasCodedValue x => View -> Link x -> IO (WithError x)
      -- Like readLink but should not crash for deleted links.


   eqLink, -- :: Link x -> Link x -> Ordering
   compareLink, -- :: Link x -> Link y -> Ordering
      -- Provide an efficient way of testing two links for equality, and
      -- ordering them.


   getLastChange, 
      -- :: View -> Link object -> IO (Maybe ObjectVersion)
      -- Return the last-change indicator for a link in the view.

   setLink,
      -- :: HasCodedValue x => View -> x -> Link x -> IO (Versioned x)
      -- Set the contents of a pre-allocated link to a particular value.
      -- NB.  This function is only intended for use for merging.  The
      -- link should not have anything in it before.

   preFetchLinks,
      -- :: HasCodedValue x => View -> [Link x] -> IO ()
      -- This function collects a lot of links in parallel.

   ) where

import Control.Concurrent
import Data.IORef

import Registry
import Dynamics
import AtomString(fromString)
import VariableSet(HasKey(..))
import Computation
import Debug
import Thread
import BinaryAll

import VersionDB
import ViewType
import CodedValue

-- ----------------------------------------------------------------------
-- Links
-- ----------------------------------------------------------------------

newtype Link x = Link Location deriving (Eq,Ord,Typeable)

instance Monad m => HasBinary (Link x) m where
   writeBin = mapWrite (\ (Link location) -> location)
   readBin = mapRead Link

instance HasKey (Link x) Location where
   toKey (Link location) = location

topLink :: Link x
topLink = Link specialLocation2

makeLink :: HasCodedValue x => View -> Versioned x -> IO (Link x)
makeLink _ (Versioned {location = location}) = return (Link location)

fetchLink :: HasCodedValue x => View -> Link x -> IO (Versioned x)
fetchLink view link =
   do
      versionedWE <- fetchLinkWE view link
      return (coerceWithError versionedWE)

preFetchLinks :: HasCodedValue x => View -> [Link x] -> IO ()
preFetchLinks view links =
   mapMConcurrent_ 
      (\ link ->
         do
            fetchLink view link
            done
         )
      links
   

fetchLinkWE :: HasCodedValue x => View -> Link x 
   -> IO (WithError (Versioned x))
fetchLinkWE (view@View{repository = repository,objects = objects}) 
      ((Link location) :: Link x) =
   do
      transformValue objects location 
         (\ objectDataOpt ->
          do
            let
               xName :: String
               xName = show (typeOf (undefined :: x))

               err mess = return (objectDataOpt,hasError (
                  "fetchLink " ++ xName ++ ": " ++ mess))

               readObject :: Bool -> ObjectVersion -> Location 
                  -> IO (Maybe ObjectData,WithError (Versioned x))
               readObject isCloned oldVersion oldLocation =
                  do
                     (xOS :: Either String x) <-
                        catchDBError (
                           do
                              osource <- retrieveObjectSource repository 
                                 oldLocation oldVersion
                              icsl <- exportICStringLen osource
                              doDecodeIO icsl view
                           )
                     case xOS of
                        Left mess -> err mess
                        Right x ->
                           do
                              statusMVar <- newMVar (UpToDate x)
                              let
                                 versioned = Versioned {
                                    location = location,
                                    statusMVar = statusMVar
                                    }
                              return (Just(
                                 PresentObject {
                                    thisVersioned = toDyn versioned,
                                    mkObjectSource 
                                       = mkObjectSourceFn view versioned
                                          (if isCloned 
                                             then
                                                Just (oldLocation,oldVersion)
                                             else
                                                Nothing
                                            )           
                                    }),
                                 hasValue versioned
                                 )
                     
            case objectDataOpt of
               Nothing ->
                  do
                     parentVersionOpt <- getParentVersion view
                     parentVersion <- case parentVersionOpt of
                        Nothing -> error ("Attempt to retrieve non-existent "
                           ++ "link in uncommitted view.")
                        Just parentVersion -> return parentVersion

                     readObject False parentVersion location
               Just (PresentObject {thisVersioned = versionedDyn}) ->
                  case fromDyn versionedDyn of
                     Just versioned 
                        -> return (objectDataOpt,hasValue versioned)
                     Nothing ->
                        let
                           yName = show versionedDyn
                        in
                           err ("fetchLink - type error in link: "
                              ++ "found a " ++ yName
                              ++ " from " ++ show location)
               Just (ClonedObject {
                  sourceLocation = oldLocation,sourceVersion = oldVersion}) ->
                     readObject True oldVersion oldLocation
            )

readLink :: HasCodedValue x => View -> Link x -> IO x
readLink view link =
   do
      versioned <- fetchLink view link
      readObject view versioned 

writeLink :: HasCodedValue x => View -> Link x -> x -> IO ()
writeLink view link x =
   do
      versioned <- fetchLink view link
      updateObject view x versioned

createLink :: HasCodedValue x => View -> x -> IO (Link x)
createLink view x =
   do
      versioned <- createObject view x
      makeLink view versioned

newEmptyLink :: HasCodedValue x => View -> IO (Link x)
newEmptyLink view =
   do
      versioned <- newEmptyObject view
      makeLink view versioned

absolutelyNewLink :: HasCodedValue x => Repository -> IO (Link x)
absolutelyNewLink repository =
   do
      location <- newLocation repository
      return (Link location)


dirtyLink :: HasCodedValue x => View -> Link x -> IO ()
dirtyLink view link =
   do
      versioned <- fetchLink view link
      dirtyObject view versioned

readLinkWE :: HasCodedValue x => View -> Link x -> IO (WithError x)
readLinkWE view link =
   do
      versionedWE <- fetchLinkWE view link
      mapWithErrorIO'
         (\ versioned ->
            do
               object <- readObject view versioned
               return (hasValue object)
            )
         versionedWE


eqLink :: Link x -> Link y -> Bool
eqLink (Link loc1) (Link loc2) = loc1 == loc2

compareLink :: Link x -> Link y -> Ordering
compareLink (Link loc1) (Link loc2) = compare loc1 loc2

cloneLink :: HasCodedValue x => View -> Link x -> View -> Link x -> IO ()
cloneLink view1 (Link location1) view2 (Link location2) =
   do
      oldVersionOpt <- getParentVersion view1
      oldVersion <- case oldVersionOpt of
         Nothing -> error "Link.cloneLink - used on virgin link."
         Just oldVersion -> return oldVersion

      isAlreadyDone <- 
         if location1 == location2 
            then
               do
                  thisVersionOpt <- getParentVersion view2
                  return (case thisVersionOpt of
                     Just thisVersion -> thisVersion == oldVersion
                     Nothing -> False
                     )
             else
                return False
      unless isAlreadyDone (
         transformValue (objects view2) location2 (\ objectDataOpt ->
            case objectDataOpt of
               Just _ -> error "Link.cloneLink - link already exists."
               Nothing -> return (Just (ClonedObject {
                  sourceLocation = location1,
                  sourceVersion = oldVersion
                  }),()
                  )
            )
         )
               
      
-- ----------------------------------------------------------------------
-- Versioned
-- ----------------------------------------------------------------------

data Versioned x = Versioned {
   location :: Location, -- Location in the view.
   statusMVar :: MVar (Status x)
   }

-- Make Versioned objects typeable (if x is).
versioned_tyRep = mkTyRep "View" "Versioned"

instance HasTyRep1 Versioned where
   tyRep1 _ = versioned_tyRep

mkObjectSourceFn :: HasCodedValue x => View 
   -> Versioned x -> Maybe (Location,ObjectVersion) -> ObjectVersion 
   -> IO (Maybe CommitChange)
-- This is the action that computes the ObjectSource to be committed to the
-- repository.
--
-- The supplied objectVersion is that of the containing view.
-- The Maybe (Location,ObjectVersion) indicates, if set, that this is
-- a cloned object, and so this should be used if the object is marked
-- as UpToDate.
mkObjectSourceFn (view@View{repository = repository})
      (Versioned {location = location,statusMVar = statusMVar}) 
      clonedOpt viewVersion =
   do
      status <- takeMVar statusMVar
      let 
         commitX x  =
            do
               xCodedValue <- doEncodeIO x view
               xObjectSource <- importICStringLen xCodedValue
               return (x,Just (Left xObjectSource))

      (x,objectSourceOpt) <- case status of
         Empty -> error "Attempt to commit Empty object!!!"
         UpToDate x -> return (x,case clonedOpt of
            Nothing -> Nothing
            Just (oldLocation,oldVersion) 
               -> Just (Right (oldLocation,oldVersion))
            )
         Cloned x oldLocation oldVersion 
            -> return (x,Just (Right (oldLocation,oldVersion)))
         Dirty x -> commitX x 
         Virgin x -> commitX x
      putMVar statusMVar (UpToDate x)
      return objectSourceOpt

data Status x = 
      Empty -- created by newEmptyObject
   |  UpToDate x -- This object committed and up-to-date
   |  Cloned x Location ObjectVersion
         -- This object 
   |  Dirty x -- This object committed, but since modified
   |  Virgin x -- Object never committed.

setTopLink :: HasCodedValue x => View -> x -> IO (Versioned x)
setTopLink view x = setLink view x (Link specialLocation2)

setLink :: HasCodedValue x => View -> x -> Link x -> IO (Versioned x)
setLink view x (Link location) = createObjectGeneral view (Virgin x) location

---
-- setOrGetTopLink is somewhat safer than setTopLink and initialises the
-- top object, if that hasn't already been done, via the supplied action.
-- Otherwise it (harmlessly) returns the existing object.
setOrGetTopLink :: HasCodedValue x => View -> IO x -> IO (Versioned x)
setOrGetTopLink (view@View{repository = repository,objects = objects}) action =
   do
      -- We delay using versionedAct doing things that don't need to be
      -- done inside transformValue, the reason being that then we
      -- can use fetchLink (which needs transformValue to have finished).
      versionedAct <- transformValue objects specialLocation2
         (\ objectDataOpt ->
            case objectDataOpt of
               Nothing ->
                  do
                     -- Not in repository, create.
                     x <- action
                     (versioned,objectData) <-
                         makeObjectData view (Virgin x) specialLocation2
                     return (Just objectData,return versioned)
               Just objectData ->
                do
                  -- Is in repository.  So we just return something
                  -- to get the topLink
                  return (Just objectData,fetchLink view topLink)
            )
      versionedAct   

createObject :: HasCodedValue x => View -> x -> IO (Versioned x)
createObject view x =
   do
      location <- newLocation (repository view)

      createObjectGeneral view (Virgin x) location

newEmptyObject :: HasCodedValue x => View -> IO (Versioned x)
newEmptyObject view = 
   do
      location <- newLocation (repository view)
      createObjectGeneral view Empty location

createObjectGeneral :: HasCodedValue x => View -> Status x -> Location 
   -> IO (Versioned x)
-- createObjectGeneral creates a completely new object for an already-
-- allocated location, given a Status (Virgin or Empty) to put in it.
createObjectGeneral view status location =
   do
      (versioned,objectData) <- makeObjectData view status location
      setValue (objects view) location objectData
      return versioned

---
-- This deletes an object from the View.
-- NB.  It is very important to make sure that the object has first
-- been deleted from anything which references it by link (EG folders)
-- or you will get the program crashing when someone tries to follow the
-- link.  
--
-- Links provided by the LinkManager (and I don't know of any others) can
-- be deleted using LinkManager.deleteLinkedObject (which also calls
-- deleteLink).
deleteLink :: HasCodedValue x => View -> Link x -> IO ()
deleteLink view (Link location) = 
   do
      debug ("Deleting " ++ show location)
      deleteFromRegistry (objects view) location

---
-- As with createObjectGeneral, create the versioned object and 
-- object data to put in the objects registry.  (All objects eventually
-- are created via this function.)
makeObjectData :: HasCodedValue x => View -> Status x -> Location 
   -> IO (Versioned x,ObjectData)
makeObjectData view (status :: Status x) location =
   do
      statusMVar <- newMVar status
      let versioned = Versioned {location = location,statusMVar = statusMVar}
      return (versioned,
         (PresentObject {
            thisVersioned = toDyn versioned,
            mkObjectSource = mkObjectSourceFn view versioned Nothing
            })) 


updateObject :: HasCodedValue x => View -> x -> Versioned x -> IO ()
updateObject view x (versioned@Versioned{statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar

      newStatus <- case status of
         Empty -> return (Virgin x)
         Virgin _ -> return (Virgin x)
         Cloned _ _ _ -> return (Virgin x)
         UpToDate _ -> return (Dirty x)
         Dirty _ -> return (Dirty x)
      putMVar statusMVar newStatus

dirtyObject :: HasCodedValue x => View -> Versioned x -> IO ()
dirtyObject view (versioned@Versioned {statusMVar = statusMVar}) =
   do
      status <- takeMVar statusMVar

      newStatus <- case status of
         UpToDate x  -> return (Dirty x)
         _ -> return status

      putMVar statusMVar newStatus

readObject :: HasCodedValue x => View -> Versioned x -> IO x
readObject view (versioned@Versioned{statusMVar = statusMVar} :: Versioned x) =
   do
      status <- readMVar statusMVar
      case status of
         Empty -> 
            error ("View.readObject on uninitialised object of type: "
               ++ show (typeOf (undefined :: x)))
         Virgin x -> return x
         Cloned x _ _ -> return x
         UpToDate x -> return x
         Dirty x -> return x

-- ----------------------------------------------------------------------
-- Access to the lastChange
-- ----------------------------------------------------------------------

getLastChange 
   :: HasCodedValue object => View -> Link object -> IO (Maybe ObjectVersion)
getLastChange view (Link location :: Link object) =
   do
      objectData <- getValueOpt (objects view) location
      let
         repositoryLastChange = 
            do
               (Just parentVersion) <- getParentVersion view
               lc <- lastChange (repository view) location parentVersion
               return (Just lc)

      case objectData of
         Nothing -> repositoryLastChange
         Just (PresentObject {thisVersioned = thisVersioned}) ->
            case fromDyn thisVersioned of
               (Just (versioned :: Versioned object)) ->
                  do
                     status <- readMVar (statusMVar versioned)
                     case status of
                        Empty -> return Nothing
                        Virgin _ -> return Nothing
                        Dirty _ -> return Nothing
                        Cloned _ _ _ -> return Nothing
                        UpToDate _ -> repositoryLastChange
         Just (ClonedObject _ _) -> return Nothing
