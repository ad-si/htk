-- | A VariantObject object contains a collection of things keyed by
-- an MMiSSSearchObject.   It also has a "current" MMiSSSearchObject
-- and a "cache" corresponding to this current object.
--
-- This cache must always contains something.  A consequence of that is that
-- all VariantObject objects contain at least one item.
module MMiSS.VariantObject(
   VariantObject,

   -- Creating VariantObject's
   newVariantObject,
      -- :: (object -> IO cache) -> object -> MMiSSSearchObject
      --    IO (VariantObject object cache)
      -- Create a new variant object with the given function for computing
      -- cache items.

   newEmptyVariantObject,
      -- :: (object -> IO cache)
      -- -> IO (VariantObject object cache)
      -- Create a new variant object with NO current element.
      --
      -- NB.  This function is dangerous because the cache is set to error.
      -- This means that if anyone tries to look at it, there will be an error.
      -- writeVariantObjectAndPoint should be used to rectify this undesirable
      -- situation

   newEmptyVariantObject1,
      -- :: (MMiSSVariantSpec -> object -> IO cache)
      -- -> IO (VariantObject object cache)
      -- Create a new variant object with NO current element.  This variant
      -- also allows the conversion function to take the VariantSpec of the
      -- variant actually found as an argument.
      --
      -- NB.  This function is dangerous because the cache is set to error.
      -- This means that if anyone tries to look at it, there will be an error.
      -- writeVariantObjectAndPoint should be used to rectify this undesirable
      -- situation


   -- Storing VariantObject's in the repository and retrieving them.
   FrozenVariantObject,
      -- instance of HasCodedValue provided "object" and "cache" are.
   freezeVariantObject,
      -- :: VariantObject object cache -> IO (FrozenVariantObject object cache)
   unfreezeVariantObject,
      -- :: (object -> IO cache) -> FrozenVariantObject object cache
      --    -> IO (VariantObject object cache)

   -- Accessing a VariantObject
   -- We provide one version of each function that uses the standard search
   -- algorithm, and one that uses exact search, insisting on an object which
   -- exactly matches that given search object.
   lookupVariantObject,
      -- :: VariantObject object cache -> MMiSSVariantSearch
      --    -> IO (Maybe object)

    -- Look up the object, and also return, if found, its MMiSSVariantSpec.
   lookupVariantObjectWithSpec,
      -- :: VariantObject object cache -> MMiSSVariantSearch
      --    -> IO (Maybe (object,MMiSSVariantSpec))

   lookupVariantObjectCache,
      -- :: VariantObject object cache -> MMiSSVariantSearch
      --    -> IO (Maybe cache)
      -- This does not always retrieve the current cache unless the search
      -- object is the same as the current one.  Otherwise it looks up the
      -- current object and computes a cache using the cache function.
   lookupVariantObjectCacheWithSpec,
      -- :: VariantObject object cache -> MMiSSVariantSearch
      -- -> IO (Maybe (cache,MMiSSVariantSpec))
      -- This also returns the actual variantSpec of the variant found.

   lookupVariantObjectExact,
      -- :: VariantObject object cache -> MMiSSVariantSpec
      --    -> IO (Maybe object)

   lookupVariantObjectCacheExact,
      -- :: VariantObject object cache -> MMiSSVariantSpec
      --    -> IO (Maybe cache)

   lookupVariantObjectAlmostExactWithSpec,
      -- :: VariantObject object cache
      -- -> MMiSSVariantSpec
      -- -> IO (Maybe (object,MMiSSVariantSpec))
      -- This does a search where it requires all but the version to
      -- be identical with that in the argument MMiSSVariantSpec.

   lookupVariantObjectAlmostExact,
      -- :: VariantObject object cache
      -- -> MMiSSVariantSpec
      -- -> IO (Maybe object)

   toVariantObjectCache,
      -- :: VariantObject object cache -> SimpleSource cache

   editMMiSSSearchObject, -- :: String -> VariantObject object cache -> IO Bool
      -- This gives the user a chance to change the current variant object,
      -- updating the cache as necessary.
      --
      -- The String should be the title of the object in question.
      --
      -- If we return False, that means no change was made.

   getCurrentVariantSearch,
      -- :: VariantObject object cache -> IO MMiSSVariantSearch
      -- Get the current variants, as a VariantSearch.

   writeVariantObject,
      -- :: VariantObject object cache -> MMiSSVariantSpec -> object
      -- -> IO ()
      -- Writing a new object

   pointVariantObject,
      -- :: VariantObject object cache -> MMiSSVariantSpec -> IO Bool
      -- Set the pointer of the variant object to the given MMiSSVariantSpec,
      -- which had better exist in the dictionary.  We return True if
      -- the pointer was in fact changed.


   pointVariantObjectAlwaysConvert,
      -- :: VariantObject object cache -> MMiSSVariantSpec -> IO ()
      -- Like pointVariantObject, but always run the converter.

   writeVariantObjectAndPoint,
      -- :: VariantObject object cache -> MMiSSVariantSpec
      -- -> object -> IO ()
      -- Writing a new object and make it current.

   -- Merging

   variantObjectObjectLinks,
      -- :: (object -> IO (ObjectLinks key)) -> VariantObject object cache
      -- -> IO ((ObjectLinks (MMiSSVariants,key)))

   attemptMergeVariantObject,
      -- :: (object -> IO cache) -> (View -> object -> IO object)
      -- -> [(View,VariantObject object cache)]
      -- -> IO (VariantObject object cache)
      --
      -- The first argument is the new converter, the second the function to
      -- be used to map old objects to new objects.

   variantObjectsSame,
      -- :: (object -> object -> Bool)
      -- -> VariantObject object cache -> VariantObject object cache -> IO Bool
      --
      -- Compare the contents of two variant objects (but not the pointer!)

   displayObjectVariants,
      -- :: VariantObject object cache -> IO ()

   copyVariantObject,
      -- :: (ObjectVersion -> IO VersionInfo) -> VariantObject object cache
      -- -> IO (VariantObject object cache)
      -- Used in MergeTypes.copyObject.

   ) where

import Data.Maybe

import Control.Concurrent.MVar
import System.IO.Unsafe(unsafeInterleaveIO)

import Util.Dynamics
import Util.Computation (done)
import Util.Sources
import Util.Broadcaster
import Util.Messages

import SimpleDB.VersionInfo

import Types.CodedValue
import Types.MergeTypes
import Types.ViewType

import MMiSS.Variant

-- -----------------------------------------------------------------------
-- The datatypes.
-- -----------------------------------------------------------------------

data VariantObject object cache = VariantObject {
   dictionary :: MMiSSVariantDict object,
   converter :: MMiSSVariantSpec -> object -> IO cache,
   currentVariantSpec :: MVar MMiSSVariantSpec,
      -- This is also used as a lock on editMMiSSSearchObject
   cache :: SimpleBroadcaster (Maybe cache)
      -- This has to be Nothing during initialisation
   }

-- | We basically need FrozenVariantObject because there isn\'t any way
-- of storing a function (converter) in the repository.
data FrozenVariantObject object cache = FrozenVariantObject {
   dictionary' :: MMiSSVariantDict object,
   currentVariantSpec' :: MMiSSVariantSpec
   } deriving (Typeable)

-- -----------------------------------------------------------------------
-- Creating VariantObject's
-- -----------------------------------------------------------------------

-- | Create a new variant object with the given function for computing
-- cache items.
newVariantObject :: (object -> IO cache) -> object -> MMiSSVariantSpec
   -> IO (VariantObject object cache)
newVariantObject converter object variantSpec =
   do
      dictionary <- newEmptyVariantDict
      addToVariantDict dictionary variantSpec object

      let
         frozenVariantObject = FrozenVariantObject {
            currentVariantSpec' = variantSpec,
            dictionary' = dictionary
            }

      unfreezeVariantObject (const converter) frozenVariantObject


-- | Create a new variant object with NO current element.
--
-- NB.  This function is dangerous because the cache is set to error.
-- This means that if anyone tries to look at it, there will be an error.
-- writeVariantObjectAndPoint should be used to rectify this undesirable
-- situation
newEmptyVariantObject :: (object -> IO cache)
   -> IO (VariantObject object cache)
newEmptyVariantObject converter = newEmptyVariantObject1 (const converter)

-- | Create a new variant object with NO current element.  This variant
-- also allows the conversion function to take the VariantSpec of the
-- variant actually found as an argument.
--
-- NB.  This function is dangerous because the cache is set to error.
-- This means that if anyone tries to look at it, there will be an error.
-- writeVariantObjectAndPoint should be used to rectify this undesirable
-- situation
newEmptyVariantObject1 :: (MMiSSVariantSpec -> object -> IO cache)
   -> IO (VariantObject object cache)
newEmptyVariantObject1 converter1 =
   do
      dictionary1 <- newEmptyVariantDict

      currentVariantSpec1 <- newMVar emptyMMiSSVariantSpec
      cache1 <- newSimpleBroadcaster Nothing

      let
         variantObject = VariantObject {
            dictionary = dictionary1,
            converter = converter1,
            currentVariantSpec = currentVariantSpec1,
            cache = cache1
            }

      return variantObject


-- | All VariantObject\'s are in fact created via this function.
unfreezeVariantObject :: (MMiSSVariantSpec -> object -> IO cache)
   -> FrozenVariantObject object cache -> IO (VariantObject object cache)
unfreezeVariantObject converter frozen =
   do
      let
         dictionary = dictionary' frozen
         spec = currentVariantSpec' frozen

      currentVariantSpec <- newMVar spec

      objectOpt <- variantDictSearch dictionary
         (fromMMiSSSpecToSearch spec)

      object <- case objectOpt of
         Just object -> return object
         Nothing -> error "unfreezeVariantObject failed"

      cache1 <- unsafeInterleaveIO (converter spec object)
         -- the cache won't be needed for copyVariantObject, for example,
         -- and as it requires getting the element it would be a pity to
         -- compute it unnecessarily.

      cache <- newSimpleBroadcaster (Just cache1)


      let
         variantObject = VariantObject {
            dictionary = dictionary,
            converter = converter,
            currentVariantSpec = currentVariantSpec,
            cache = cache
            }
      return variantObject

freezeVariantObject :: VariantObject object cache
   -> IO (FrozenVariantObject object cache)
freezeVariantObject variantObject =
   do
      let
         dictionary' = dictionary variantObject
      currentVariantSpec' <- readMVar (currentVariantSpec variantObject)
      let
         frozenVariantObject = FrozenVariantObject {
            dictionary' = dictionary',
            currentVariantSpec' = currentVariantSpec'
            }
      return frozenVariantObject

-- -----------------------------------------------------------------------
-- FrozenVariantObject's instance of HasCodedValue
-- -----------------------------------------------------------------------

instance HasBinary object CodingMonad
   => HasBinary (FrozenVariantObject object cache) CodingMonad where

   writeBin = mapWrite
      (\ (FrozenVariantObject {
         dictionary' = dictionary',
         currentVariantSpec' = currentVariantSpec'
         })
         ->
         (dictionary',currentVariantSpec')
         )


   readBin = mapRead
      (\ (dictionary',currentVariantSpec') ->
         (FrozenVariantObject {
            dictionary' = dictionary',
            currentVariantSpec' = currentVariantSpec'
            })
         )

-- -----------------------------------------------------------------------
-- Accessing a VariantObject
-- We provide one version of each function that uses the standard search
-- algorithm, and one that uses exact search, insisting on an object which
-- exactly matches that given search object.
-- We also provide lookupVariantObjectWithSpec, which the XML API uses.
-- (since we tell the user the variants of objects found with getObject).
-- -----------------------------------------------------------------------

lookupVariantObject:: VariantObject object cache -> MMiSSVariantSearch
   -> IO (Maybe object)
lookupVariantObject variantObject variantSearch =
   variantDictSearch (dictionary variantObject) variantSearch

lookupVariantObjectWithSpec :: VariantObject object cache
   -> MMiSSVariantSearch -> IO (Maybe (object,MMiSSVariantSpec))
lookupVariantObjectWithSpec variantObject variantSearch =
   variantDictSearchWithSpec (dictionary variantObject) variantSearch

lookupVariantObjectCache :: VariantObject object cache -> MMiSSVariantSearch
   -> IO (Maybe cache)
lookupVariantObjectCache variantObject variantSearch =
   do
      cacheSpecOpt
         <- lookupVariantObjectCacheWithSpec variantObject variantSearch
      return (fmap
         (\ (cache,_) -> cache)
         cacheSpecOpt
         )

lookupVariantObjectCacheWithSpec
   :: VariantObject object cache -> MMiSSVariantSearch
   -> IO (Maybe (cache,MMiSSVariantSpec))
lookupVariantObjectCacheWithSpec variantObject variantSearch =
   do
      objectOpt <- lookupVariantObjectWithSpec variantObject variantSearch
      case objectOpt of
         Nothing -> return Nothing
         Just (object,spec) ->
            do
               cache <- converter variantObject spec object
               return (Just (cache,spec))

lookupVariantObjectExact :: VariantObject object cache -> MMiSSVariantSpec
   -> IO (Maybe object)
lookupVariantObjectExact variantObject variantSpec =
   variantDictSearchExact (dictionary variantObject) variantSpec

lookupVariantObjectAlmostExact :: VariantObject object cache
   -> MMiSSVariantSpec
   -> IO (Maybe object)
lookupVariantObjectAlmostExact variantObject variantSpec =
   variantDictSearchAlmostExact (dictionary variantObject) variantSpec

lookupVariantObjectAlmostExactWithSpec :: VariantObject object cache
   -> MMiSSVariantSpec
   -> IO (Maybe (object,MMiSSVariantSpec))
lookupVariantObjectAlmostExactWithSpec variantObject variantSpec =
   variantDictSearchAlmostExactWithSpec (dictionary variantObject) variantSpec

lookupVariantObjectCacheExact :: VariantObject object cache
   -> MMiSSVariantSpec
   -> IO (Maybe cache)
lookupVariantObjectCacheExact variantObject variantSpec =
   do
      objectOpt <- lookupVariantObjectExact variantObject variantSpec
      case objectOpt of
         Nothing -> return Nothing
         Just object ->
            do
               cache <- converter variantObject variantSpec object
               return (Just cache)

toVariantObjectCache :: VariantObject object cache -> SimpleSource cache
toVariantObjectCache (variantObject :: VariantObject object cache) =
   let
      source1 :: SimpleSource (Maybe cache)
      source1 = toSimpleSource (cache variantObject)

      source2 :: SimpleSource cache
      source2 = fmap
         (fromMaybe (error
            "MMiSSVariantObject: attempt to read unitialised variant object"))
         source1
   in
      source2


-- -----------------------------------------------------------------------
-- Changing the VariantObject
-- -----------------------------------------------------------------------

-- | Writing a new object
writeVariantObject :: VariantObject object cache -> MMiSSVariantSpec -> object
   -> IO ()
writeVariantObject variantObject variantSpec object =
   -- We jump through various hoops to detect if the cache needs to be changed.
   modifyMVar_ (currentVariantSpec variantObject) (\ variantSpec1 ->
       do
          addToVariantDict (dictionary variantObject) variantSpec object
          if variantSpec == variantSpec1
             then
                do
                   cache1 <- converter variantObject variantSpec object
                   broadcast (cache variantObject) (Just cache1)
             else
                done
          return variantSpec1
       )

-- | Set the pointer of the variant object to the given MMiSSVariantSpec,
-- which had better exist in the dictionary.
pointVariantObject :: VariantObject object cache -> MMiSSVariantSpec -> IO Bool
pointVariantObject variantObject newVariantSpec =
   modifyMVar (currentVariantSpec variantObject) (\ oldVariantSpec ->
      if oldVariantSpec == newVariantSpec
         then
            do
               cacheOpt <- readContents (cache variantObject)
               case cacheOpt of
                  Nothing -> runConverter
                     -- Cache needs to be initialised anyway.
                  Just _ -> return (oldVariantSpec,False)
         else
            runConverter
      )
   where
      runConverter =
         do
            object <- getObject variantObject newVariantSpec
            newCache <- converter variantObject newVariantSpec object
            broadcast (cache variantObject) (Just newCache)
            return (newVariantSpec,True)

-- | Like pointVariantObject, but always run the converter.
pointVariantObjectAlwaysConvert
   :: VariantObject object cache -> MMiSSVariantSpec -> IO ()
pointVariantObjectAlwaysConvert variantObject newVariantSpec =
   modifyMVar_ (currentVariantSpec variantObject) (\ oldVariantSpec ->
      do
         object <- getObject variantObject newVariantSpec
         newCache <- converter variantObject newVariantSpec object
         broadcast (cache variantObject) (Just newCache)
         return newVariantSpec
      )

getObject :: VariantObject object cache -> MMiSSVariantSpec -> IO object
getObject variantObject variantSpec =
   do
      objectOpt <- variantDictSearchExact (dictionary variantObject)
         variantSpec
      let
         object = fromMaybe (error (
            "MMiSSVariantObject.getObject object"
            ++ " - point to non-existent variant.")) objectOpt
      return object

-- | Writing a new object and make it current.
writeVariantObjectAndPoint :: VariantObject object cache -> MMiSSVariantSpec
   -> object -> IO ()
writeVariantObjectAndPoint variantObject variantSpec object
      =
   modifyMVar_ (currentVariantSpec variantObject) (\ variantSpec1 ->
       do
          addToVariantDict (dictionary variantObject) variantSpec object
          cache1 <- converter variantObject variantSpec object
          broadcast (cache variantObject) (Just cache1)
          return variantSpec
       )

-- | This gives the user a chance to change the current variant object,
-- updating the cache as necessary.
--
-- The String should be the title of the object in question.
--
-- If we return False, that means no change was made.
editMMiSSSearchObject :: String -> VariantObject object cache -> IO Bool
editMMiSSSearchObject objectTitle variantObject =
   modifyMVar (currentVariantSpec variantObject)
      (\ variantSpec0 ->
         do
            variantsSpec1Opt <- editMMiSSVariantSpec variantSpec0
            case variantsSpec1Opt of
               Nothing -> return (variantSpec0,False)
               Just variantSpec1 ->
                  do
                     let
                        changed = (variantSpec1 /= variantSpec0)
                     if changed
                        then
                           do
                              cacheOpt <- lookupVariantObjectCache
                                 variantObject
                                 (fromMMiSSSpecToSearch variantSpec1)
                              case cacheOpt of
                                 Just cache1 ->
                                    do
                                       broadcast (cache variantObject)
                                          (Just cache1)
                                       return (variantSpec1,True)
                                 Nothing ->
                                    do
                                       errorMess
                                          "No matching variant found!"
                                       return (variantSpec0,False)
                        else
                           return (variantSpec0,False)
         )



-- | Get the current variants, as a VariantSearch.
getCurrentVariantSearch :: VariantObject object cache -> IO MMiSSVariantSearch
getCurrentVariantSearch variantObject =
   do
      variantSpec <- readMVar (currentVariantSpec variantObject)
      return (fromMMiSSSpecToSearch variantSpec)

-- -----------------------------------------------------------------------
-- Merging and copying.
-- -----------------------------------------------------------------------

variantObjectObjectLinks
   :: (object -> IO (ObjectLinks key)) -> VariantObject object cache
   -> IO ((ObjectLinks (MMiSSVariants,key)))
variantObjectObjectLinks getIndividualObjectLinks variantObject =
    getMMiSSVariantDictObjectLinks getIndividualObjectLinks
       (dictionary variantObject)

-- The first argument is the new converter, the second the function to
-- be used to map old objects to new objects.
attemptMergeVariantObject
   :: (MMiSSVariantSpec -> object -> IO cache)
   -> (View -> object -> IO object)
   -> [(View,VariantObject object cache)]
   -> IO (VariantObject object cache)
attemptMergeVariantObject newConverter convertObject variantObjects =
   do
      dictionary1 <- attemptMergeMMiSSVariantDict convertObject
         (map
            (\ (view,variantObject) -> (view,dictionary variantObject))
            variantObjects
            )
      let
         (_,headVariant):_ = variantObjects

      currentVariantSpec1 <- readMVar (currentVariantSpec headVariant)
      (Just object1) <- variantDictSearch dictionary1
         (fromMMiSSSpecToSearch currentVariantSpec1)

      let
         frozenVariantObject = FrozenVariantObject {
            dictionary' = dictionary1,
            currentVariantSpec' = currentVariantSpec1
            }

      unfreezeVariantObject newConverter frozenVariantObject

variantObjectsSame :: (object -> object -> Bool)
   -> VariantObject object cache -> VariantObject object cache -> IO Bool
variantObjectsSame testEq variantObject1 variantObject2 =
   variantDictsSame testEq
      (dictionary variantObject1) (dictionary variantObject2)


copyVariantObject
   :: (ObjectVersion -> IO VersionInfo) -> VariantObject object cache
   -> IO (VariantObject object cache)
copyVariantObject getNewVersionInfo variantObject0 =
   do
      let
         dictionary0 = dictionary variantObject0
      dictionary1 <- copyVariantDict getNewVersionInfo dictionary0
      let
         variantObject1 = variantObject0 {dictionary = dictionary1}
      return variantObject1

-- -----------------------------------------------------------------------
-- Displaying
-- -----------------------------------------------------------------------

displayObjectVariants :: VariantObject object cache -> IO ()
displayObjectVariants variantObject
   = displayMMiSSVariantDictKeys (dictionary variantObject)

-- -----------------------------------------------------------------------
-- HasGetAllVariants instance
-- -----------------------------------------------------------------------

instance HasGetAllVariants (VariantObject object cache) object where
   getAllVariants variantObject =
      getAllVariants (dictionary variantObject)
