{- A Registry is a mapping from ordered values.  For the Registry
   type itself, all target values have the same type.  For the
   UntypedRegistry type, the values
   can have any Typeable type.
   -}
module Registry(
   Registry, -- A "Registry from to" maps from values to to values.
   UntypedRegistry, -- An "UntypedRegistry from" maps from values to
                -- any Typeable values.
   LockedRegistry, -- A "LockedRegistry from to" is like a 
      -- "Registry from to" but with finer locking.
   UntypedLockedRegistry, -- An "UntypedLockedRegistry from" is
      -- like an "UntypedRegistry from" but with finer locking.
   Untyped, -- Type constructor for registries with untyped contents.

   NewRegistry(..),
   GetSetRegistry(..),
   GetSetRegistryDyn(..), -- direct access to dynamic values in
      -- Untyped's.
   KeyOpsRegistry(..),
      -- These classes describe access operations for registries.
   -- other specific operations
   listRegistryContents,
      -- :: Registry from to -> IO [(from,to)]
   listToNewRegistry,
      -- :: Ord from => [(from,to)] -> IO (Registry from to)
   changeKey,
      -- :: Ord from => Registry from to -> from -> from -> IO ()

   -- Operation for getting values directly from a Registry
   getRegistryValue,
      -- :: Ord from => Registry from to -> from -> IO to
      -- (This can be used to get a value without having to put
      -- a type annotation on it.)
   -- Operations which get and set the Dyn directly from an Untyped
   setValueAsDyn,
      -- :: registry -> from -> Dyn -> IO ()
   getValueAsDyn,
      -- :: registry -> from -> IO Dyn

   getValueDefault, -- :: ... => to -> registry -> from -> IO to


   -- Unsafe/UnsafeRegistry are equivalent to Untyped/UntypedRegistry except 
   -- for the additional functionality of causing a core-dump if misused,
   -- and not requiring Typeable.
   Unsafe,
   UnsafeRegistry,

   getValue', 
      -- Function to be used instead of getValue for debugging purposes.
   ) where

import IO
import Maybe
import Maybes

import qualified GlaExts(unsafeCoerce#)
import IOExts(newIORef,readIORef,writeIORef)
import FiniteMap
import Concurrent

import Computation(done)
import Dynamics


-- ----------------------------------------------------------------------
-- Classes, which describe the implementation.
-- ----------------------------------------------------------------------

class NewRegistry registry where
   newRegistry :: IO registry
   emptyRegistry :: registry -> IO ()

class GetSetRegistry registry from to where
   transformValue :: registry -> from -> (Maybe to -> IO (Maybe to,extra)) 
      -> IO extra
      -- transform a value, where "Nothing" means "value is not in
      -- the registry.  Locking is important, but depends on the
      -- implementation.
      -- Only this function has to be defined.
   getValueOpt :: registry -> from -> IO (Maybe to)
      -- returns Nothing if the value is not defined or
      -- has the wrong type.
   getValueOpt registry from = transformValue registry from
      (\ valueOpt -> return (valueOpt,valueOpt))

   getValue :: registry -> from -> IO to
      -- should raise an IO error if the value is not defined or- 
      -- (for Untyped) has the wrong type.
   getValue registry from =
      do
         valueOpt <- getValueOpt registry from
         case valueOpt of
            Nothing -> error "Registry.getValue  - value undefined"
            Just value -> return value
   
   setValue :: registry -> from -> to -> IO ()
   setValue registry from to =
      transformValue registry from (\ _ -> return (Just to,()))

getValueDefault :: GetSetRegistry registry from to 
   => to -> registry -> from -> IO to
getValueDefault defTo registry from =
   do
      toOpt <- getValueOpt registry from
      case toOpt of
         Nothing -> return defTo
         Just to -> return to 

class KeyOpsRegistry registry from where
   deleteFromRegistryBool :: registry -> from -> IO Bool
   -- deleteFromRegistryBool returns True if the element was in
   -- the registry and deletes it, otherwise False (and does nothing).
   deleteFromRegistry :: registry -> from -> IO ()
   -- This should fail silently if the key does not
   -- exist in the map.
   deleteFromRegistry registry from =
      do
         deleteFromRegistryBool registry from
         done

   listKeys :: registry -> IO [from]

-- ----------------------------------------------------------------------
-- Typed registries
-- The locking here for transformValue is not so clever and just locks the 
-- whole map while the fallback action runs.
-- ----------------------------------------------------------------------

newtype Ord from => Registry from to = Registry (MVar (FiniteMap from to))

instance Ord from => NewRegistry (Registry from to) where
   newRegistry =
      do
         mVar <- newMVar emptyFM
         return (Registry mVar)
   emptyRegistry (Registry mVar) =
      do
         takeMVar mVar
         putMVar mVar emptyFM

instance Ord from => GetSetRegistry (Registry from to) from to where
   getValue registry from =
      do
         valueOpt <- getValueOpt registry from
         case valueOpt of
            Nothing -> 
               ioError(userError "Registry.getValue - value not found")
            Just value -> return value

   getValueOpt (Registry mVar) from =
      do
         map <- readMVar mVar
         return (lookupFM map from)

   transformValue (Registry mVar) from transformer =
      do
         map <- takeMVar mVar
         (newSetting,extra) <- transformer (lookupFM map from)
         newMap <- case newSetting of
            Just newTo -> return (addToFM map from newTo)
            Nothing -> return (delFromFM map from)
         putMVar mVar newMap
         return extra

   setValue (Registry mVar) from to =
      do
         map <- takeMVar mVar
         putMVar mVar (addToFM map from to)


getRegistryValue :: Ord from => Registry from to -> from -> IO to
getRegistryValue registry from = getValue registry from

instance Ord from => KeyOpsRegistry (Registry from to) from where
   deleteFromRegistryBool (Registry mVar) from =
      do
         map <- takeMVar mVar
         if elemFM from map 
            then
               do
                  putMVar mVar (delFromFM map from)
                  return True
            else
               do
                  putMVar mVar map
                  return False

   deleteFromRegistry (Registry mVar) from =
      do
         map <- takeMVar mVar
         putMVar mVar (delFromFM map from)
   listKeys (Registry mVar) =
      do
         map <- readMVar mVar
         return (keysFM map)

listRegistryContents :: Ord from => Registry from to -> IO [(from,to)]
listRegistryContents (Registry mVar) =
   do
      map <- readMVar mVar
      return (fmToList map)

listToNewRegistry :: Ord from => [(from,to)] -> IO (Registry from to)
listToNewRegistry contents =
   do
      let map = listToFM contents
      mVar <- newMVar map
      return (Registry mVar)

-- | look up the element given by the first key, and if it exists
-- delete it, replacing it with the element given by the second key.
changeKey :: Ord from => Registry from to -> from -> from -> IO ()
changeKey (Registry mVar) oldKey newKey =
   modifyMVar_ mVar (\ fmap0 -> return (case lookupFM fmap0 oldKey of
      Nothing -> fmap0
      Just elt ->
         let
            fmap1 = delFromFM fmap0 oldKey
            fmap2 = addToFM fmap1 newKey elt
         in
            fmap2
         )
      )

-- ----------------------------------------------------------------------
-- Untyped Registries
-- ----------------------------------------------------------------------

-- We abbreviate a common case:
type UntypedRegistry from = Untyped Registry from

newtype Untyped registry from = Untyped (registry from Dyn)

instance NewRegistry (registry from Dyn) 
   => NewRegistry (Untyped registry from) where
   newRegistry =
      do
         registry <- newRegistry
         return (Untyped registry)
   emptyRegistry (Untyped registry) = emptyRegistry registry

fromDynMessage :: Typeable to => String -> Dyn -> to
fromDynMessage fName dyn =
   case fromDyn dyn of
      Just to -> to
      Nothing -> error ("Registry."++fName++" - value has wrong type")

instance (Typeable to,GetSetRegistry (registry from Dyn) from Dyn) 
   => GetSetRegistry (Untyped registry from) from to where
   transformValue (Untyped registry) from transformer =
      do
         let
            valMapIn = fromDynMessage "transformValue"
            valMapOut val = toDyn val
            transformerDyn dynInOpt =
               do
                  let valInOpt = (fmap valMapIn) dynInOpt
                  (valOutOpt,extra) <- transformer valInOpt
                  let dynOutOpt = (fmap valMapOut) valOutOpt
                  return (dynOutOpt,extra)
         transformValue registry from transformerDyn 

instance KeyOpsRegistry (registry from Dyn) from 
   => KeyOpsRegistry (Untyped registry from) from where
   deleteFromRegistryBool (Untyped registry) from =
      deleteFromRegistryBool registry from
   deleteFromRegistry (Untyped registry) from =
      deleteFromRegistry registry from
   listKeys (Untyped registry) = listKeys registry

-- We also provide direct setting/unsetting of Dyn values.
class GetSetRegistryDyn registry from where
   setValueAsDyn :: registry -> from -> Dyn -> IO ()
   getValueAsDyn :: registry -> from -> IO Dyn

instance GetSetRegistry (registry from Dyn) from Dyn
   => GetSetRegistryDyn (Untyped registry from) from where

   setValueAsDyn (Untyped registry) from dyn =
      setValue registry from dyn
   getValueAsDyn (Untyped registry) from =
      getValue registry from

-- ----------------------------------------------------------------------
-- Unsafe Registries
-- To be used only in dire emergency where GHC's obscure multi-parameter
-- type rules aren't able to infer Typeable, these will cause core dumps
-- if the types are wrong.
-- ----------------------------------------------------------------------

type UnsafeRegistry from = Unsafe Registry from

data Obj = Obj
 -- to hold the value, which may be of any type.

toObj :: a -> Obj
toObj = GlaExts.unsafeCoerce#

fromObj :: Obj -> a
fromObj = GlaExts.unsafeCoerce#

newtype Unsafe registry from = Unsafe (registry from Obj)

instance NewRegistry (registry from Obj) 
   => NewRegistry (Unsafe registry from) where
   newRegistry =
      do
         registry <- newRegistry
         return (Unsafe registry)
   emptyRegistry (Unsafe registry) = emptyRegistry registry

instance (GetSetRegistry (registry from Obj) from Obj) 
   => GetSetRegistry (Unsafe registry from) from to where
   transformValue (Unsafe registry) from transformer =
      do
         let
            transformerObj objInOpt =
               do
                  let valInOpt = (fmap fromObj) objInOpt
                  (valOutOpt,extra) <- transformer valInOpt
                  let objOutOpt = (fmap toObj) valOutOpt
                  return (objOutOpt,extra)
         transformValue registry from transformerObj 

instance KeyOpsRegistry (registry from Obj) from 
   => KeyOpsRegistry (Unsafe registry from) from where
   deleteFromRegistryBool (Unsafe registry) from =
      deleteFromRegistryBool registry from
   deleteFromRegistry (Unsafe registry) from =
      deleteFromRegistry registry from
   listKeys (Unsafe registry) = listKeys registry

-- ----------------------------------------------------------------------
-- Locked registries.  These improve on the previous model in
-- that transformValue actions do not lock the whole registry,
-- but only the key whose value is being transformed.
-- NB - there is a subtle concurrency problem with deleting from a locked
-- registry.  If this happens at the same time as we attempt to transform a
-- value, the delete action can return, while the transform action is still
-- going on.
-- ----------------------------------------------------------------------

newtype LockedRegistry from to = Locked (Registry from (MVar to))

type UntypedLockedRegistry from = Untyped LockedRegistry from

instance Ord from => NewRegistry (LockedRegistry from to) where
   newRegistry =
      do
         registry <- newRegistry
         return (Locked registry)
   emptyRegistry (Locked registry) = emptyRegistry registry

instance Ord from => GetSetRegistry (LockedRegistry from to) from to where
   transformValue (Locked registry) from transformer =
      do
         (mVar,isNew) <-
            transformValue registry from
               (\ mVarOpt -> case mVarOpt of
                  Nothing -> 
                     do
                        mVar <- newEmptyMVar
                        return (Just mVar,(mVar,True))
                  Just mVar -> return (Just mVar,(mVar,False))
                  )
         valInOpt <- if isNew then return Nothing else 
            do
               valIn <- takeMVar mVar
               return (Just valIn)
         (valOutOpt,extra) <- transformer valInOpt
         case valOutOpt of
            Just valOut -> putMVar mVar valOut
            Nothing ->
               do
                  case valInOpt of
                     Just valIn -> putMVar mVar valIn
                     Nothing -> done
                  deleteFromRegistry registry from 
         return extra 

instance Ord from => KeyOpsRegistry (LockedRegistry from to) from where
   deleteFromRegistryBool (Locked registry) from =
      transformValue registry from
         (\ (mVarOpt :: Maybe (MVar to)) -> return (Nothing,isJust mVarOpt))
   listKeys (Locked registry) = listKeys registry

-- ----------------------------------------------------------------------
-- Typeable instances
-- We make Registry and LockedRegistry instances of Typeable.
-- The others don't need to be since they are type synonyms, not
-- type constructors.
-- ----------------------------------------------------------------------

registry_tyRep = mkTyRep "Registry" "Registry"
instance HasTyRep2 Registry where
   tyRep2 _ = registry_tyRep

lockedRegistry_tyRep = mkTyRep "Registry" "LockedRegistry"
instance HasTyRep2 LockedRegistry where
   tyRep2 _ = lockedRegistry_tyRep

-- ----------------------------------------------------------------------
-- Function to be preferred to getValue when it is not absolutely certain
-- if a value is there, since it prints the label if things go wrong.
-- ----------------------------------------------------------------------

getValue' :: GetSetRegistry registry from to 
   => String -> registry -> from -> IO to

#ifdef DEBUG

getValue' label registry from =
   do
      toOpt <- getValueOpt registry from
      case toOpt of
         Nothing -> error ("Registry.getValue' - failed with "++label)
         Just to -> return to

#else

getValue' label = getValue

#endif