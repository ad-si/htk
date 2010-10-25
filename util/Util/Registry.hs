{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonoPatBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description: Store information by key.
--
-- A Registry is a mapping from ordered values.  For the Registry
-- type itself, all target values have the same type.  For the
-- UntypedRegistry type, the values
-- can have any Typeable type.
module Util.Registry(
   Registry, -- A "Registry from to" maps from values to to values.
   UntypedRegistry, -- An "UntypedRegistry from" maps from values to
                -- any Typeable values.
   LockedRegistry, -- A "LockedRegistry from to" is like a
      -- "Registry from to" but with finer locking.
   UntypedLockedRegistry, -- An "UntypedLockedRegistry from" is
      -- like an "UntypedRegistry from" but with finer locking.
   Untyped, -- Type constructor for registries with untyped contents.

   -- Unsafe/UnsafeRegistry are equivalent to Untyped/UntypedRegistry except
   -- for the additional functionality of causing a core-dump if misused,
   -- and not requiring Typeable.  THIS WILL GO IN GHC6.04
   Unsafe,
   UnsafeRegistry,

   NewRegistry(..),
   GetSetRegistry(..),
   GetSetRegistryDyn(..), -- direct access to dynamic values in
      -- Untyped's.
   KeyOpsRegistry(..),
      -- These classes describe access operations for registries.
   ListRegistryContents(..),
      -- extra block functions for typed registries.

   -- other specific operations
   changeKey,
      -- :: Ord from => Registry from to -> from -> from -> IO ()

   -- Operation for getting values directly from a Registry
   getRegistryValue,
      -- :: Ord from => Registry from to -> from -> IO to
      -- (This can be used to get a value without having to put
      -- a type annotation on it.)

   getValueDefault, -- :: ... => to -> registry -> from -> IO to

   lockedRegistryCheck, -- :: IO a -> IO (Either String a)
      -- For operations involving LockedRegistry's, catches the exception
      -- raised when we attempt to access a value inside a transformValue
      -- operation.


   getValue',
      -- Function to be used instead of getValue for debugging purposes.
   getValueSafe,
      -- alias for that (useful in combination with CPP).
   getRegistryValueSafe,
      -- :: Ord from => String -> Registry from to -> from -> IO to
      -- corresponds to getValueSafe and getRegistryValue
   ) where

import Data.Maybe

import Control.Monad.Trans
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent
import Control.Exception
import GHC.Prim(unsafeCoerce#)
   -- Ouch.  Will go with ghc6

import Util.ExtendedPrelude(newFallOut,mkBreakFn)
import Util.Dynamics
import Util.BinaryAll
import Util.CompileFlags
import Util.Object(ObjectID)


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


-- | ListRegistryContents will not be implemented for the untyped registries.
class ListRegistryContents registry from to where
   listRegistryContents :: registry from to -> IO [(from,to)]

   listRegistryContentsAndEmptyRegistry :: registry from to -> IO [(from,to)]
      -- ^ this is atomic.

   listToNewRegistry :: [(from,to)] -> IO (registry from to)

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
         return ()

   listKeys :: registry -> IO [from]

-- ----------------------------------------------------------------------
-- Typed registries
-- The locking here for transformValue is not so clever and just locks the
-- whole map while the fallback action runs.
-- ----------------------------------------------------------------------

newtype Ord from => Registry from to = Registry (MVar (Map.Map from to))
   deriving (Typeable)

instance Ord from => NewRegistry (Registry from to) where
   newRegistry =
      do
         mVar <- newMVar Map.empty
         return (Registry mVar)
   emptyRegistry (Registry mVar) =
      do
         takeMVar mVar
         putMVar mVar Map.empty

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
         return (Map.lookup from map)

   transformValue (Registry mVar) from transformer =
      modifyMVar mVar
         (\ map ->
            do
               (newSetting,extra) <- transformer (Map.lookup from map)
               newMap <- case newSetting of
                  Just newTo -> return (Map.insert from newTo map)
                  Nothing -> return (Map.delete from map)
               return (newMap,extra)
            )

   setValue (Registry mVar) from to =
      do
         map <- takeMVar mVar
         putMVar mVar (Map.insert from to map)


getRegistryValue :: Ord from => Registry from to -> from -> IO to
getRegistryValue registry from = getValue registry from


getRegistryValueSafe :: Ord from => String -> Registry from to -> from -> IO to
getRegistryValueSafe label registry from = getValueSafe label registry from

instance Ord from => KeyOpsRegistry (Registry from to) from where
   deleteFromRegistryBool (Registry mVar) from =
      do
         map <- takeMVar mVar
         if Map.member from map
            then
               do
                  putMVar mVar (Map.delete from map)
                  return True
            else
               do
                  putMVar mVar map
                  return False

   deleteFromRegistry (Registry mVar) from =
      do
         map <- takeMVar mVar
         putMVar mVar (Map.delete from map)
   listKeys (Registry mVar) =
      do
         map <- readMVar mVar
         return (Map.keys map)

instance Ord from => ListRegistryContents Registry from to where
   listRegistryContents (Registry mVar) =
      do
         fm <- readMVar mVar
         return (Map.toList fm)

   listRegistryContentsAndEmptyRegistry (Registry mVar) =
      modifyMVar mVar (\ fm ->
         return (Map.empty,Map.toList fm)
         )

   listToNewRegistry contents =
      do
         let map = Map.fromList contents
         mVar <- newMVar map
         return (Registry mVar)

-- | look up the element given by the first key, and if it exists
-- delete it, replacing it with the element given by the second key.
changeKey :: Ord from => Registry from to -> from -> from -> IO ()
changeKey (Registry mVar) oldKey newKey =
   modifyMVar_ mVar (\ fmap0 -> return (case Map.lookup oldKey fmap0 of
      Nothing -> fmap0
      Just elt ->
         let
            fmap1 = Map.delete oldKey fmap0
            fmap2 = Map.insert newKey elt fmap1
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

fromDynamicMessage :: Typeable to => String -> Dyn -> to
fromDynamicMessage fName dyn =
   case fromDynamic dyn of
      Just to -> to
      Nothing -> error ("Registry."++fName++" - value has wrong type")

instance (Typeable to,GetSetRegistry (registry from Dyn) from Dyn)
   => GetSetRegistry (Untyped registry from) from to where
   transformValue (Untyped registry) from transformer =
      do
         let
            valMapIn = fromDynamicMessage "transformValue"
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
toObj = unsafeCoerce#

fromObj :: Obj -> a
fromObj = unsafeCoerce#

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
--
-- They also catch cases where an locked registry function is used
-- inside a transformValue, and throw an appropriate exception, which
-- can be caught using lockedRegistryCheck.
-- ----------------------------------------------------------------------

newtype LockedRegistry from to
   = Locked (Registry from (MVar (Maybe to),Set.Set ThreadId))
   deriving (Typeable)

type UntypedLockedRegistry from = Untyped LockedRegistry from

instance Ord from => NewRegistry (LockedRegistry from to) where
   newRegistry =
      do
         registry <- newRegistry
         return (Locked registry)
   emptyRegistry (Locked registry) = emptyRegistry registry

-- utility functions transformValue will need
takeVal :: Ord from => LockedRegistry from to -> from -> IO (Maybe to)
takeVal (Locked registry) from =
   do
      mVar <-
         transformValue registry from
            (\ dataOpt ->
               do
                  threadId <- myThreadId
                  case dataOpt  of
                     Nothing ->
                        do
                           mVar <- newMVar Nothing
                           return (Just (mVar,Set.singleton threadId),mVar)
                     Just (mVar,set0) ->
                        if Set.member threadId set0
                           then -- error
                              mkBreakFn lockedFallOutId
                                 ("Circular transformValue detected in "
                                    ++ "Registry.LockedRegistry")
                           else
                              return (Just (mVar,Set.insert threadId set0),mVar)
               )
      takeMVar mVar


putVal :: Ord from => LockedRegistry from to -> from -> Maybe to -> IO ()
putVal (Locked registry) from toOpt =
   transformValue registry from
      (\ dataOpt ->
         do
            threadId <- myThreadId
            case dataOpt of
               Nothing -> error "Registry: unmatched putVal"
               Just (mVar,set0) ->
                  do
                     let
                        set1 = Set.delete threadId set0
                     if Set.null set1 && not (isJust toOpt)
                        then
                           return (Nothing,())
                        else
                           do
                              putMVar mVar toOpt
                              return (Just (mVar,set1),())
         )



lockedRegistryCheck :: IO a -> IO (Either String a)

lockedFallOutId :: ObjectID
(lockedFallOutId,lockedRegistryCheck) = lockedCheckBreak

lockedCheckBreak :: (ObjectID,IO a -> IO (Either String a))
lockedCheckBreak = unsafePerformIO newFallOut
{-# NOINLINE lockedCheckBreak #-}

instance Ord from => GetSetRegistry (LockedRegistry from to) from to where
   transformValue lockedRegistry from transformer =
      do
         valInOpt <- takeVal lockedRegistry from
         resultOrError <- Control.Exception.try (transformer valInOpt)
         case resultOrError of
            Left error ->
               do
                  putVal lockedRegistry from valInOpt
                  Control.Exception.throw (error :: SomeException)
            Right (valOutOpt,extra) ->
               do
                  putVal lockedRegistry from valOutOpt
                  return extra

instance Ord from => KeyOpsRegistry (LockedRegistry from to) from where
   deleteFromRegistryBool lockedRegistry from =
      do
         toOpt <- takeVal lockedRegistry from
         putVal lockedRegistry from Nothing
         return (isJust toOpt)
   listKeys (Locked registry) = listKeys registry


-- ----------------------------------------------------------------------
-- Function to be preferred to getValue when it is not absolutely certain
-- if a value is there, since it prints the label if things go wrong.
-- ----------------------------------------------------------------------

getValueSafe :: GetSetRegistry registry from to
   => String -> registry -> from -> IO to
getValueSafe = getValue'


getValue' :: GetSetRegistry registry from to
   => String -> registry -> from -> IO to
getValue' =
   if isDebug
      then
         (\ label registry from ->
            do
               toOpt <- getValueOpt registry from
               case toOpt of
                  Nothing -> error ("Registry.getValue' - failed with "
                     ++ label)
                  Just to -> return to
            )
      else
         (\ label -> getValue)



-- ----------------------------------------------------------------------
-- Instance of HasBinary for monads which have IO.
-- ----------------------------------------------------------------------

instance (HasBinary (from,to) m,Ord from,MonadIO m)
   => HasBinary (Registry from to) m where

   writeBin = mapWriteIO listRegistryContents
   readBin = mapReadIO listToNewRegistry
