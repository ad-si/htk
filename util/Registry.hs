{- A Registry is a mapping from ordered values.  For the Registry
   type itself, all target values have the same type.  For the
   UntypedRegistry type, the values
   can have any Typeable type.
   -}
module Registry(
   Registry, -- A "Registry from to" maps from values to to values.
   UntypedRegistry, -- An "UntypedRegistry from" maps from values to
                -- any Typeable values.
   NewRegistry(..),
   GetSetRegistry(..),
   DeleteFromRegistry(..),
      -- These classes describe access operations for registries.
   ) where

import IO

import FiniteMap
import Concurrent

import Dynamics

-- ----------------------------------------------------------------------
-- Classes, which describe the implementation.
-- ----------------------------------------------------------------------

class NewRegistry registry where
   newRegistry :: IO registry

class GetSetRegistry registry from to where
   getValue :: registry -> from -> IO to
      -- should raise an IO error if the value is not defined or 
      -- (for UntypedRegistry) has the wrong type.
   setValue :: registry -> from -> to -> IO ()

class DeleteFromRegistry registry from where
   deleteFromRegistry :: registry -> from -> IO ()

-- ----------------------------------------------------------------------
-- Typed registries
-- ----------------------------------------------------------------------

newtype Ord from => Registry from to = Registry (MVar (FiniteMap from to))

instance Ord from => NewRegistry (Registry from to) where
   newRegistry =
      do
         mVar <- newMVar emptyFM
         return (Registry mVar)

instance Ord from => GetSetRegistry (Registry from to) from to where
   getValue (Registry mVar) from =
      do
         map <- readMVar mVar
         case lookupFM map from of
            Nothing -> 
               ioError(userError "Registry.getValue - value not found")
            Just value -> return value
   setValue (Registry mVar) from to =
      do
         map <- takeMVar mVar
         putMVar mVar (addToFM map from to)


instance Ord from => DeleteFromRegistry (Registry from to) from where
   deleteFromRegistry (Registry mVar) from =
      do
         map <- takeMVar mVar
         putMVar mVar (delFromFM map from)

-- ----------------------------------------------------------------------
-- Untyped registries
-- ----------------------------------------------------------------------

newtype Ord from => UntypedRegistry from = UntypedRegistry (Registry from Dyn)

instance Ord from => NewRegistry (UntypedRegistry from) where
   newRegistry = 
      do
         registry <- newRegistry
         return (UntypedRegistry registry)

instance (Ord from,Typeable to) 
   => GetSetRegistry (UntypedRegistry from) from to where
   getValue (UntypedRegistry registry) from =
      do
         dyn <- getValue registry from
         case fromDyn dyn of
            Nothing ->
               ioError(userError "Registry.getValue - value of wrong type")
            Just value -> return value
   setValue (UntypedRegistry registry) from to =
      do
         let
            dyn = toDyn to
         setValue registry from dyn

instance Ord from => DeleteFromRegistry (UntypedRegistry from) from where
   deleteFromRegistry (UntypedRegistry registry) from =
      deleteFromRegistry registry from
