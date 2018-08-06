{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A wrapper for the new GHC (and Hugs) Dynamic module.
-- The main improvement over the original Dynamic module is
-- that we provide flavours of TypeableXXXX for kinds with
-- arguments other than *, a feature used by "DisplayView".
module Util.Dynamics (
        Typeable(..), -- inherited from Dynamic
        TypeRep, -- same as Dynamic.TypeRep


        Dyn, -- equal to Dynamic.Dynamic
        toDyn, -- inherited from Dynamic.toDyn
        fromDynamic, -- inherited from Dynamic.fromDynamic
        fromDynamicWE, -- :: Dyn -> WithError a

        coerce, -- read Dyn or (match) error
        coerceIO, -- read Dyn or fail with typeMismatch
        typeMismatch,
        dynCast, -- Cast to another value of the same type, or
           -- error (useful for extracting from existential types).
        dynCastOpt
        )
where

import qualified Data.Dynamic
import Data.Typeable

import Util.Computation
import Util.Debug(debug)

fromDynamic :: Typeable a => Dyn -> Maybe a
fromDynamic = Data.Dynamic.fromDynamic

-- | Like 'fromDynamic' but provides an error message indicating what
-- types are getting confused.
fromDynamicWE :: Typeable a => Dyn -> WithError a
fromDynamicWE dyn =
   case fromDynamic dyn of
      Just a -> return a
      (aOpt @ Nothing) ->
         fail ("Dynamic type error.  Looking for "
            ++ show (typeOf (typeHack aOpt))
            ++ " but found a " ++ show dyn)
   where
      typeHack :: Maybe a -> a
      typeHack _ = undefined
type Dyn = Data.Dynamic.Dynamic

toDyn :: Typeable a => a -> Dyn
toDyn = Data.Dynamic.toDyn

coerce  :: Typeable a => Dyn -> a
coerce d =
   case fromDynamic d of
      Just x -> x

coerceIO :: Typeable a => Dyn -> IO a
coerceIO d =
   case fromDynamic d of
      Nothing ->
         do
            debug "Dynamics.coerceIO failure"
            ioError typeMismatch
      (Just x) -> return x

typeMismatch :: IOError
typeMismatch =
        userError "internal type of dynamics does not match expected type"

dynCast :: (Typeable a,Typeable b) => String -> a -> b
dynCast mess value = case dynCastOpt value of
   Nothing -> error ("Dynamics.dynCast failure in "++mess)
   Just value2 -> value2

dynCastOpt :: (Typeable a,Typeable b) => a -> Maybe b
dynCastOpt = cast
