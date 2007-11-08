-- | A wrapper for the new GHC (and Hugs) Dynamic module.
-- The main improvement over the original Dynamic module is
-- that we provide flavours of TypeableXXXX for kinds with
-- arguments other than *, a feature used by "DisplayView".
module Dynamics (
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
        dynCastOpt,

        mkTypeRep,
           -- :: String -> String -> TypeRep

        -- Flavours of Typeable we need not already in Data.Typeable.
        -- The only customer for these at the moment seems to be
        -- types/DisplayView.hs
        Typeable1_1(..),
        Typeable2_11(..),
        Typeable3_111(..),
        Typeable4_0111(..),
        Typeable5_00111(..),
        Typeable6_000111(..),
        )
where

import qualified Data.Dynamic
import Data.Typeable

import Computation
import Debug(debug)

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
dynCastOpt = Data.Dynamic.cast

-- | Construct a TypeRep for a type or type constructor with no arguments.
-- The first string should be the module name, the second that of the type.
mkTypeRep :: String -> String -> TypeRep
mkTypeRep s1 s2 = mkTyConApp (mkTyCon (s1 ++ "." ++ s2)) []

-- ------------------------------------------------------------
-- Flavours of Typeable we need not already in Data.Typeable.
-- The only customer for these at the moment seems to be
-- types/DisplayView.hs
-- ------------------------------------------------------------

class Typeable1_1 ty where
   typeOf1_1 :: Typeable1 typeArg => ty typeArg -> TypeRep

instance (Typeable1_1 ty,Typeable1 typeArg) => Typeable (ty typeArg) where
   typeOf (x :: ty typeArg) = (typeOf1_1 x) `mkAppTy` typeOf v
      where
         v :: typeArg ()
         v = error "Dynamics.31"

class Typeable2_11 ty where
   typeOf2_11 :: (Typeable1 typeArg1,Typeable1 typeArg2)
      => ty typeArg1 typeArg2 -> TypeRep

instance (Typeable2_11 ty,Typeable1 typeArg1)
      => Typeable1_1 (ty typeArg1) where
   typeOf1_1 (x :: ty typeArg1 typeArg2) =
         (typeOf2_11 x) `mkAppTy` (typeOf1 v)
      where
         v :: typeArg1 ()
         v = error "Dynamics.23"

class Typeable3_111 ty where
   typeOf3_111 :: (Typeable1 typeArg1,Typeable1 typeArg2,Typeable1 typeArg3)
      => ty typeArg1 typeArg2 typeArg3 -> TypeRep

instance (Typeable3_111 ty,Typeable1 typeArg1)
      => Typeable2_11 (ty typeArg1) where
   typeOf2_11 (x :: ty typeArg1 typeArg2 typeArg3) =
         (typeOf3_111 x) `mkAppTy` (typeOf1 v)
      where
         v :: typeArg1 ()
         v = error "Dynamics.23"

class Typeable4_0111 ty where
   typeOf4_0111
      :: (Typeable ty1,
         Typeable1 typeArg1,Typeable1 typeArg2,Typeable1 typeArg3)
      => ty ty1 typeArg1 typeArg2 typeArg3  -> TypeRep

instance (Typeable4_0111 ty,Typeable ty1)
      => Typeable3_111 (ty ty1) where
   typeOf3_111 (x :: ty ty1 typeArg2 typeArg3 typeArg4) =
         (typeOf4_0111 x) `mkAppTy` (typeOf v)
      where
         v :: ty1
         v = error "Dynamics.23"

class Typeable5_00111 ty where
   typeOf5_00111
      :: (Typeable ty1,Typeable ty2,
         Typeable1 typeArg1,Typeable1 typeArg2,Typeable1 typeArg3)
      => ty ty1 ty2 typeArg1 typeArg2 typeArg3  -> TypeRep

instance (Typeable5_00111 ty,Typeable ty1)
      => Typeable4_0111 (ty ty1) where
   typeOf4_0111 (x :: ty ty1 ty2 typeArg1 typeArg2 typeArg3) =
         (typeOf5_00111 x) `mkAppTy` (typeOf v)
      where
         v :: ty1
         v = error "Dynamics.23"

class Typeable6_000111 ty where
   typeOf6_000111
      :: (Typeable ty1,Typeable ty2,Typeable ty3,
         Typeable1 typeArg1,Typeable1 typeArg2,Typeable1 typeArg3)
      => ty ty1 ty2 ty3 typeArg1 typeArg2 typeArg3  -> TypeRep

instance (Typeable6_000111 ty,Typeable ty1)
      => Typeable5_00111 (ty ty1) where
   typeOf5_00111 (x :: ty ty1 ty2 ty3 typeArg1 typeArg2 typeArg3) =
         (typeOf6_000111 x) `mkAppTy` (typeOf v)
      where
         v :: ty1
         v = error "Dynamics.23"

