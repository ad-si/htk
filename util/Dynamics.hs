{- #########################################################################

MODULE        : Dynamics
AUTHOR        : George Russell
DATE          : August 1999
VERSION       : ??
DESCRIPTION   : A wrapper for the new GHC (and Hugs) Dynamic module,
                supposed to look like Dynamics in the old
                Uniform.  In time it might be nice to get rid of
                this module altogether

                Changes!  The first attempt didn't work.
                GHC's Dynamic structure compares type constructors
                by number, and the number is assigned at the time of
                creation (unsafely).  So we need to create a separate
                type for type constructors and insist they are
                created at a specific point of the file.

   ######################################################################### -}

-- NB.  Unlike in the original implementation, we cannot Read
-- TypeTags.  Time will tell if this is a problem.
-- PDyns and coerceUnsafe function have been removed - neither
-- seem to be used.
module Dynamics (
        Typeable(..), -- inherited from Dynamic
        TypeRep, -- same as Dynamic.TypeRep
        TyRep, -- pre-form of TypeRep

        mkTyRep, -- :: String -> String -> TyRep
        -- create a type constructor "name".  This should be done once
        -- per constructor, at the top level.  The first string is th
        -- module name, the second the type constructor name.

        Dyn, -- equal to Dynamic.Dynamic
        toDyn, -- inherited from Dynamic.toDyn
        fromDyn, -- NOT inherited from Dynamic.fromDyn

        coerce, -- read Dyn or (match) error
        coerceIO, -- read Dyn or fail with typeMismatch
        typeMismatch,
        dynCast, -- Cast to another value of the same type, or
           -- error (useful for extracting from existential types). 
        -- The HasTyRep* classes are abbreviations for constructing
        -- instances of Typeable. 
        HasTyRep(..),
        HasTyRep1(..),
        HasTyRep2(..),
        HasTyRep3(..),
        HasTyRep4(..),
        HasTyRep1_1(..),
        HasTyRep2_11(..),
        HasTyRep3_011(..),
        HasTyRep4_0011(..),
        ) 
where

import qualified Dynamic
import Dynamic(Typeable(..),TypeRep)
import Debug(debug)

#if (__GLASGOW_HASKELL__ >= 503)

import GHC.Dynamic(Dynamic(..))

#else
import PrelDynamic(Dynamic(..))
#endif

import qualified GlaExts

type Dyn = Dynamic.Dynamic

badTyRep = error "Tyrep accessed"

fromDyn :: Typeable a => Dyn -> Maybe a
fromDyn (Dynamic _ o) = Just (GlaExts.unsafeCoerce# o)

toDyn :: Typeable a => a -> Dyn
toDyn d = Dynamic badTyRep (GlaExts.unsafeCoerce# d)

type TypeTag = Dynamic.TypeRep
type TyCon = Dynamic.TyCon

mkTyRep :: String -> String -> TyRep
mkTyRep mname tname = TyRep (Dynamic.mkTyCon (mname ++ "." ++ tname)) []

coerce  :: Typeable a => Dyn -> a
coerce d = 
   case fromDyn d of 
      Just x -> x

coerceIO :: Typeable a => Dyn -> IO a
coerceIO d = 
   case fromDyn d of
      Nothing ->
         do 
            debug "Dynamics.coerceIO failure"
            ioError typeMismatch
      (Just x) -> return x

typeMismatch :: IOError
typeMismatch = 
        userError "internal type of dynamics does not match expected type"

dynCast :: (Typeable a,Typeable b) => String -> a -> b
dynCast mess value = case fromDyn (toDyn value) of
   Nothing -> error ("Dynamics.dynCast failure in "++mess)
   Just value2 -> value2

------------------------------------------------------------------------
-- The HasTyRep* classes are used to indicate that
-- a type constructor produces typeable values
-- We organise them in a cunning way to avoid overlapping type classes;
-- the cunning was Simon Peyton Jones'.
------------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- HasTyRep for constructors of kind 0
-- ----------------------------------------------------------------------


---
-- A TyCon with arguments.
data TyRep = TyRep TyCon [TypeRep]

appTyRep :: TyRep -> TypeRep -> TyRep
appTyRep (TyRep tyCon typeReps) typeRep = TyRep tyCon (typeRep:typeReps)

toTypeRep :: TyRep -> TypeRep
toTypeRep (TyRep tyCon typeReps) = Dynamic.mkAppTy tyCon (reverse typeReps)

class HasTyRep ty where
   tyRep :: ty -> TyRep

instance HasTyRep ty => Typeable ty where
   typeOf _ =
      let
         (tC :: ty) = error "Dynamics.1"
      in
         toTypeRep (tyRep tC)

-- ----------------------------------------------------------------------
-- TyRep's for types of kind more than zero with arguments of kind zero.
-- ----------------------------------------------------------------------

class HasTyRep1 ty where
   tyRep1 :: ty value -> TyRep

instance (HasTyRep1 ty,Typeable value) => HasTyRep (ty value) where
   tyRep _ =
      let
         (tC :: ty value) = error "Dynamics.2"
         (v :: value) = error "Dynamics.3"
      in
         appTyRep (tyRep tC) (typeOf v)

class HasTyRep2 ty where
   tyRep2 :: ty value1 value2 -> TyRep

instance (HasTyRep2 ty,Typeable value1) => HasTyRep1 (ty value1) where
   tyRep1 _ =
      let
         (tC :: ty value1 ()) = error "Dynamics.4"
         (v :: value1) = error "Dynamics.3"
      in
         appTyRep (tyRep2 tC) (typeOf v)

class HasTyRep3 ty where
   tyRep3 :: ty value1 value2 value3 -> TyRep

instance (HasTyRep3 ty,Typeable value1) => HasTyRep2 (ty value1) where
   tyRep2 _ =
      let
         (tC :: ty value1 () ()) = error "Dynamics.4"
         (v :: value1) = error "Dynamics.5"
      in
         appTyRep (tyRep3 tC) (typeOf v)
         
class HasTyRep4 ty where
   tyRep4 :: ty value1 value2 value3 value4 -> TyRep

instance (HasTyRep4 ty,Typeable value1) => HasTyRep3 (ty value1) where
   tyRep3 _ =
      let
         (tC :: ty value1 () () ()) = error "Dynamics.6"
         (v :: value1) = error "Dynamics.7"
      in
         appTyRep (tyRep4 tC) (typeOf v)

-- ------------------------------------------------------------
-- Some instances of TyRep for type arguments of non-zero kind.
-- We need a versions of HasTyRep's for different kinds.
-- ------------------------------------------------------------

class HasTyRep1_1 ty where
   tyRep1_1 :: HasTyRep1 typeArg => ty typeArg -> TyRep

instance (HasTyRep1_1 ty,HasTyRep1 typeArg) => HasTyRep (ty typeArg) where
   tyRep _ =
      let
         (tC :: ty typeArg) = error "Dynamics.30"
         (v :: typeArg ()) = error "Dynamics.31"
      in
          appTyRep (tyRep1_1 tC) (toTypeRep (tyRep1 v))

class HasTyRep2_11 ty where
   tyRep2_11 :: (HasTyRep1 typeArg1,HasTyRep1 typeArg2) =>
      ty typeArg1 typeArg2 -> TyRep

instance (HasTyRep2_11 ty,HasTyRep1 typeArg) => HasTyRep1_1 (ty typeArg) where
   tyRep1_1 _ =
      let
         (tC :: ty typeArg Dummy) = error "Dynamics.22"
         (v :: typeArg ()) = error "Dynamics.23"
      in
          appTyRep (tyRep2_11 tC) (toTypeRep (tyRep1 v))


class HasTyRep3_011 ty where
   tyRep3_011 :: (HasTyRep1 typeArg1,HasTyRep1 typeArg2) =>
      ty value1 typeArg1 typeArg2 -> TyRep

instance (HasTyRep3_011 ty,Typeable value) => HasTyRep2_11 (ty value) where
   tyRep2_11 _ =
      let
         (tC :: ty value Dummy Dummy) = error "Dynamics.24"
         (v :: value) = error "Dynamics.25"
      in
         appTyRep (tyRep3_011 tC) (typeOf v)

class HasTyRep4_0011 ty where
   tyRep4_0011 :: (HasTyRep1 typeArg1,HasTyRep1 typeArg2) =>
      ty value1 value2 typeArg1 typeArg2 -> TyRep

instance (HasTyRep4_0011 ty,Typeable value1) => HasTyRep3_011 (ty value1) where
   tyRep3_011 _ =
      let
         (tC :: ty value1 () Dummy Dummy) = error "Dynamics.26"
         (v :: value1) = error "Dynamics.27"
      in
         appTyRep (tyRep4_0011 tC) (typeOf v)

data Dummy x = Dummy x

dummy_tyRep = mkTyRep "Dynamics" "Dummy"
instance (HasTyRep1 Dummy) where
   tyRep1 _ = dummy_tyRep

