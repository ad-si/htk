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
        TypeTag, -- equal to Dynamic.TypeRep

        mkTyCon, -- :: String -> String -> TyCon
        -- create a type constructor "name".  This should be done once
        -- per constructor, at the top level.  The first string is th
        -- module name, the second the type constructor name.
        TyCon, -- type constructor name.  Equals Dynamic.TyCon
        mkTypeTag, -- :: TyConName -> [TypeTag] -> TypeTag
        -- create a type tag for TyCon (t1,t2,...,tn), n>=0.

        Dyn, -- equal to Dynamic.Dynamic
        toDyn, -- inherited from Dynamic.toDyn
        fromDyn, -- NOT inherited from Dynamic.fromDyn

        coerce, -- read Dyn or (match) error
        coerceIO, -- read Dyn or fail with typeMismatch
        typeMismatch,
        dynCast, -- Cast to another value of the same type, or
           -- error (useful for extracting from existential types).

        -- The HasTyCon* classes are abbreviations for constructing
        -- instances of Typeable. 
        HasTyCon(..),
        HasTyCon1(..),
        HasTyCon2(..),
        HasTyCon3(..),
        HasTyCon4(..),
        HasTyCon40011(..),
        ) 
where

import qualified GlaExts(unsafeCoerce#)
import qualified Dynamic
import Dynamic(Typeable(..))
import PrelDynamic(Dynamic(..))
import Debug(debug)


-- -----------------------------------------------------------------------
-- The fromDyn/toDyn hack
-- -----------------------------------------------------------------------

type Dyn = Dynamic

tyRep = error "Tyrep accessed"

fromDyn :: Typeable a => Dyn -> Maybe a
fromDyn (Dynamic _ o) = Just (GlaExts.unsafeCoerce# o)

toDyn :: Typeable a => a -> Dyn
toDyn d = Dynamic tyRep (GlaExts.unsafeCoerce# d)

-- -----------------------------------------------------------------------
-- End of hack
-- -----------------------------------------------------------------------

type TypeTag = Dynamic.TypeRep
type TyCon = Dynamic.TyCon

mkTyCon :: String -> String -> TyCon
mkTyCon mname tname = Dynamic.mkTyCon (mname ++ "." ++ tname)

mkTypeTag :: TyCon -> [TypeTag] -> TypeTag
mkTypeTag tycon targs = Dynamic.mkAppTy tycon targs

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
-- The HasTyCon* classes are used to indicate that
-- a type constructor produces typeable values
------------------------------------------------------------------------

class HasTyCon typeCon where
   tyCon :: typeCon -> TyCon

instance HasTyCon typeCon => Typeable typeCon where
   typeOf _ =
      let
         (tC :: typeCon) = error "Dynamics.1"
      in
         mkTypeTag (tyCon tC) []

class HasTyCon1 typeCon where
   tyCon1 :: Typeable value => typeCon value -> TyCon

instance (HasTyCon1 typeCon,Typeable value) => Typeable (typeCon value) where
   typeOf _ =
      let
         (tC :: typeCon value) = error "Dynamics.2"
         (v :: value) = error "Dynamics.3"
      in
         mkTypeTag (tyCon1 tC) [typeOf v]
 
class HasTyCon2 typeCon where
   tyCon2 :: (Typeable value1,Typeable value2) 
      => typeCon value1 value2 -> TyCon

instance (HasTyCon2 typeCon,Typeable value1,Typeable value2) 
      => Typeable (typeCon value1 value2) where
   typeOf _ =
      let
         (tC :: typeCon value1 value2) = error "Dynamics.4"
         (v1 :: value1) = error "Dynamics.5"
         (v2 :: value2) = error "Dynamics.6"
      in
         mkTypeTag (tyCon2 tC) [typeOf v1,typeOf v2]

class HasTyCon3 typeCon where
   tyCon3 :: (Typeable value1,Typeable value2,Typeable value3) 
      => typeCon value1 value2 value3 -> TyCon

instance (HasTyCon3 typeCon,Typeable value1,Typeable value2,Typeable value3) 
      => Typeable (typeCon value1 value2 value3) where
   typeOf _ =
      let
         (tC :: typeCon value1 value2 value3) = error "Dynamics.7"
         (v1 :: value1) = error "Dynamics.8"
         (v2 :: value2) = error "Dynamics.9"
         (v3 :: value3) = error "Dynamics.10"
      in
         mkTypeTag (tyCon3 tC) [typeOf v1,typeOf v2,typeOf v3]

class HasTyCon4 typeCon where
   tyCon4 :: (Typeable value1,Typeable value2,Typeable value3,Typeable value4) 
      => typeCon value1 value2 value3 value4 -> TyCon

instance (HasTyCon4 typeCon,Typeable value1,Typeable value2,Typeable value3,
      Typeable value4) => Typeable (typeCon value1 value2 value3 value4) where
   typeOf _ =
      let
         (tC :: typeCon value1 value2 value3 value4) = error "Dynamics.11"
         (v1 :: value1) = error "Dynamics.12"
         (v2 :: value2) = error "Dynamics.13"
         (v3 :: value3) = error "Dynamics.14"
         (v4 :: value4) = error "Dynamics.15"
      in
         mkTypeTag (tyCon4 tC) [typeOf v1,typeOf v2,typeOf v3,typeOf v4]

class HasTyCon40011 typeCon where
   tyCon40011 :: (Typeable value1,Typeable value2,
      HasTyCon1 con3,HasTyCon1 con4)
      => typeCon value1 value2 con3 con4 -> TyCon

instance (HasTyCon40011 typeCon,Typeable value1,Typeable value2,
      HasTyCon1 con3,HasTyCon1 con4) 
      => Typeable (typeCon value1 value2 con3 con4) where
   typeOf _ =
      let
         (tC :: typeCon value1 value2 con3 con4) = tC
         (v1 :: value1) = v1
         (v2 :: value2) = v2
         (v3 :: con3 ()) = v3
         (v4 :: con4 ()) = v4
         mk con = mkTypeTag con []
      in
         mkTypeTag (tyCon40011 tC) [typeOf v1,typeOf v2,
            mk (tyCon1 v3),mk (tyCon1 v4)]

