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
        ) 
where

import qualified Dynamic
import Dynamic(Typeable)

import Debug(debug)

type TypeTag = Dynamic.TypeRep
type TyCon = Dynamic.TyCon

mkTyCon :: String -> String -> TyCon
mkTyCon mname tname = Dynamic.mkTyCon (mname ++ "." ++ tname)

mkTypeTag :: TyCon -> [TypeTag] -> TypeTag
mkTypeTag tycon targs = Dynamic.mkAppTy tycon targs

type Dyn = Dynamic.Dynamic

fromDyn :: Typeable a => Dyn -> Maybe a
fromDyn = Dynamic.fromDynamic

toDyn :: Typeable a => a -> Dyn
toDyn = Dynamic.toDyn

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
            debug d
            ioError typeMismatch
      (Just x) -> return x

typeMismatch :: IOError
typeMismatch = 
        userError "internal type of dynamics does not match expected type"



