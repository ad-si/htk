{- AttributeType encodes the type of things that can be stored in
   attributes. -}
module AttributeType(
   AttributeType(..),
   -- Represents the type
   HasAttributeType(..),
   -- class of types with an AttributeType.

   ) where

import Int

import ExtendedPrelude(bottom)

import AttributeRepresentation

---------------------------------------------------------------------
-- Primitive Attribute Types.  These describe attribute types,
-- together with the types of the constructors.
---------------------------------------------------------------------

newtype PrimAttributeType = PrimAttributeType String deriving (Ord,Eq)
-- for now anyway.  Outside this section, no-one should access this
-- representation.

instance HasConverter PrimAttributeType String where
   encode' (PrimAttributeType str) = str
   decode' str = PrimAttributeType str

-- We provide different versions of the following class for different
-- type kinds.
class HasPrimAttributeType ty where
   primAttributeType :: ty -> PrimAttributeType

class HasPrimAttributeType1 tycon where
   primAttributeType1 :: tycon arg -> PrimAttributeType

class HasPrimAttributeType2 tycon where
   primAttributeType2 :: tycon arg1 arg2 -> PrimAttributeType

---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------

data AttributeType =
   AttributeType {
      constructor :: PrimAttributeType,
      subtypes :: [AttributeType]
      } deriving (Ord,Eq)

class HasCodedValue ty => HasAttributeType ty where
   attributeType :: ty -> AttributeType

---------------------------------------------------------------------
-- Instances of HasAttributeType in terms of HasPrimAttributeType
---------------------------------------------------------------------

instance (HasCodedValue ty,HasPrimAttributeType ty) 
   => HasAttributeType ty
      where
   attributeType value = AttributeType {
      constructor = primAttributeType value,
      subtypes = []
      }

instance (HasCodedValue (tycon arg),HasPrimAttributeType1 tycon,
   HasAttributeType arg) => HasAttributeType (tycon arg)
      where
   attributeType value = AttributeType {
      constructor = primAttributeType1 value,
      subtypes = [attributeType (bottom :: arg)]
      }

instance (HasCodedValue (tycon arg1 arg2),HasPrimAttributeType2 tycon,
   HasAttributeType arg1,HasAttributeType arg2)
   => HasAttributeType (tycon arg1 arg2)
      where
   attributeType value = AttributeType {
      constructor = primAttributeType2 value,
      subtypes = [
         attributeType (bottom :: arg1),
         attributeType (bottom :: arg2)
         ]
      }

---------------------------------------------------------------------
-- Instances of HasPrimAttributeType*
---------------------------------------------------------------------

instance HasPrimAttributeType () where
   primAttributeType _ = decode' ""

instance HasPrimAttributeType Char where
   primAttributeType _ = decode' "C"

instance HasPrimAttributeType Bool where
   primAttributeType _ =  decode' "B"

instance HasPrimAttributeType Int8 where
   primAttributeType _ = decode' "I8"

instance HasPrimAttributeType Int16 where
   primAttributeType _ = decode' "I8"

instance HasPrimAttributeType Int32 where
   primAttributeType _ = decode' "I8"

instance HasPrimAttributeType Int64 where
   primAttributeType _ = decode' "I8"

instance HasPrimAttributeType Int where
   primAttributeType _ = decode' "I"

instance HasPrimAttributeType1 [] where
   primAttributeType1 _ = decode' "["

instance HasPrimAttributeType1 Maybe where
   primAttributeType1 _ = decode' "M"

instance HasPrimAttributeType2 Either where
   primAttributeType2 _ = decode' "E"

---------------------------------------------------------------------
-- PrimAttributeType as an instance of HasCodedValue
---------------------------------------------------------------------

instance HasConverter String (ShortList Char) where
   encode' str = ShortList str
   decode' (ShortList str) = str

instance HasConverter PrimAttributeType (ShortList Char) where
   encode' value = encode' (encode' value :: String)
   decode' value = decode' (decode' value :: String)

instance HasCodedValue PrimAttributeType where
   encode value codedValue = 
      encode (encode' value :: ShortList Char) codedValue
   decode codedValue0 =
      let
         (value' :: ShortList Char,codedValue1) = decode codedValue0
      in
         (decode' value',codedValue1)

---------------------------------------------------------------------
-- AttributeType as an instance of HasCodedValue
---------------------------------------------------------------------

instance HasCodedValue AttributeType where
   encode (AttributeType {constructor = constructor,subtypes = subtypes})
      codedValue0 =
      let
         codedValue1 = encode (ShortList subtypes) codedValue0
         codedValue2 = encode constructor codedValue1
      in
         codedValue2
   decode codedValue0 =
      let
         (constructor,codedValue1) = decode codedValue0
         (ShortList subtypes,codedValue2) = decode codedValue1
      in
         (AttributeType {constructor = constructor,subtypes = subtypes},
            codedValue2)
