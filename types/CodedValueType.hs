{- CodedValueType's represent the type of a CodedValue; IE a
   value of an attribute attached to an object.  So in general
   types which instance HasCodedValue should also instance
   HasCodedValueType and vice-versa, though this is not (currently)
   enforced by the type system.

   For the time being, we use Typeable for this purpose . . .
   However properties which should always be true are that
   (1) the CodedValueType is sufficient to determine the representation
       as stored by CodedValue.encodeIO.
   (2) Int/String/Bool/Either/Maybe/lists/ShortList and tuples of size at
       most 4 are instances.
   -}
module CodedValueType(
   CodedValueType,
   -- represents the type,
   HasCodedValueType(..),
   -- Things which have this type.  
   ) where

import Dynamics

import CodedValue

newtype CodedValueType = CodedValueType TypeTag

class HasCodedValueType value where
   codedValueType :: value -> CodedValueType

-- ----------------------------------------------------------------------
-- Instances
--- ---------------------------------------------------------------------

instance Typeable value => HasCodedValueType value where
   codedValueType value = CodedValueType (typeOf value)
