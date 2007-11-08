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

   CodedValueTypeNorm,
   -- Something (a) to which a CodedValueType can be converted;
   -- (b) an instance of Eq,Ord,HasCodedValue.
   -- This allows us to read and write CodedValueType's.
   ) where

import Dynamics
import AtomString

import CodedValue

newtype CodedValueType = CodedValueType TypeTag

class HasCodedValueType value where
   codedValueType :: value -> CodedValueType

-- ----------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------

instance Typeable value => HasCodedValueType value where
   codedValueType value = CodedValueType (typeOf value)

-- ----------------------------------------------------------------------
-- CodedValueTypeNorm
-- This is made abstract so we can replace it with CodedValueType itself
-- if we later provide a version of that which can be read and written.
-- ---------------------------------------------------------------------

newtype CodedValueTypeNorm = CodedValueTypeNorm AtomString deriving (Eq,Ord)

codedValueTypeNorm_tyCon = mkTyCon "CodedValueType" "CodedValueTypeNorm"

instance HasTyCon CodedValueTypeNorm where
   typeOf _ = codedValueTypeNorm_tyCon

instance HasCodedValue CodedValueTypeNorm where
   encodeIO = mapEncodeIO (\ (CodedValueTypeNorm a) -> Str a)
   decodeIO = mapDecodeIO (\ (Str a) -> CodedValueTypeNorm a)

