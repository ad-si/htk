{- This module defines the types EntityNames (and so on), as used by
   the LinkManager. -}
module EntityNames(
   EntityName, 
      -- a name identifying a single item in a folder
   EntityFullName, 
      -- identifies a sequence of names, the first one being
      -- the most significant.
   EntitySearchName,
      -- An EntityFullName but possibly 
   EntityPath, -- Specifies a list of full paths to search; may also 
      -- specify relative paths.
   ) where

import List

import AtomString
import ExtendedPrelude
import DeepSeq

import SimpleForm

import CodedValue

-- ----------------------------------------------------------------------
-- The types.
-- ----------------------------------------------------------------------

---
-- Example EntityName's "a", "bc".  In general a non-empty sequence of
-- characters none of which may be ".", "/" or ":".
newtype EntityName = EntityName String

---
-- An EntityFullName is a name for some object, relative to some other
-- object.  "." represents that other object.
-- Example EntityFullName's: "a", "a/bc".
newtype EntityFullName = EntityFullName [EntityName]

---
-- EntitySearchName's represent search paths starting from some particular
-- object.
-- Example EntitySearchName's: "abc", "a/bc", ".", "../a/b".
--
-- Perhaps a BNF description will help.
--
-- <name> is a legal EntityName.
--
-- <subdirs> ::= <name> ( "/" <name> )*
--
-- <parents> ::= ( "../" ) *
--
-- <fullName> :: = "."
-- <fullName> :: = <subdirs>
--
-- <searchName> ::= "."
-- <searchName> ::= <parents> <subdirs>
-- <searchName> ::= <parents> "../"
--
-- Then <fullName> is a legal EntityFullName and <searchName> a legal 
-- EntitySearchName.
data EntitySearchName = 
      FromParent EntitySearchName -- go up one directory.
   |  FromHere EntityFullName

---
-- An EntityPath is a non-empty sequence of EntitySearchNames.  Thus we
-- can define the syntax as follows:
-- <path> ::= <searchName> ( ":" <searchName> )*
newtype EntityPath = EntityPath [EntitySearchName]

-- ----------------------------------------------------------------------
-- Instances of StringClass
-- We also include checks for validity, using AtomString.fromStringError.
-- ----------------------------------------------------------------------

---
-- We outlaw certain characters from entity names, so that fromString is
-- unambiguous.

badChar :: Char -> Bool
badChar '/' = True
badChar '.' = True
badChar ':' = True
badChar _ = False

---
-- EntityNames are represented with names separated by periods.
instance StringClass EntityName where
   toString (EntityName name) = name
   fromString "" = fromStringError "Empty entity names are forbidden"
   fromString name =
      if any badChar name
         then
            fromStringError (show name ++ " contains illegal characters")
         else
            EntityName name

instance StringClass EntityFullName where
   toString (EntityFullName []) = "."
   toString (EntityFullName entityNames) =
      unsplitByChar '/' (map toString entityNames)

   fromString "" = fromStringError ("\"\" is not a valid full name")
   fromString "." = EntityFullName []
   fromString str =
      EntityFullName (map fromString (splitByChar '/' str))

instance StringClass EntitySearchName where
   toString (FromHere (EntityFullName [])) = "."
   toString other = toStringInner other
      where
         toStringInner (FromParent parent) = "../" ++ toStringInner parent
         toStringInner (FromHere (EntityFullName [])) = ""
         toStringInner (FromHere entityFullName) = toString entityFullName

   fromString "" = fromStringError "\"\" is not a valid search name"
   fromString "." = FromHere (EntityFullName [])
   fromString other = fromStringInner (splitByChar '/' other)
      where
         fromStringInner ("..":rest) = FromParent (fromStringInner rest)
         fromStringInner names 
            = FromHere (EntityFullName (map fromString names))

---
-- EntityPaths are represented as EntityPathComponents separated by colons.
instance StringClass EntityPath where
   toString (EntityPath components) 
      = unsplitByChar ':' (map toString components)
   fromString "" = fromStringError "Empty paths are not allowed"
   fromString str =  EntityPath (map fromString (splitByChar ':' str))

-- ----------------------------------------------------------------------
-- To pick up errors we use DeepSeq to do the necessary seq'ing.
-- ----------------------------------------------------------------------

instance DeepSeq EntityName where
   deepSeq (EntityName n) y = deepSeq n y

instance DeepSeq EntityFullName where
   deepSeq (EntityFullName names) y = deepSeq names y

instance DeepSeq EntitySearchName where
   deepSeq (FromHere fN) y = deepSeq fN y
   deepSeq (FromParent sN) y = deepSeq sN y

instance DeepSeq EntityPath where
   deepSeq (EntityPath sNs) y = deepSeq sNs y

-- ----------------------------------------------------------------------
-- Thus we make them instances of FormTextField
-- ----------------------------------------------------------------------

instance FormTextFieldIO EntityName where
   makeFormStringIO = return . toString
   readFormStringIO = fromStringWE

instance FormTextFieldIO EntityFullName where
   makeFormStringIO = return . toString
   readFormStringIO = fromStringWE

instance FormTextFieldIO EntitySearchName where
   makeFormStringIO = return . toString
   readFormStringIO = fromStringWE

instance FormTextFieldIO EntityPath where
   makeFormStringIO = return . toString
   readFormStringIO = fromStringWE




