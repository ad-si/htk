{- This module defines the types EntityNames (and so on), as used by
   the LinkManager. -}
module EntityNames(
   EntityName(..), 
      -- a name identifying a single item in a folder
   EntityFullName(..), 
      -- identifies a sequence of names, the first one being
      -- the most significant.
   EntitySearchName(..),
      -- An EntityFullName but possibly 
   EntityPath(..), -- Specifies a list of full paths to search; may also 
      -- specify relative paths. 
   raiseEntityPath, -- :: EntityPath -> EntityPath
      -- raise an entityPath replacing each path by a reference from its
      -- parent (so a/b -> ../a/b)
   entityDir, -- :: EntityFullName -> Maybe EntityFullName
      -- Find the parent of the object, if there is one.
   entityBase, -- :: EntityFullName -> Maybe EntityName
      -- Find the name of the object within its parent.
   trivialFullName, -- :: EntityFullName
      -- Name with no components.
   trivialPath, -- :: EntityPath
      -- Path just searching in this object.

   ImportCommands(..),
   ImportCommand(..),
   Directive(..)

   ) where

import List

import AtomString
import ExtendedPrelude
import DeepSeq
import Dynamics
import Computation
import BinaryAll

import SimpleForm

import CodedValue

-- ----------------------------------------------------------------------
-- The types.
-- ----------------------------------------------------------------------

---
-- Example EntityName's "a", "bc".  In general a non-empty sequence of
-- characters none of which may be ".", "/" or ":".
newtype EntityName = EntityName String deriving (Eq,Ord,Typeable)
---
-- An EntityFullName is a name for some object, relative to some other
-- object.  "." represents that other object.
-- Example EntityFullName's: "a", "a/bc".
newtype EntityFullName = EntityFullName [EntityName] deriving (Eq,Ord)

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
-- <searchName> ::= <parents> <fullName>
--
-- Then <fullName> is a legal EntityFullName and <searchName> a legal 
-- EntitySearchName.
data EntitySearchName = 
      FromParent EntitySearchName -- go up one directory.
   |  FromHere EntityFullName
   deriving (Eq,Ord)

---
-- An EntityPath is a non-empty sequence of EntitySearchNames.  Thus we
-- can define the syntax as follows:
-- <path> ::= <searchName> ( ":" <searchName> )*
newtype EntityPath = EntityPath [EntitySearchName] deriving (Ord,Eq)


-- *************************************************************************
-- Changes for new import facilities:
-- *************************************************************************

-- A PackageName for a path alias is defined as follows:
-- 
-- PackageName ::= Identifier | Alias | PackageName.Identifier
--
-- where an Alias is an Identifier which was defined through a path alias statement.
-- Furthermore, an Identifier can be one these special values: "Root", "Current", "Parent" 
--
-- So, we translate this into EntityNames:
--
-- alias ::= <name>       (names defined by path alias statements)
-- specialName ::= "Current" | "Parent" | "Root"
-- identifier ::= <specialName> | <alias> | <name>     (identifier and alias are disjoint)
-- <fullname> ::= <identifier>  ("." <fullname>)*
-- 
-- where: <name> is meant as defined by George above, but cannot be a specialName
--
-- So, we code identifier with the datatype 'EntityName' and the fullnames with
-- 'EntityFullName'. The individual parts of a PackageName, devided by "." are
-- translated into list members of the corresponding EntityFullName-value.


newtype ImportCommands = ImportCommands [ImportCommand]

data ImportCommand = 
      Import [Directive] EntityFullName
   |  PathAlias EntityName EntityFullName

data Directive = 
      Qualified
   |  Unqualified
   |  Global
   |  Local
   |  Hide [EntityFullName]
   |  Reveal [EntityFullName]
   |  Rename {
               newName :: EntityName,
               oldName :: EntityFullName
             }

-- ----------------------------------------------------------------------
-- Instances of CodedValue
-- ----------------------------------------------------------------------

instance Monad m => HasWrapper Directive m where
   wraps = [
      wrap0 0 Qualified,
      wrap0 1 Unqualified,
      wrap0 2 Global,
      wrap0 3 Local,
      wrap1 4 Hide,
      wrap1 5 Reveal,
      wrap2 6 Rename
      ]
   unWrap = (\ wrapper -> case wrapper of
      Qualified -> UnWrap 0 ()
      Unqualified -> UnWrap 1 ()
      Global -> UnWrap 2 ()
      Local -> UnWrap 3 ()
      Hide l -> UnWrap 4 l
      Reveal l -> UnWrap 5 l
      Rename n o -> UnWrap 6 (n,o)
      )

instance Monad m => HasBinary ImportCommand m where
   writeBin = mapWrite (\ ic -> case ic of
      Import ds e -> Left (ds,e)
      PathAlias e ef -> Right (e,ef)
      )
   readBin = mapRead (\ et -> case et of
      Left (ds,e) -> Import ds e
      Right (e,ef) -> PathAlias e ef
      )

instance Monad m => HasBinary ImportCommands m where
   writeBin = mapWrite (\ (ImportCommands ics) -> ics)
   readBin = mapRead (\ ics -> ImportCommands ics)
   


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
   fromStringWE "" = hasError "Empty entity names are forbidden"
   fromStringWE name =
      if any badChar name
         then
            hasError (show name ++ " contains illegal characters")
         else
            hasValue (EntityName name)

instance StringClass EntityFullName where
   toString (EntityFullName []) = "."
   toString (EntityFullName entityNames) =
      unsplitByChar '/' (map toString entityNames)

   fromStringWE "" = hasError ("\"\" is not a valid full name")
   fromStringWE "." = hasValue (EntityFullName [])
   fromStringWE str =
      mapWithError EntityFullName
         (concatWithError
            (map fromStringWE (splitByChar '/' str))
            )


instance StringClass EntitySearchName where
   toString (FromHere (EntityFullName [])) = "."
   toString other = toStringInner other
      where
         toStringInner (FromParent parent) = "../" ++ toStringInner parent
         toStringInner (FromHere (EntityFullName [])) = ""
         toStringInner (FromHere entityFullName) = toString entityFullName

   fromStringWE "" = hasError "\"\" is not a valid search name"
   fromStringWE "." = hasValue (FromHere (EntityFullName []))
   fromStringWE "../" = hasValue (FromParent (FromHere (EntityFullName [])))
   fromStringWE ('.':'.':'/':rest) =
      mapWithError FromParent (fromStringWE rest)
   fromStringWE other = 
      mapWithError FromHere (fromStringWE other)

---
-- EntityPaths are represented as EntityPathComponents separated by colons.
instance StringClass EntityPath where
   toString (EntityPath components) 
      = unsplitByChar ':' (map toString components)
   fromStringWE "" = hasError "Empty paths are not allowed"
   fromStringWE str = 
     mapWithError EntityPath
         (concatWithError
            (map fromStringWE (splitByChar ':' str))
            )

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
   readFormStringIO = return . fromStringWE

instance FormTextFieldIO EntityFullName where
   makeFormStringIO = return . toString
   readFormStringIO = return . fromStringWE

instance FormTextFieldIO EntitySearchName where
   makeFormStringIO = return . toString
   readFormStringIO = return . fromStringWE

instance FormTextFieldIO EntityPath where
   makeFormStringIO = return . toString
   readFormStringIO = return . fromStringWE

-- ----------------------------------------------------------------------
-- Instances of HasCodedValue
-- ----------------------------------------------------------------------

instance Monad m => HasBinary EntityName m where
   writeBin = mapWrite (\ name -> Str name)
   readBin = mapRead (\ (Str name) -> name)

instance Monad m => HasBinary EntityFullName m where
   writeBin = mapWrite (\ name -> Str name)
   readBin = mapRead (\ (Str name) -> name)

instance Monad m => HasBinary EntitySearchName m where
   writeBin = mapWrite (\ name -> Str name)
   readBin = mapRead (\ (Str name) -> name)

instance Monad m => HasBinary EntityPath m where
   writeBin = mapWrite (\ name -> Str name)
   readBin = mapRead (\ (Str name) -> name)

-- ----------------------------------------------------------------------
-- raiseEntityPath
-- ----------------------------------------------------------------------

raiseEntityPath :: EntityPath -> EntityPath
raiseEntityPath (EntityPath entitySearchNames) =
   EntityPath (map FromParent entitySearchNames)

-- ----------------------------------------------------------------------
-- Miscellaneous functions.
-- ----------------------------------------------------------------------

entityDir :: EntityFullName -> Maybe EntityFullName
entityDir (EntityFullName names) = fmap EntityFullName (chop 1 names)

entityBase :: EntityFullName -> Maybe EntityName
entityBase (EntityFullName names) = lastOpt names

trivialFullName :: EntityFullName
trivialFullName = EntityFullName []

trivialPath :: EntityPath
trivialPath = EntityPath [FromHere (EntityFullName [])]