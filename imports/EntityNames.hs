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

   entityDir, -- :: EntityFullName -> Maybe EntityFullName
      -- Find the parent of the object, if there is one.
   entityBase, -- :: EntityFullName -> Maybe EntityName
      -- Find the name of the object within its parent.
   trivialFullName, -- :: EntityFullName
      -- Name with no components.

   ImportCommands(..),
   ImportCommand(..),
   Directive(..)

   ) where

import Char

import List

import Maybes
import AtomString
import ExtendedPrelude
import DeepSeq
import Dynamics
import Computation
import BinaryAll

import SimpleForm

-- ----------------------------------------------------------------------
-- The types.
-- ----------------------------------------------------------------------

---
-- Represented as a letter followed by a sequence of characters which may be 
-- letters, digits, '_' or ':'.  Letters and digits do not have to be
-- ASCII; any Unicode letters or digits are permitted.
--
-- "Root", "Parent" or "Current" are forbidden in the written representation.
-- However they can arise internally though prefixing by a module which has
-- such components in its name.
--
-- Example EntityName's: "a", "bc", "z9_", "þ1".
newtype EntityName = EntityName String deriving (Eq,Ord,Typeable,Show)

---
-- An EntityFullName represents a path from an object to
-- some object within it.
--
-- If the path is non-empty, it is represented as a sequence of EntityName's 
-- separated by '.' (but spaces are not allowed).
--
-- Example EntityFullName's: "a", "a.bc", "z9_.q".
--
-- If the path is empty it is represented by "Current".
-- 
newtype EntityFullName = EntityFullName [EntityName] deriving (Eq,Ord,Show)

-- An EntitySearchName represents a path from an object to some other object,
-- which may or may not be within it.
--
-- It may be an EntityFullName, or may be preceded by one of the following
-- (in each case separated by dots), "Current", "Root" or some non-empty 
-- sequence of "Parent".  

-- Example EntitySearchName's: "a", "Current.a", "Root.a", "Parent.Parent.a".
--
-- FromHere is actually identical to FromCurrent, *except* when the
-- search-name is the expansion of an alias.  
data EntitySearchName = 
      FromParent EntitySearchName -- go up one directory.
   |  FromHere EntityFullName
   |  FromCurrent EntityFullName
   |  FromRoot EntityFullName
   deriving (Eq,Ord,Show)

-- used internally only.
data EntityName' = Name EntityName | Current | Root | Parent

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


newtype ImportCommands = ImportCommands [ImportCommand] deriving (Eq)

data ImportCommand = 
      Import [Directive] EntitySearchName
   |  PathAlias EntityName EntitySearchName deriving (Eq,Ord)

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
   deriving (Eq,Ord)

-- ----------------------------------------------------------------------
-- Instances of HasBinary
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

instance StringClass EntityName' where
   toString (Name (EntityName str)) = str 
   toString Current = "Current"
   toString Parent = "Parent"
   toString Root = "Root"

   fromStringWE "" = hasError "Empty entity names are forbidden"
   fromStringWE "Current" = hasValue Current
   fromStringWE "Parent" = hasValue Parent
   fromStringWE "Root" = hasValue Root
   fromStringWE (name @ (c:cs)) =
      if (isAlpha c) && (all isAlphaNum cs)
         then
            hasValue (Name (EntityName name))
         else
            hasError ("Name " ++ show name 
               ++ " contains inappropriate characters")

---
-- EntityNames are represented with names separated by periods.
instance StringClass EntityName where
   toString (EntityName name) = name
   fromStringWE str =
      mapWithError'
         (\ name' -> case name' of
            Name name -> hasValue name
            _ -> hasError ("Unexpected " ++ str)
            )
         (fromStringWE str)

instance StringClass EntityFullName where
   toString (EntityFullName []) = "Current"
   toString (EntityFullName entityNames) =
      unsplitByChar '.' (map toString entityNames)

   fromStringWE "" = hasError ("\"\" is not a valid full name")
   fromStringWE "Current" = hasValue (EntityFullName [])
   fromStringWE str =
      mapWithError EntityFullName
         (concatWithError
            (map fromStringWE (splitByChar '.' str))
            )

instance StringClass EntitySearchName where
   toString (FromRoot (EntityFullName [])) = "Root"
   toString (FromRoot fullName) = "Root." ++ toString fullName
   toString (FromCurrent (EntityFullName [])) = "Current"
   toString (FromCurrent fullName) = "Current." ++ toString fullName
   toString (FromHere fullName) = toString fullName
   toString (FromParent (FromHere (EntityFullName []))) = "Parent"
   toString (FromParent searchName) = "Parent." ++ toString searchName

   fromStringWE "" = badSearchStr ""
   fromStringWE str =
      let
         strs1 :: [String]
         strs1 = splitByChar '.' str

         names1 :: [WithError EntityName']
         names1 = map fromStringWE strs1

         names2 :: WithError [EntityName']
         names2 = listWithError names1

         checkRest :: [EntityName'] -> (EntityFullName -> EntitySearchName) 
            -> WithError EntitySearchName
         checkRest names wrapper =
            let
               nameOpts =
                  map
                     (\ name' -> case name' of
                        Name name -> Just name
                        _ -> Nothing
                        )
                     names

               namesOpt = fromMaybes nameOpts
            in                
               case namesOpt of
                  Nothing -> badSearchStr str
                  Just names -> hasValue (wrapper (EntityFullName names))

         checkParents :: [EntityName'] -> WithError EntitySearchName
         checkParents (Parent : list) = 
            mapWithError FromParent (checkParents list)
         checkParents list = checkRest list FromHere

         convert :: [EntityName'] -> WithError EntitySearchName
         convert (Root : list) = checkRest list FromRoot
         convert (Current : list) = checkRest list FromCurrent
         convert list = checkParents list
      in
         mapWithError' convert names2

badSearchStr :: String -> WithError EntitySearchName
badSearchStr str = hasError (show str ++ " is not a valid search name")

          
-- ----------------------------------------------------------------------
-- To pick up errors we use DeepSeq to do the necessary seq'ing.
-- ----------------------------------------------------------------------

instance DeepSeq EntityName where
   deepSeq (EntityName n) y = deepSeq n y

instance DeepSeq EntityFullName where
   deepSeq (EntityFullName names) y = deepSeq names y

instance DeepSeq EntitySearchName where
   deepSeq (FromHere fN) y = deepSeq fN y
   deepSeq (FromCurrent fN) y = deepSeq fN y
   deepSeq (FromParent sN) y = deepSeq sN y
   deepSeq (FromRoot fn) y = deepSeq fn y

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

-- ----------------------------------------------------------------------
-- Instances of HasBinary
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

-- ----------------------------------------------------------------------
-- Miscellaneous functions.
-- ----------------------------------------------------------------------

entityDir :: EntityFullName -> Maybe EntityFullName
entityDir (EntityFullName names) = fmap EntityFullName (chop 1 names)

entityBase :: EntityFullName -> Maybe EntityName
entityBase (EntityFullName names) = lastOpt names

trivialFullName :: EntityFullName
trivialFullName = EntityFullName []
