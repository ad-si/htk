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

   searchNameDirBase, 
      -- :: EntitySearchName -> Maybe (EntitySearchName,Maybe EntityName)
      -- Split a search-name by its last component.
 
   toFullName, -- :: EntitySearchName -> Maybe EntityFullName
      -- return the corresponding EntityFullName, if there is one.

   ImportCommands(..),
   ImportCommand(..),
   Directive(..),

   trivialImportCommands, -- :: ImportCommands
      -- commands which import nothing.


   -- parsers
   entityNameParser, -- :: GenParser Char st EntityName
   entityFullNameParser, -- :: GenParser Char st EntityFullName
   entitySearchNameParser, -- :: GenParser Char st EntitySearchName
   ) where

import Char
import List

import Text.ParserCombinators.Parsec 

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
--
-- FromAbsolute is an absolute name, that always works within an object and
-- ignores any environment.  It is (currently) only used internally and
-- so has no readable parse syntax.  If displayed it is displayed as 
-- #ABSOLUTE.[fullName] or #ABSOLUTE.
data EntitySearchName = 
      FromParent EntitySearchName -- go up one directory.
   |  FromHere EntityFullName
   |  FromCurrent EntityFullName
   |  FromRoot EntityFullName
   |  FromAbsolute EntityFullName
   deriving (Eq,Ord,Show)

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

trivialImportCommands :: ImportCommands
trivialImportCommands = ImportCommands []

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

-- EntityNames are represented with names separated by periods.
instance StringClass EntityName where
   toString (EntityName name) = name
   fromStringWE = mkFromStringWE entityNameParser "Entity Name"

instance StringClass EntityFullName where
   toString (EntityFullName []) = "Current"
   toString (EntityFullName entityNames) =
      unsplitByChar '.' (map toString entityNames)

   fromStringWE = mkFromStringWE entityFullNameParser "Entity FullName" 

instance StringClass EntitySearchName where
   toString (FromRoot (EntityFullName [])) = "Root"
   toString (FromRoot fullName) = "Root." ++ toString fullName
   toString (FromCurrent (EntityFullName [])) = "Current"
   toString (FromCurrent fullName) = "Current." ++ toString fullName
   toString (FromHere fullName) = toString fullName
   toString (FromParent (FromHere (EntityFullName []))) = "Parent"
   toString (FromParent searchName) = "Parent." ++ toString searchName
   toString (FromAbsolute (EntityFullName [])) = "#ABSOLUTE"
   toString (FromAbsolute fullName) = "#ABSOLUTE." ++ toString fullName

   fromStringWE (str @ ('#':'A':'B':'S':'O':'L':'U':'T':'E':rest)) 
      | rest == [] 
         = hasValue (FromAbsolute (EntityFullName []))
      | '.' : fullNameStr <- rest,
         Right fullName <- fromWithError (fromStringWE fullNameStr)
         = hasValue (FromAbsolute fullName)
   fromStringWE str
      = mkFromStringWE entitySearchNameParser "Entity Search Name" str
          
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
   deepSeq (FromAbsolute fn) y = deepSeq fn y

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

-- Split off a final component from a name.
entityDirBase :: EntityFullName -> Maybe (EntityFullName,EntityName)
entityDirBase (EntityFullName []) = Nothing
entityDirBase (EntityFullName (name0 : names0)) =
   case entityDirBase (EntityFullName names0) of
      Nothing -> Just (EntityFullName [],name0)
      Just (EntityFullName names1,name1) 
         -> Just (EntityFullName (name0 : names1),name1)

entityDir :: EntityFullName -> Maybe EntityFullName
entityDir fullName = fmap fst (entityDirBase fullName)
 
entityBase :: EntityFullName -> Maybe EntityName
entityBase fullName = fmap snd (entityDirBase fullName)

trivialFullName :: EntityFullName
trivialFullName = EntityFullName []

toFullName :: EntitySearchName -> Maybe EntityFullName
toFullName (FromHere fullName) = Just fullName
toFullName (FromCurrent fullName) = Just fullName
toFullName _ = Nothing

searchNameDirBase 
   :: EntitySearchName -> Maybe (EntitySearchName,Maybe EntityName)
searchNameDirBase (FromRoot fname0) = case entityDirBase fname0 of
   Just (fname1,name) -> Just (FromRoot fname1,Just name)
   Nothing -> Nothing
searchNameDirBase (FromHere fname0) = case entityDirBase fname0 of
   Just (fname1,name) -> Just (FromHere fname1,Just name)
   Nothing -> Just (FromParent (FromHere trivialFullName),Nothing)
searchNameDirBase (FromCurrent fname0) = case entityDirBase fname0 of
   Just (fname1,name) -> Just (FromCurrent fname1,Just name)
   Nothing -> Just (FromParent (FromCurrent trivialFullName),Nothing)
searchNameDirBase (FromParent sname0) = case searchNameDirBase sname0 of
   Just (sname1,nameOpt) -> Just (FromParent sname1,nameOpt)
searchNameDirBase (FromAbsolute fname0) = case entityDirBase fname0 of
   Just (fname1,name) -> Just (FromAbsolute fname1,Just name)
   Nothing -> Nothing


-- ----------------------------------------------------------------------
-- Parser functions for EntityName, EntityFullName and EntitySearchName
-- ----------------------------------------------------------------------

simpleNameParser :: GenParser Char st String
simpleNameParser =
   do
     c <- letter
     cs <- many simpleNameCharParser
     return (c : cs)

simpleNameCharParser :: GenParser Char st Char
-- Characters allowed after the first character of a simpleName.
simpleNameCharParser =
   satisfy
      (\ ch -> isAlphaNum ch || (ch == '_') || (ch == ':'))

simpleNamesParser :: GenParser Char st [String]
simpleNamesParser =
   do
      spaces
      sepBy simpleNameParser (char '.')

entityNameParser :: GenParser Char st EntityName
entityNameParser =
   do
      spaces
      str <- simpleNameParser
      case mkEntityName str of
         Nothing -> fail (show str ++ " is not a valid entity name")
         Just name -> return name 

entityFullNameParser :: GenParser Char st EntityFullName
entityFullNameParser =
   do
      spaces
      strs <- simpleNamesParser
      case mkEntityFullName strs of
         Nothing -> fail (show (unsplitByChar0 '.' strs)  
            ++ " is not a valid entity full name")
         Just name -> return name 

entitySearchNameParser :: GenParser Char st EntitySearchName
entitySearchNameParser =
   do
      spaces
      strs <- simpleNamesParser
      case mkEntitySearchName strs of
         Nothing -> fail (show (unsplitByChar0 '.' strs)  
            ++ " is not a valid entity search name")
         Just name -> return name 



-- --------------------------------------------------------------------------
-- Utility functions for parser
-- --------------------------------------------------------------------------

mkEntityName :: String -> Maybe EntityName
mkEntityName name =
   if reservedName name
      then
         Nothing
      else
         Just (EntityName name)

reservedName :: String -> Bool
reservedName "Root" = True
reservedName "Parent" = True
reservedName "Current" = True
reservedName _ = False

mkEntityFullName :: [String] -> Maybe EntityFullName
mkEntityFullName [] = Nothing
mkEntityFullName strs =
   let
      entityNameOpts :: [Maybe EntityName]   
      entityNameOpts =
         map 
            (\ str -> mkEntityName str)
            strs

      entityNamesOpt :: Maybe [EntityName]
      entityNamesOpt = fromMaybes entityNameOpts
   in
      fmap
         EntityFullName
         entityNamesOpt

mkEntitySearchName :: [String] -> Maybe EntitySearchName
mkEntitySearchName [] = Nothing
mkEntitySearchName strs0 =
   let
      (prefixFn,strs1) = searchNamePrefix strs0

      fullNameOpt = case strs1 of
         [] -> Just (EntityFullName [])
         _ -> mkEntityFullName strs1
   in
      fmap prefixFn fullNameOpt 

searchNamePrefix :: [String] -> (EntityFullName -> EntitySearchName,[String])
searchNamePrefix ("Current" : strs) = (FromCurrent,strs)
searchNamePrefix ("Root" : strs) = (FromRoot,strs)
searchNamePrefix strs = getParents strs
   where
      getParents :: [String] -> (EntityFullName -> EntitySearchName,[String])
      getParents ("Parent" : strs0) =
         let
            (prefixFn0,strs1) = getParents strs0
         in
            (FromParent . prefixFn0,strs1)
      getParents strs = (FromHere,strs)
