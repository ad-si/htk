-- | An 'AccessClass' describes what access is permitted to an object.
module AccessClass(
   AccessClass(..),
   GroupOrUser(..),
   Permission(..),
   Permissions,
   emptyAccessClass,
   editPermissions,
   parseAccessClass,
   parsePermissions,
   unparsePermissions,
   ) where

import Char

import Text.ParserCombinators.Parsec

import SimpleForm

import GroupFile

import BinaryAll
import Computation 
import ExtendedPrelude

import {-# SOURCE #-} SimpleDBServer


-- -------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------

-- | 'Just' 'True'  means access is permitted; 'Just' 'False', that it is
-- denied; 'Nothing', that this permission says nothing about the access.
data Permission = Permission {
   readAccess :: Maybe Bool,
   writeAccess :: Maybe Bool
   } deriving (Show)

type Permissions = [(GroupOrUser,Permission)]

-- | The permissions for a particular object.
data AccessClass = AccessClass {
   permissions :: Permissions,
      -- ^ We check the permissions in order until we find one that matches
      -- and gives Just True or Just False.
   parent :: Maybe Location
      -- ^ If none is found, we then look for a parent access class using
      -- Location.
   }

-- -------------------------------------------------------------------------
-- Elementary values
-- -------------------------------------------------------------------------

emptyAccessClass :: AccessClass
emptyAccessClass = AccessClass {permissions = [],parent = Nothing}

-- -------------------------------------------------------------------------
-- Instances of HasBinary
-- -------------------------------------------------------------------------

instance Monad m => HasBinary Permission m where
   writeBin = mapWrite (\ (Permission {
      readAccess = readAccess,writeAccess = writeAccess})
      -> (readAccess,writeAccess))
   readBin = mapRead (\ (readAccess,writeAccess) 
      -> Permission {readAccess = readAccess,writeAccess = writeAccess})

instance (Monad m,HasBinary Location m) => HasBinary AccessClass m where
   writeBin = mapWrite (\ (AccessClass {
      permissions = permissions,
      parent = parents}) 
      -> (permissions,parents))
   readBin = mapRead (\ (permissions,parents) 
      -> AccessClass {
      permissions = permissions,
      parent = parents})

-- -------------------------------------------------------------------------
-- Reading an access class
-- -------------------------------------------------------------------------

editPermissions :: Permissions -> IO (Maybe Permissions)
editPermissions permissions0 =
   do
      let
         permissionsForm1 :: Form String
         permissionsForm1 =
            newFormEntry "Edit" (unparsePermissions permissions0)

         permissionsForm2 :: Form Permissions
         permissionsForm2 = mapForm parsePermissions permissionsForm1

         example :: Form ()
         example = nullForm 
            "Example: +#trustedUser:rw -readOnlyGroup:w"

         permissionsForm3 = fmap fst (permissionsForm2 // example)

      doForm "Edit permissions" permissionsForm3

-- -------------------------------------------------------------------------
-- Unparsing an access class.
-- -------------------------------------------------------------------------

unparsePermissions :: Permissions -> String
unparsePermissions permissions =
   let
      permissions1 :: [(Bool,GroupOrUser,String)]
      permissions1 = unparsePermissions1 permissions

      permissions2 :: [String]
      permissions2 = map
         (\ (access,groupOrUser,pStr) ->
            (if access then "+" else "-")
            ++ unparseGroupOrUser groupOrUser
            ++ ":"
            ++ pStr
            )
         permissions1
   in
      unwords permissions2

unparsePermissions1 :: Permissions -> [(Bool,GroupOrUser,String)]
unparsePermissions1 permissions =
   do
      (g,permission) <- permissions

      (pStr,access) <- case (readAccess permission,writeAccess permission) of
         (Nothing,Nothing) -> []
         (Just b1,Just b2) ->
            if b1 == b2
               then
                  [("rw",b1)]
               else
                  [("r",b1),("w",b2)]
         (Just b1,Nothing) -> [("r",b1)]
         (Nothing,Just b2) -> [("w",b2)]
      return (access,g,pStr)
                   


-- -------------------------------------------------------------------------
-- Parsing an access class.
-- -------------------------------------------------------------------------

-- | Parse an 'AccessClass' given the parent data (which we don't parse).
parseAccessClass :: Maybe Location -> String -> WithError AccessClass 
parseAccessClass parentOpt permissionsStr =
   do
      permissions <- parsePermissions permissionsStr
      return (AccessClass {
         permissions = permissions,
         parent = parentOpt
         })

-- | Parse the permissions part of an 'AccessClass'
parsePermissions :: String -> WithError Permissions
parsePermissions permissionsStr =
   let
      pe :: Either ParseError Permissions
      pe = parse permissionsParser "Input" permissionsStr
   in
      case pe of
         Left error -> fail (show error)
         Right permissions -> return permissions

-- | The permissions
permissionsParser :: Parser Permissions
permissionsParser =
   do
      spaces
      permission <- permissionParser
      permissions <- permissions1Parser
      return (permission:permissions)

-- | What we expect after reading a permission
permissions1Parser :: Parser Permissions
permissions1Parser =
   allowEOF (
      do
         spaces1
         allowEOF (
            do
               permission <- permissionParser
               permissions <- permissions1Parser
               return (permission : permissions)
            )
      )
      

allowEOF :: Parser [a] -> Parser [a]
allowEOF parser0 =
      (do
         eof
         return []
      )
   <|>parser0
      

-- | A single permission
permissionParser :: Parser (GroupOrUser,Permission)
permissionParser =
   do
      accessOrDeny <- plusMinusParser
      groupOrUser <- groupOrUserParser
      colonParser
      rws <- rwParser

      let
         writeAccess :: Maybe Bool
         writeAccess = 
            do
               findJust (\ b -> if b then Just () else Nothing) rws
               return accessOrDeny

         readAccess :: Maybe Bool
         readAccess = 
            do
               findJust (\ b -> if b then Nothing else Just ()) rws
               return accessOrDeny

         permission = Permission {
            readAccess = readAccess,
            writeAccess = writeAccess
            }

      return (groupOrUser,permission)

-- | A plus, minus or nothing
plusMinusParser :: Parser Bool
plusMinusParser =
      (do
         char '+'
         return True
      )
   <|>(do
         char '-'
         return False
      )
   <|>(return True)

-- | A colon
colonParser :: Parser ()
colonParser =
   do
      char ':'
      return ()

      

-- | u[ser] or g[roup] (True for a group)
userGroupParser :: Parser Bool
userGroupParser =
      (do
         char 'u'
         optional (string "ser")
         return False
      )
   <|>(do
         char 'g'
         optional (string "roup")
         return True
      )

-- | A non-empty list of r and w characters (True for w)
rwParser :: Parser [Bool]
rwParser =
   many1 (
         (do
            char 'r'
            return False
         )
      <|>(do
            char 'w'
            return True
         )
      )

-- | One or more spaces
spaces1 :: Parser ()
spaces1 = skipMany1 space


