-- | 'Permissions' describes what access is permitted to an object.
-- This module defines the type and various operations for editing in and
-- converting it to and from Strings and representing it in binary.
module Permissions(
   GroupOrUser(..),
   Permission(..),
   Activity(..),
   Permissions,
   permissionsValid,
   permissionsValidCheck,
   editPermissions,
   parsePermissions,
   unparsePermissions,
   ) where

import Char
import Maybe

import Text.ParserCombinators.Parsec

import BinaryAll
import Computation 

import SimpleForm

import GroupFile

import VersionInfo
import ServerErrors


-- -------------------------------------------------------------------------
-- Data types
-- -------------------------------------------------------------------------

data Permission = Permission {
   grant :: Bool, 
      -- ^ If 'True', we hereby grant permission, otherwise we deny it.
   domain :: Maybe GroupOrUser,
      -- ^ Who this permission applies to.  If 'Nothing`, it applies to
      -- everyone.
   fromVersion :: Maybe ObjectVersion,
      -- If set, permission applies only to this version and its descendants.
   toVersion :: Maybe ObjectVersion,
      -- If set, permission applies only to this version and its ancestors.
   activities :: [Activity]
      -- ^ What activities this permission applies to.
      -- This list will always be nonempty.
   } deriving (Show) 


data Activity = ReadActivity | WriteActivity | PermissionsActivity 
   deriving (Show,Eq,Enum)
   

-- | In 'Permissions', those to whom the permission applies are given
-- by the 'GroupOrUser'.  If this is empty, the permission applies to 
-- everyone.
-- 
-- The permissions are checked in order until we find one that matches
-- and gives Just True or Just False.
type Permissions = [Permission]

-- -------------------------------------------------------------------------
-- Instances of HasBinary
-- -------------------------------------------------------------------------

instance Monad m => HasBinary Activity m where
   writeBin = mapWrite ViaEnum
   readBin = mapRead enum

instance Monad m => HasBinary Permission m where
   writeBin = mapWrite (\      --
      (Permission {grant = grant,domain = domain,activities = activities,
         fromVersion = fromVersion,toVersion = toVersion})
      -> (grant,domain,activities,fromVersion,toVersion)
      )
   readBin = mapRead (\        --
      (grant,domain,activities,fromVersion,toVersion)
      -> (Permission {grant = grant,domain = domain,activities = activities,
         fromVersion = fromVersion,toVersion = toVersion})
      )

-- -------------------------------------------------------------------------
-- Editing permissions
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
            "Example: +#trustedUser:rw -readOnlyGroup:w -*:rw"

         permissionsForm3 = fmap fst (permissionsForm2 // example)

      doForm "Edit permissions" permissionsForm3

-- -------------------------------------------------------------------------
-- Check validity of a set of permissions
-- -------------------------------------------------------------------------

permissionsValid :: Permissions -> Bool
permissionsValid [] = True
permissionsValid (permission : permissions1) =
   permissionValid permission && permissionsValid permissions1

permissionValid :: Permission -> Bool
permissionValid permission =
   case (elem PermissionsActivity (activities permission),
         fromVersion permission,toVersion permission) of
      (False,_,_) -> True
      (_,Nothing,Nothing) -> True
      _ -> False

permissionsValidCheck :: Permissions -> IO ()
permissionsValidCheck permissions =
   if permissionsValid permissions
      then
         done
      else
         throwError MiscError "Permissions are invalid"

-- -------------------------------------------------------------------------
-- Reading permissions
-- -------------------------------------------------------------------------

unparsePermissions :: Permissions -> String
unparsePermissions permissions1 =
   let
      permissions2 :: [String]
      permissions2 = map
         (\ permission ->
            (if grant permission then "+" else "-")
            ++ case domain permission of
               Nothing -> "*"
               Just groupOrUser -> unparseGroupOrUser groupOrUser
            ++ ":"
            ++ map unparseActivity (activities permission)
            ++ unparseVersionPair (fromVersion permission) 
               (toVersion permission)
            )
         permissions1
   in
      unwords permissions2
                   

unparseActivity :: Activity -> Char
unparseActivity activity = case activity of
   ReadActivity -> 'r'
   WriteActivity -> 'w'
   PermissionsActivity -> 'p'

unparseVersionPair :: Maybe ObjectVersion -> Maybe ObjectVersion -> String
unparseVersionPair fromVersionOpt toVersionOpt =
   case (fromVersionOpt,toVersionOpt) of
      (Nothing,Nothing) -> ""
      (fromV,Nothing) -> p fromV True Nothing
      (Nothing,toV) -> p  Nothing True toV
      (fromV,toV) 
         | fromV == toV -> p fromV False Nothing
         | True -> p fromV True toV
   where
      p :: Maybe ObjectVersion -> Bool -> Maybe ObjectVersion -> String
      p ov1Opt doDots ov2Opt =
         ":["
         ++ q ov1Opt
         ++ (if doDots then ".." else "")
         ++ q ov2Opt
         ++ "]"

      q :: Maybe ObjectVersion -> String
      q ovOpt = case ovOpt of
         Nothing -> ""
         Just (ObjectVersion v) -> show v


-- -------------------------------------------------------------------------
-- Parsing an access class.
-- -------------------------------------------------------------------------

-- | Parse 'Permissions'
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
      allowEOF (
         do
            permission <- permissionParser
            permissions <- permissions1Parser
            return (permission:permissions)
         )

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
permissionParser :: Parser Permission
permissionParser =
   do
      grant <- plusMinusParser
      domain <-
            (do 
               groupOrUser <- groupOrUserParser
               return (Just groupOrUser)
            )
         <|>(do
               char '*'
               return Nothing
            )
      colonParser
      activities <- rwParser
      (fromVersion,toVersion) <- versionInterval

      let
         permission = Permission {
            grant = grant,
            domain = domain,
            fromVersion = fromVersion,
            toVersion = toVersion,
            activities = activities
            }

      if permissionValid permission
         then
            done
         else
            unexpected "p in a permission with a version restriction"

      return permission

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

-- | A non-empty list of r and w characters (True for w)
rwParser :: Parser [Activity]
rwParser =
   many1 (
         (do
            char 'r'
            return ReadActivity
         )
      <|>(do
            char 'w'
            return WriteActivity
         )
      <|>(do
            char 'p'
            return PermissionsActivity
         )
      )

-- | The range of applicable object versions including the previous colon,
-- if present
versionInterval :: Parser (Maybe ObjectVersion,Maybe ObjectVersion)
versionInterval =
   do
      intervalOpt <- optional0 (
         do
            char ':'
            versionInterval1
         )

      return (fromMaybe (Nothing,Nothing) intervalOpt)

-- | The range of applicable object versions
versionInterval1 :: Parser (Maybe ObjectVersion,Maybe ObjectVersion) 
versionInterval1 =
   do
      char '['
      versionAOpt <- optional0 versionParser
      dotsOpt <- optional0 dotsParser
      versionBOpt <- optional0 versionParser
      char ']'
      case (versionAOpt,dotsOpt,versionBOpt) of
         (Just versionA,Nothing,Nothing) 
            -> return (Just versionA,Just versionA)
         (Just versionA,Just (),Nothing)
            -> return (Just versionA,Nothing)
         (Just versionA,Just (),Just versionB)
            -> return (Just versionA,Just versionB)
         (Nothing,Just (),Just versionB)
            -> return (Nothing,Just versionB)
         _ -> unexpected "Unable to parse range of applicable object versions"
      
-- | The parser combinator Parsec doesn't define
optional0 :: Parser a -> Parser (Maybe a)
optional0 parser =
      (do
         a <- parser
         return (Just a)
         )
   <|>(return Nothing)

-- | An ObjectVersion
versionParser :: Parser ObjectVersion
versionParser =
   do
      digits <- many1 (satisfy isDigit)
      return (ObjectVersion (read digits))

-- | Two periods
dotsParser :: Parser ()
dotsParser =
   do
      char '.'
      char '.'
      done

-- | One or more spaces
spaces1 :: Parser ()
spaces1 = skipMany1 space




