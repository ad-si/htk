-- | This module contains the code for reading the password file and
-- checking users against it.
-- 
-- The password file is re-read for each authentication request.
-- 
-- The password file has the following format:
-- 
-- > user:encrypted password:...
--
-- This module also contains the code for permitting users to claim
-- "ADMIN" status.  To do this they must belong to the group "ADMIN".
module PasswordFile(
   User(..), -- information for a particular user
   getUserEntry, -- :: String -> IO (Maybe User)
      -- get that information, given the user id.
   getAdminStatus, -- :: User -> IO Bool
      -- Returns True if user currently has admin status
   claimAdmin, -- :: User -> IO Bool
      -- Claim admin status.
   revokeAdmin, -- :: User -> IO ()
      -- Revoke admin status.
   ) where

import IO
import Maybe

import Control.Concurrent.MVar

import Computation
import ExtendedPrelude
import WBFiles

import GroupFile (getGroupFile,userIsInGroup)


-- ------------------------------------------------------------------------
-- The User datatype.
-- ------------------------------------------------------------------------

-- | NB.  Various bits of the SimpleDB assume that in fact the User
-- type is created afresh for each session, for example the abuse of
-- weak pointers in 'SimpleDBTypes.openVersions'.
data User = User {
   userId :: String,
   encryptedPassword :: String,
   adminMVar :: MVar Bool,
   other :: String
   }

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance Eq User where
   (==) = mapEq adminMVar

-- ------------------------------------------------------------------------
-- Accessing the data for a particular user
-- ------------------------------------------------------------------------

getUserEntry :: String -> IO (Maybe User)
getUserEntry uId =
   do
      users <- getUserEntrys
      return (findJust
         (\ user -> if userId user == uId then Just user else Nothing) 
         users
         )
   

getUserEntrys :: IO [User]
getUserEntrys =
   do
      passwdname <- getServerFile "passwd"
      passwd <- openFile passwdname ReadMode
      contents <- hGetContents passwd
      let
         userLines = lines contents
         userActWEs = map parseLine userLines
      userOpts <- mapM
         (\ userWE -> case fromWithError userWE of
            Right userAct -> 
               do
                  user <- userAct
                  return (Just user)
            Left error ->
               do
                  putStrLn error
                  return Nothing
            )
         userActWEs
      let
         users = catMaybes userOpts
      hClose passwd
      return users

parseLine :: String -> WithError (IO User)
parseLine userLine =
   case splitByChar ':' userLine of
      userId:encryptedPassword:other:_ ->
         hasValue (
            do
               adminMVar <- newMVar False
               return (User {
                  userId = userId,
                  encryptedPassword = encryptedPassword,
                  adminMVar = adminMVar,
                  other = other
                  })
             )
      _ -> hasError (
          "Couldn't parse line in server password file: "++userLine)

-- | Returns user's current admin status.
getAdminStatus :: User -> IO Bool
getAdminStatus user =
   modifyMVar (adminMVar user)
      (\ status ->
         if status
            then
               do
                  stillInAdmin <- userIsInAdmin user
                  return (stillInAdmin,stillInAdmin)
            else
               return (False,False)
         )

-- | Returns whether user may claim admin status
userIsInAdmin :: User -> IO Bool
userIsInAdmin user =
   do
       groupFile <- getGroupFile
       return (userIsInGroup groupFile (userId user) "ADMIN")

-- | Claim admin status; return True if successful.
claimAdmin :: User -> IO Bool
claimAdmin user =
   modifyMVar (adminMVar user)
      (\ status ->
         do
            allowed <- userIsInAdmin user
            return (allowed,allowed)
         )

-- | Revoke admin status.
revokeAdmin :: User -> IO ()
revokeAdmin user = modifyMVar_ (adminMVar user) (\ _ -> return False)