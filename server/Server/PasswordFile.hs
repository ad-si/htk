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
module Server.PasswordFile(
   User(userId,encryptedPassword,other), -- information for a particular user
   getUserEntry, -- :: String -> IO (Maybe User)
      -- get that information, given the user id.
   createUser, -- :: String -> IO User
      -- create a user id.
   getAdminStatus, -- :: User -> IO Bool
      -- Returns True if user currently has admin status
   claimAdmin, -- :: User -> IO Bool
      -- Claim admin status.
   revokeAdmin, -- :: User -> IO ()
      -- Revoke admin status.
   alwaysAllowAdmin, -- :: IO ()
      -- Used for internal servers.
   ) where

import System.IO
import Data.Maybe

import Control.Concurrent.MVar
import Data.IORef
import System.IO.Unsafe

import Util.Computation
import Util.Object
import Util.ExtendedPrelude
import Util.WBFiles

import Server.GroupFile (getGroupFile,userIsInGroup)


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
   other :: String,
   oID :: ObjectID
   }

-- ------------------------------------------------------------------------
-- Instances
-- ------------------------------------------------------------------------

instance Eq User where
   (==) = mapEq oID

-- ------------------------------------------------------------------------
-- Accessing the data for a particular user
-- ------------------------------------------------------------------------

getUserEntry :: String -> IO (Maybe User)
getUserEntry uId =
   do
      users <- getUserEntrys
      case (findJust
            (\ user -> if userId user == uId then Just user else Nothing)
            users
            ) of
         Just user0 ->
            do
               oID <- newObject
               return (Just (user0 {oID = oID}))
         Nothing -> return Nothing

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
                  other = other,
                  oID = error "PasswordFile.oID1"
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
       always <- readIORef alwaysAllowAdminFlag
       if always
          then
             return True
          else
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

-- | Create a 'User' (this is used for the internal server)
createUser :: String -> IO User
createUser userId =
   do
      adminMVar <- newMVar False
      oID <- newObject
      return (User {
         userId = userId,
         encryptedPassword = "",
         adminMVar = adminMVar,
         other = "",
         oID = oID
         })

-- ------------------------------------------------------------------------
-- Admin management for the internal server.
-- ------------------------------------------------------------------------

-- | alwaysAllowAdmin should only be called for the internal server.
alwaysAllowAdmin :: IO ()
alwaysAllowAdmin = writeIORef alwaysAllowAdminFlag True

alwaysAllowAdminFlag :: IORef Bool
alwaysAllowAdminFlag = unsafePerformIO (newIORef False)
{-# NOINLINE alwaysAllowAdminFlag #-}
