{- This module contains the code for reading the password file and
   checking users against it.

   The password file is re-read for each authentication request.

   The password file has the following format:

user:encrypted password:status:other data:...

   Current valid values for status are "ADMIN" or "".

   -}
module PasswordFile(
   User(..), -- information for a particular user
   getUserEntry, -- :: String -> IO (Maybe User)
      -- get that information, given the user id.
   ) where

import IO
import Maybe

import Computation
import ExtendedPrelude
import WBFiles


-- ------------------------------------------------------------------------
-- The User datatype.
-- ------------------------------------------------------------------------

data User = User {
   userId :: String,
   encryptedPassword :: String,
   isAdmin :: Bool,
   other :: String
   }

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
         userWEs = map parseLine userLines
      userOpts <- mapM
         (\ userWE -> case fromWithError userWE of
            Right user -> return (Just user)
            Left error ->
               do
                  putStrLn error
                  return Nothing
            )
         userWEs
      let
         users = catMaybes userOpts
      hClose passwd
      return users

parseLine :: String -> WithError User
parseLine userLine =
   case splitByChar ':' userLine of
      userId:encryptedPassword:status:other:_ ->
         hasValue (User {
            userId = userId,
            encryptedPassword = encryptedPassword,
            isAdmin = (status == "ADMIN"),
            other = other
            })
      _ -> hasError (
          "Couldn't parse line in server password file: "++userLine)

