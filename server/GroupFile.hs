-- | This module controls the 
module GroupFile(
   GroupOrUser(..),
   groupOrUserParser,
   unparseGroupOrUser,
   getGroupFile,
   ) where

import IO
import Char
import Time
import Maybe

import System.IO.Unsafe
import Control.Concurrent.MVar
import System.Directory

import BinaryAll
import WBFiles
import Messages

import Text.ParserCombinators.Parsec

-- -----------------------------------------------------------------------
-- data types
-- -----------------------------------------------------------------------

-- | Each list corresponds to one group, and contains the individual
-- users and other groups contained in that group.
--
-- Each list is represented by a line in the group file of the form
-- group1:#user1,#user2,group2
-- Spaces (apart from separating newlines) are not permitted.
newtype GroupFile = GroupFile [(String,[GroupOrUser])] deriving (Show)

data GroupOrUser = 
      User String -- ^ User with this identifier 
   |  Group String -- ^ Group with this identifier
   deriving (Show)

-- -----------------------------------------------------------------------
-- Instances
-- -----------------------------------------------------------------------

instance Monad m => HasBinary GroupOrUser m where
   writeBin = mapWrite (\ gOrU -> case gOrU of
      User name -> Left name
      Group name -> Right name
      )
   readBin = mapRead (\ uOrG -> case uOrG of
      Left name -> User name
      Right name -> Group name
      )


-- -----------------------------------------------------------------------
-- Reading the Group File
-- This needs to be reread regularly.  The strategy we adopt is to check
-- the file's last modification time every 5 seconds.
-- -----------------------------------------------------------------------

howOftenCheck :: TimeDiff
howOftenCheck =
   TimeDiff {tdYear = 0,tdMonth = 0,tdDay = 0,tdHour = 0,
      tdMin = 0,tdSec = 5,tdPicosec = 0}

data GroupFileData = GroupFileData {
   lastChecked :: ClockTime,
   lastModificationTime :: ClockTime,
   groupFile :: GroupFile
   }

groupFileMVar :: MVar GroupFileData
groupFileMVar = unsafePerformIO (newMVar initialGroupFileData)
{-# NOINLINE initialGroupFileData #-}

initialGroupFileData :: GroupFileData
initialGroupFileData = GroupFileData {
   lastChecked = initialClockTime,
   lastModificationTime = initialClockTime,
   groupFile = error "Group file not initialised"
   }


getGroupFile :: IO GroupFile 
getGroupFile = modifyMVar groupFileMVar (\ groupFileData0 ->
   do
      currentClockTime <- getClockTime
      let
         checkClockTime = addToClockTime howOftenCheck
            (lastChecked groupFileData0) 
      if checkClockTime > currentClockTime
         then
            return (groupFileData0,groupFile groupFileData0)
         else
            do
               modificationTime <- getModificationTime groupFileName
               if modificationTime /= lastModificationTime groupFileData0
                  then
                     do
                        groupFileOpt <- getGroupFile1
                        let
                           groupFile1 = fromMaybe
                              (groupFile groupFileData0)
                              groupFileOpt
                        let
                           groupFileData1 = GroupFileData {
                              lastChecked = currentClockTime,
                              lastModificationTime = modificationTime,
                              groupFile = groupFile1
                              }
                        return (groupFileData1,groupFile1)
                  else
                     return (groupFileData0 {lastChecked = currentClockTime},
                        groupFile groupFileData0)
      )

getGroupFile1 :: IO (Maybe GroupFile)
getGroupFile1 =
   do
      errorOrGroupFile <- IO.try (parseFromFile groupFileParser groupFileName) 
      case errorOrGroupFile of       
         Left err1 ->
            do
               errorMess (show err1)
               return Nothing
         Right (Left err2) ->
            do
               errorMess (show err2)
               return Nothing
         Right (Right groupFile) -> return (Just groupFile)
  


groupFileName :: FilePath
groupFileName = unsafePerformIO getGroupFileName
{-# NOINLINE groupFileName #-}

getGroupFileName :: IO FilePath
getGroupFileName = getServerFile "groups"
               
-- -----------------------------------------------------------------------
-- Parsing
-- -----------------------------------------------------------------------

groupFileParser :: Parser GroupFile
groupFileParser =
   do
      groupFileEntries <- sepBy groupFileEntryParser (char '\n')
      return (GroupFile groupFileEntries)

groupFileEntryParser :: Parser (String,[GroupOrUser])
groupFileEntryParser =
   do
      groupName <- nameParser
      char ':'
      groupEntries <- sepBy groupOrUserParser (char ',')
      return (groupName,groupEntries)

unparseGroupOrUser :: GroupOrUser -> String
unparseGroupOrUser (User name) = '#' : name
unparseGroupOrUser (Group name) = name

-- | Group of user parser
groupOrUserParser :: Parser GroupOrUser
groupOrUserParser =
      (do
         char '#'
         name <- nameParser
         return (User name)
      )
   <|>(do
         name <- nameParser
         return (Group name)
      )

-- | User or group-id parser
nameParser :: Parser String
nameParser =
   do
      firstLetter <- satisfy isAlpha
      rest <- many (satisfy isAlphaNum)
      return (firstLetter:rest)
