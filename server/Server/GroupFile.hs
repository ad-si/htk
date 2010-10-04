-- | This module controls the
module Server.GroupFile(
   GroupOrUser(..),
   GroupFile,
   groupOrUserParser,
   unparseGroupOrUser,
   getGroupFile,
   userIsInGroup,
   extractInfoForUser,
   listContainingGroups,
   ) where

import System.IO.Error as IO
import Data.Char
import System.Time
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe
import Control.Concurrent.MVar
import System.Directory

import Util.IOExtras
import Util.BinaryAll
import Util.WBFiles
import Util.Messages

import Text.ParserCombinators.Parsec

-- -----------------------------------------------------------------------
-- data types
-- -----------------------------------------------------------------------

-- | Each list corresponds to one group, and contains the individual
-- users and other groups contained in that group.
--
-- Each list is represented by a line in the group file of the form
--    group1:#user1,#user2,group2
-- No spaces are permitted in such a line, except at the end.
--
-- Comment lines are also permitted.  They must begin
-- with a "%" character.  The rest of the text in the line is ignored.
--

-- | This is the type corresponding to the GroupFile as initially read in.
newtype GroupFile1 = GroupFile1 [GroupFileEntry] deriving (Show)

type GroupFileEntry = (String,[GroupOrUser])

-- | This is a type containing the GroupFile after it has been indexed.
-- Each group or user maps to the set of names of groups which explicitly
-- include it.
newtype GroupFile = GroupFile GroupFileMap

type GroupFileMap = Map.Map GroupOrUser (Set String)

data GroupOrUser =
      User String -- ^ User with this identifier
   |  Group String -- ^ Group with this identifier
   deriving (Eq,Ord,Show)


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

type GroupFileMapTransmit = [(GroupOrUser,[String])]

mapGroupFile :: GroupFile -> GroupFileMapTransmit
mapGroupFile (GroupFile fm) =
   let
      list1 :: [(GroupOrUser,Set String)]
      list1 = Map.toList fm

      list2 :: [(GroupOrUser,[String])]
      list2 = fmap (\ (gOrU,set) -> (gOrU,Set.toList set)) list1
   in
      list2

unmapGroupFile :: GroupFileMapTransmit -> GroupFile
unmapGroupFile list1 =
   let
      list2 :: [(GroupOrUser,Set String)]
      list2 = fmap (\ (gOrU,list) -> (gOrU,Set.fromList list)) list1
   in
      GroupFile (Map.fromList list2)

instance Monad m => HasBinary GroupFile m where
   writeBin = mapWrite mapGroupFile
   readBin = mapRead unmapGroupFile

-- -----------------------------------------------------------------------
-- Index the group file
-- -----------------------------------------------------------------------

indexGroupFile :: GroupFile1 -> GroupFile
indexGroupFile (GroupFile1 groupList) =
   let
      fm :: GroupFileMap
      fm = foldl foldFn1 Map.empty groupList

      foldFn1 :: GroupFileMap -> GroupFileEntry -> GroupFileMap
      foldFn1 fm0 (groupName,toAddList) =
         foldl (foldFn2 groupName) fm0 toAddList

      foldFn2 :: String -> GroupFileMap -> GroupOrUser -> GroupFileMap
      foldFn2 groupName fm0 toAdd =
         let
            set0 = Map.findWithDefault Set.empty toAdd fm0
            set1 = Set.insert groupName set0
            fm1 = Map.insert toAdd set1 fm0
         in
            fm1
   in
      GroupFile fm

-- -----------------------------------------------------------------------
-- Answering queries
-- -----------------------------------------------------------------------

-- | Returns 'True' if the user is in the group
userIsInGroup ::
   GroupFile
   -> String -- ^ user name
   -> String -- ^ group name
   -> Bool
userIsInGroup groupFile user targetGroup =
      not (isJust (scanContainingGroups (User user) Set.empty))
   where
      -- we maintain a (Set String) containing those groups already
      -- being searched or which have already been searched, to stop us
      -- getting into endless loops (in case of mutually self-including
      -- groups) or wasting time going down known blind alleys.
      scanContainingGroups :: GroupOrUser -> Set String -> Maybe (Set String)
      scanContainingGroups groupOrUser visitedSet0 =
         let
            containingGroups :: Set String
            containingGroups = findContainingGroups groupFile groupOrUser
         in
            if Set.member targetGroup containingGroups
               then
                  Nothing
               else
                  let
                     containingGroupsList :: [String]
                     containingGroupsList = Set.toList containingGroups

                     scanList :: [String] -> Set String
                        -> Maybe (Set String)
                     scanList [] visitedSet = Just visitedSet
                     scanList (group:groups) visitedSet0 =
                        case groupIsIn group visitedSet0 of
                           Nothing -> Nothing
                           Just visitedSet1 -> scanList groups visitedSet1
                  in
                     scanList containingGroupsList visitedSet0

      groupIsIn :: String -> Set String -> Maybe (Set String)
         -- groupIsIn returns (Just set) if the group is not contained
         -- in 'targetGroup', otherwise Nothing.
      groupIsIn group visitedSet0 =
         if Set.member group visitedSet0
            then
               Just visitedSet0
            else
               let
                  visitedSet1 :: Set String
                  visitedSet1 = Set.insert group visitedSet0
               in
                  scanContainingGroups (Group group) visitedSet1

-- | Extract that portion of the group file this user is permitted to
-- see.  (In effect, the list of those groups it belongs to)
extractInfoForUser :: GroupFile -> String -> GroupFile
extractInfoForUser groupFile user =
   let
      containingGroups = extractAllContainingGroups groupFile user
   in
      GroupFile (Map.singleton (User user) containingGroups)


-- | List groups containing this user.
listContainingGroups :: GroupFile -> String -> [String]
listContainingGroups groupFile user
   = Set.toList (extractAllContainingGroups groupFile user)

extractAllContainingGroups :: GroupFile -> String -> Set String
extractAllContainingGroups groupFile user = extract1 (User user) Set.empty
   where
      -- We maintain an accumulating parameter containing those
      -- groups found so far
      extract1 :: GroupOrUser -> Set String -> Set String
      extract1 groupOrUser foundSoFar0 =
         let
            containingGroups :: Set String
            containingGroups = findContainingGroups groupFile groupOrUser
         in
            scanList (Set.toList containingGroups) foundSoFar0

      extract :: String -> Set String -> Set String
      extract group foundSoFar0 =
         if Set.member group foundSoFar0
            then
               foundSoFar0
            else
               let
                  foundSoFar1 = Set.insert group foundSoFar0
               in
                  extract1 (Group group) foundSoFar1

      scanList :: [String] -> Set String -> Set String
      scanList [] foundSoFar0 = foundSoFar0
      scanList (group : groups) foundSoFar0 =
         scanList groups (extract group foundSoFar0)

findContainingGroups :: GroupFile -> GroupOrUser -> Set String
findContainingGroups (GroupFile fm) gOrU
   = Map.findWithDefault Set.empty gOrU fm



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
               modificationTimeOpt
                  <- catchDoesNotExist (getModificationTime groupFileName)
               case modificationTimeOpt of
                  Just modificationTime
                     | modificationTime /= lastModificationTime groupFileData0
                     -> do
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
                     | True ->
                        return (
                           groupFileData0 {lastChecked = currentClockTime},
                           groupFile groupFileData0)
                  Nothing ->
                     let
                        emptyGroupFile = GroupFile Map.empty

                        nullGroupFileData1 = GroupFileData {
                           lastChecked = currentClockTime,
                           lastModificationTime = initialClockTime,
                           groupFile = emptyGroupFile
                           }
                     in
                        return (nullGroupFileData1,emptyGroupFile)
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
      groupFileEntryOpts <- sepBy lineParser (char '\n')
      let
         groupFile1 = GroupFile1 (catMaybes groupFileEntryOpts)
      return (indexGroupFile groupFile1)

lineParser :: Parser (Maybe GroupFileEntry)
lineParser =
      (do
         groupFileEntry <- groupFileEntryParser
         return (Just groupFileEntry)
      )
   <|>(do
         commentParser
         return Nothing
      )
   <|>(do
         trailingSpaces
         return Nothing
      )

groupFileEntryParser :: Parser GroupFileEntry
groupFileEntryParser =
   do
      groupName <- nameParser
      char ':'
      groupEntries <- sepBy groupOrUserParser (char ',')
      trailingSpaces
      return (groupName,groupEntries)

commentParser :: Parser ()
commentParser =
   do
      char '%'
      skipMany (satisfy (/= '\n'))


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
nameParser = many1 (satisfy isAlphaNum)

-- | Allow trailing spaces
trailingSpaces :: Parser ()
trailingSpaces = skipMany (satisfy (\ ch -> isSpace ch && ch /= '\n'))
