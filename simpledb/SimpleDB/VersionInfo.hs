-- | This module maintains the extra information that is maintained by the
-- server about each version, such as parent versions, user-id of
-- creator, and so on.
module SimpleDB.VersionInfo(
   ObjectVersion(..),
   -- firstVersion points to the first version that will be
   -- allocated.
   firstVersion,

   UserInfo(..),
   ServerInfo(..),
   VersionInfo(..),

   -- Functions for manipulating VersionAttributes,

   VersionAttributes,
   lookupVersionAttribute, -- :: VersionAttributes -> String -> Maybe String
   setVersionAttribute,
       -- :: VersionAttributes -> String -> String -> VersionAttributes
   emptyVersionAttributes, -- :: VersionAttributes
   exportVersionAttributes, -- :: VersionAttributes -> [(String,String)]
   importVersionAttributes, -- :: [(String,String)] -> VersionAttributes



   --
   -- Server-side interface.
   --

   mkVersionInfo,
      -- :: VersionState -> PasswordFile.User
      -- -> Either UserInfo VersionInfo -> IO VersionInfo
      -- Fill out the files in a VersionInfo, checking that the user is
      -- allowed to check in this version.

   changeUserInfo,
      -- :: PasswordFile.User -> VersionInfo -> UserInfo
      -- -> WithError VersionInfo
      -- Modify the UserInfo for a VersionInfo, checking that the user is
      -- allowed to do this.

   commitVersionInfo,
      -- :: VersionInfo -> Maybe VersionInfo
      -- Mark VersionInfo as committed, or return Nothing if it already is.

   changeVersionInfo,
      -- :: PasswordFile.User -> VersionInfo -> VersionInfo
      -- -> WithError VersionInfo
      -- Modify a VersionInfo, checking that the user is
      -- allowed to do this.  (We do not allow system data to be changed.)

   cleanVersionInfo,
      -- :: VersionInfo -> VersionInfo
      -- Clear settable fields (used on checkout).
   --
   -- Client-side interface
   --

   topVersionInfo, -- :: VersionInfo
   displayVersionInfo, -- :: Bool -> VersionInfo -> IO ()
      -- If the Bool is set we provide a button allowing the user
      -- to see the extra system parameters.

      -- It only returns a UserInfo, since the rest of a
      -- VersionInfo is uneditable.
   editVersionInfo, -- :: String -> VersionInfo -> IO (Maybe UserInfo)

   -- comparing VersionInfo's globally uniquely.
   VersionInfoKey, -- instance of Ord
   mapVersionInfo, -- :: VersionInfo -> VersionInfoKey


   checkVersionInfoFormat,
      -- :: String -> WithError CompiledFormatString
      -- Compile a format string, checking at the same time that it only uses
      -- format letters we know about.

   evalVersionInfoFormat,
      -- :: CompiledFormatString -> VersionInfo -> IO String
      -- Display a VersionInfo according to the given format (which should have
      -- been produced with checkVersionInfoFormat)

   getVersionInteger,
      -- :: ObjectVersion -> Integer
      -- A function you should avoid using if at all possible.

   ) where

import Time
import List (intersperse)

import Data.Typeable
import Util.DeprecatedFiniteMap

import Util.Computation
import Util.BinaryAll
import Util.ExtendedPrelude
import Util.Dynamics
import Util.AtomString(StringClass(..))
import Util.CommandStringSub
import Util.Messages

import HTk.Toolkit.DialogWin
import HTk.Toolkit.SimpleForm
import HTk.Toolkit.MarkupText
import HTk.Toplevel.HTk(text,height,width,value,background)

import qualified Server.PasswordFile as PasswordFile

import SimpleDB.ServerErrors
import SimpleDB.ObjectVersion
import {-# SOURCE #-} SimpleDB.VersionState


-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------
{-
newtype ObjectVersion = ObjectVersion Integer
   deriving (Eq,Ord,Typeable,DeepSeq,Enum)
   -- Type of handle referring to a version.
-}

firstVersion :: ObjectVersion
firstVersion = ObjectVersion 1

-- | The information that must be specified on each commit.
data UserInfo = UserInfo {
   label :: String,
      -- ^ Label for this version in the graph.
   contents :: String,
      -- ^ More detailed text about this version
   version :: ObjectVersion,
      -- ^ The objectVersion for this commit.
   parents :: [ObjectVersion],
      -- ^ Parents of this version.  The first parent, if any, is special
      -- as on commit we describe the entries by their difference from
      -- this first parent.
   versionAttributes :: VersionAttributes
      -- ^ Version attributes.
   } deriving Show

-- | miscellaneous attributes.
-- At the moment the only envisaged use for these is for the deletion
-- count, but we provide more for so the format can be extended easily.
newtype VersionAttributes = VersionAttributes (FiniteMap String String)

-- | The information that can either be automatically constructed by the
-- server, or specified on commit.  Once constructed the ServerInfo value
-- is not supposed to change, even when we transmit the version to another
-- server.  Thus it provides, we hope, a reliable way of globally (for
-- all MMiSS servers in the world) distinguishing between different versions.
data ServerInfo = ServerInfo {
   serverId :: String,
      -- ^ Globally unique id for the originating server, as generated by
      -- WBFiles.getServerId.
   serialNo :: Integer,
      -- ^ Integer which should be unique for this server.  We originally
      -- get this from the version number on the first checkin.
   timeStamp :: ClockTime,
      -- ^ When this version was first checked in.
   userId :: String -- identifier of the committing user.
   } deriving (Show,Eq,Ord)

data VersionInfo = VersionInfo {
   user :: UserInfo,
   server :: ServerInfo,
   isPresent :: Bool
      -- If True, means that an attempt to retrieve this version will
      -- succeed.
      --
      -- This field is under the control of the SimpleDB server.  Its setting
      -- when sent to the server (in a VersionInfo1 constructor, for example)
      -- will be ignored.
   } deriving (Show,Typeable)

-- ----------------------------------------------------------------------
-- Instances of Show, StringClass
-- ----------------------------------------------------------------------

instance Show ObjectVersion where
   showsPrec n (ObjectVersion v) acc = "[" ++ show v ++ "]" ++ acc

instance StringClass ObjectVersion where
   toString (ObjectVersion v) = show v
   fromStringWE s =
      case readCheck s of
         Just i -> hasValue (ObjectVersion i)
         Nothing -> hasError "Can't parse ObjectVersion - must be a number"

instance Show VersionAttributes where
   showsPrec n (VersionAttributes fm) acc =
      let
         l :: [(String,String)]
         l = fmToList fm

         kvs :: [String]
         kvs = map (\ (k,v) -> k ++ "=" ++ v) l
      in
         (concat (intersperse " " kvs)) ++ acc

-- ----------------------------------------------------------------------
-- Getting out the underlying integer
-- ----------------------------------------------------------------------

getVersionInteger :: ObjectVersion -> Integer
getVersionInteger (ObjectVersion i) = i
{-# DEPRECATED getVersionInteger
   "because it ignores the tree structure of versions" #-}

-- ----------------------------------------------------------------------
-- Instances of HasBinary
-- ----------------------------------------------------------------------

instance Monad m => HasBinary ObjectVersion m where
   writeBin = mapWrite (\ (ObjectVersion i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (ObjectVersion i))

instance Monad m => HasBinary UserInfo m where
   writeBin = mapWrite
      (\ (UserInfo {label = label,contents = contents,
            version = version,parents = parents,
            versionAttributes = versionAttributes})
         ->
         (label,contents,version,parents,versionAttributes)
         )
   readBin = mapRead
      (\ (label,contents,version,parents,versionAttributes)
         ->
         (UserInfo {label = label,contents = contents,
            version = version,parents = parents,
            versionAttributes = versionAttributes})
         )

instance Monad m => HasBinary ServerInfo m where
   writeBin = mapWrite
      (\ (ServerInfo {serverId = serverId,serialNo = serialNo,
            timeStamp = timeStamp,userId = userId})
         ->
         (serverId,serialNo,timeStamp,userId)
         )
   readBin = mapRead
      (\ (serverId,serialNo,timeStamp,userId)
         ->
         (ServerInfo {serverId = serverId,serialNo = serialNo,
            timeStamp = timeStamp,userId = userId})
         )

instance Monad m => HasBinary VersionInfo m where
   writeBin = mapWrite
      (\ (VersionInfo {user = user,server = server,isPresent = isPresent})
         ->
         (user,server,isPresent)
         )
   readBin = mapRead
      (\ (user,server,isPresent)
         ->
         (VersionInfo {user = user,server = server,isPresent = isPresent})
         )

instance Monad m => HasBinary VersionAttributes m where
   writeBin = mapWrite exportVersionAttributes
   readBin = mapRead importVersionAttributes

-- ----------------------------------------------------------------------
-- Comparing VersionInfo in a globally unique way, assuming the
-- client doesn't rig the VersionInfos.
-- ----------------------------------------------------------------------

type VersionInfoKey = ServerInfo -- must be instance of Ord

mapVersionInfo :: VersionInfo -> VersionInfoKey
mapVersionInfo versionInfo = server versionInfo
   -- The userId is necessary as a security measure, since otherwise
   --    a user could create a version with whatever id he liked and
   --    then effectively overwrite someone else's version.  The timeStamp
   --    guards against the possibility of a server being restarted and
   --    new serial-numbers generated.


-- ----------------------------------------------------------------------
-- Instance of Eq and Ord.  This version will only be unique for
-- the repository.  But then it really will be unique, whatever the
-- client does.
-- ----------------------------------------------------------------------

instance Eq UserInfo where
   (==) = mapEq version

instance Ord UserInfo where
   compare = mapOrd version

instance Eq VersionInfo where
   (==) = mapEq user

instance Ord VersionInfo where
   compare = mapOrd user

-- ----------------------------------------------------------------------
-- Instances of Eq and Ord where we don't cheat
-- ----------------------------------------------------------------------

instance Eq VersionAttributes where
   (==) = mapEq (\ (VersionAttributes fm) -> fmToList fm)

instance Ord VersionAttributes where
   compare = mapOrd (\ (VersionAttributes fm) -> fmToList fm)


userInfoMap :: Full UserInfo -> (String,String,ObjectVersion,[ObjectVersion],
   VersionAttributes)
userInfoMap (Full user)
   = (label user,contents user,version user,parents user,
      versionAttributes user)

instance Eq (Full UserInfo) where
   (==) = mapEq userInfoMap

instance Ord (Full UserInfo) where
   compare = mapOrd userInfoMap


serverInfoMap :: Full ServerInfo -> (String,Integer,ClockTime,String)
serverInfoMap (Full serverInfo)
   = (serverId serverInfo,serialNo serverInfo,timeStamp serverInfo,
      userId serverInfo)

instance Eq (Full ServerInfo) where
   (==) = mapEq serverInfoMap

instance Ord (Full ServerInfo) where
   compare = mapOrd serverInfoMap

versionInfoMap :: Full VersionInfo -> (Full UserInfo,Full ServerInfo,Bool)
versionInfoMap (Full versionInfo) =
   (Full (user versionInfo),Full (server versionInfo),isPresent versionInfo)

instance Eq (Full VersionInfo) where
   (==) = mapEq versionInfoMap

instance Ord (Full VersionInfo) where
   compare = mapOrd versionInfoMap


-- ----------------------------------------------------------------------
-- Code for filling in the ServerInfo, if necessary, also checking
-- that the user meets the appropriate criteria.
-- We set isPresent to be False if necessary.  To set this to True,
-- the caller (SimpleDBServer) needs to use commitVersionInfo on commit.
-- ----------------------------------------------------------------------

mkVersionInfo :: VersionState -> PasswordFile.User
   -> Either UserInfo VersionInfo -> IO VersionInfo
mkVersionInfo versionState user (Left versionUser) =
   do
      timeStamp <- getClockTime
      let
         ObjectVersion serialNo = version versionUser
         userId = PasswordFile.userId user

         versionServer = ServerInfo {
            serverId = (thisServerId versionState),
            serialNo = serialNo,
            timeStamp = timeStamp,
            userId = userId
            }

         versionInfo = VersionInfo {
            user = versionUser,
            server = versionServer,
            isPresent = False
            }

      return versionInfo
mkVersionInfo versionState user (Right versionInfo) =
   do
      let
         thisUser = userId (server versionInfo)

      if thisUser == PasswordFile.userId user
         then
            done
         else
            do
               isAdmin <- PasswordFile.getAdminStatus user
               if isAdmin
                  then
                     done
                  else
                     throwError AccessError
                        "This version does not belong to you"
      return (versionInfo {isPresent = False})

-- Returns Nothing if the version is already committed.
commitVersionInfo :: VersionInfo -> Maybe VersionInfo
commitVersionInfo versionInfo0 =
   if isPresent versionInfo0
      then
         Nothing
      else
         Just (versionInfo0 {isPresent = True})

changeUserInfo :: PasswordFile.User -> VersionInfo -> UserInfo
   -> IO VersionInfo
changeUserInfo user1 versionInfo userInfo =
   return (versionInfo {user = userInfo})

-- | Modify a VersionInfo, checking that the user is
-- allowed to do this.  (We do not allow system data to be changed.)
changeVersionInfo :: PasswordFile.User -> VersionInfo -> VersionInfo
   -> IO VersionInfo
changeVersionInfo user1 versionInfo0 versionInfo1 =
   do
      let
         server0 = server versionInfo0
         server1 = server versionInfo1
      if server0 /= server1
         then
            throwError MiscError
               "Cannot change fundamental system parameters of version"
         else
            changeUserInfo user1 versionInfo0 (user versionInfo1)

-- ----------------------------------------------------------------------
-- Creating, Viewing and Editing the VersionInfo
-- ----------------------------------------------------------------------

topVersionInfo :: VersionInfo
   -- this is the VersionInfo attached to the top version.
   -- Note that since the userId is "", this will mean the server can only be
   -- initialised (and top version added) by ADMIN users (or users called
   -- "" of course).
topVersionInfo = VersionInfo {
   user = UserInfo {
      label = "EMPTY",
      contents = "Initial empty version",
      version = firstVersion,
      parents = [],
      versionAttributes = emptyVersionAttributes
      },
   server = ServerInfo {
      serverId = "",
      serialNo = 0,
      timeStamp = initialClockTime,
      userId = ""
      },
   isPresent = False
      -- as indeed it is when the repository is initialised, the only time
      -- topVersionInfo is used.
   }



displayVersionInfo :: Bool -> VersionInfo -> IO ()
displayVersionInfo allowSystem versionInfo =
   do
      let
         user1 = user versionInfo
         server1 = server versionInfo

         scroll :: [MarkupText] -> [Config (Dialog a)]
         scroll markups = [new [scrollMarkupText (39,6) markups]]

         b :: String -> MarkupText
         b str = bold [prose str]

         n :: String -> MarkupText
         n str = prose str

      showSystem <- createDialogWin'
         (("Quit",False) :
             if allowSystem
                then
                   [("Show System Info",True)]
                else
                   []
            )
         Nothing
         (scroll [
            b (label user1),newline,
            n (contents user1),newline,
            n ("Attributes: " ++ show (versionAttributes user1)),newline
            ])
         [text "Version Info"]

      let
         vToS :: ObjectVersion -> String
         vToS (ObjectVersion v) = show v

         vsToS :: [ObjectVersion] -> String
         vsToS [] = ""
         vsToS vs = unsplitByChar ' ' (map vToS vs)

      when showSystem
         (do
            calendarTime <- toCalendarTime (timeStamp server1)

            createDialogWin'
               [("Quit",())]
               Nothing
               (scroll [
                  b "Version: ",n (vToS (version user1)),newline,
                  b "Parents: ",n (vsToS (parents user1)),newline,
                  b "ServerId: ",n (serverId server1),newline,
                  b "Serial No: ",n (show (serialNo server1)),newline,
                  b "Created by: " ,n (userId server1),newline,
                  b "Created on: ", n (calendarTimeToString calendarTime)
                  ])
               [text "Version System Info"]
               )



-- cleanVersionInfo is used to clear user-settable fields when a version
-- is checked out.
cleanVersionInfo :: VersionInfo -> VersionInfo
cleanVersionInfo versionInfo0 =
   let
      user0 = user versionInfo0
      user1 = user0 {label = "",contents = "",
         versionAttributes = emptyVersionAttributes}
      versionInfo1 = versionInfo0 {user = user1}
   in
      versionInfo1

-- editVersionInfo allows the user to edit the things a user is normally
-- allowed to edit for a VersionInfo.
-- (It returns Nothing if the user cancels.)
-- The String argument gives the title to use.
--
-- It only returns a UserInfo, since the rest of a
-- VersionInfo is uneditable.
--
-- In no case do we allow the user to edit the version attributes,
-- because I can't be bothered.
editVersionInfo :: String -> VersionInfo -> IO (Maybe UserInfo)
editVersionInfo text versionInfo =
   do
      htkPres <- htkPresent
      (if htkPres then editVersionInfoGraphic else editVersionInfoText)
         text versionInfo

editVersionInfoText :: String -> VersionInfo -> IO (Maybe UserInfo)
editVersionInfoText title versionInfo =
   do
      let
         user0 = user versionInfo
         label0 = label user0
         contents0 = contents user0

      messageMess title
      label1 <- textQuery ("New title, or just hit ENTER for " ++ show label0)
      let
         label2 = if label1 == "" then label0 else label1

      contents1 <- textQuery ("New description, or just hit ENTER for "
         ++ show contents0)
      let
         contents2 = if contents1 == "" then contents0 else contents1

         user1 = user0 {label = label2,contents=contents2}

      return (Just user1)



editVersionInfoGraphic :: String -> VersionInfo -> IO (Maybe UserInfo)
editVersionInfoGraphic title versionInfo =
   do
      let
         user0 = user versionInfo

         labelForm :: Form String
         labelForm = newFormEntry "Label" (label user0)

         contentsForm :: Form String
         contentsForm =
            fmap
               (\ ((),str) -> str)
               (   nullForm "Description:" //
                   editableTextForm
                      [height 10,width 40,background "white",
                         value (contents user0)]
                   )

      newInfoOpt <- doForm title
         (labelForm // contentsForm)

      return (
         case newInfoOpt of
            Nothing -> Nothing
            Just (label1,contents1) ->
               let
                  user1 = user0 {
                     label = label1,contents = contents1}
               in
                  Just user1
         )

-- ----------------------------------------------------------------------
-- VersionAttributes function
-- ----------------------------------------------------------------------

lookupVersionAttribute :: VersionAttributes -> String -> Maybe String
lookupVersionAttribute (VersionAttributes fm) key =
   lookupFM fm key

setVersionAttribute :: VersionAttributes -> String -> String
   -> VersionAttributes
setVersionAttribute (VersionAttributes fm) key value =
   VersionAttributes (addToFM fm key value)

emptyVersionAttributes :: VersionAttributes
emptyVersionAttributes = VersionAttributes emptyFM

exportVersionAttributes :: VersionAttributes -> [(String,String)]
exportVersionAttributes (VersionAttributes fm) = fmToList fm

importVersionAttributes :: [(String,String)] -> VersionAttributes
importVersionAttributes l = VersionAttributes (listToFM l)

-- ----------------------------------------------------------------------
-- We also support textual display of a VersionInfo using format letters
-- (the CommandStringSub method).  Here
--    %V is expanded to the version number
--    %L to the label
--    %C to the contents (detailed description)
--    %P to the parents (as a list of version numbers)
--    %U to the committing user
--    %T to the timestamp
--    %A to the version attributes
--
-- Example: a format of "%V %L %P\n" will give a short description of the
-- version (version number, label, and parents).  "%V %L %P %U %T\n%C\n" will
-- put the short information (version number, label, parents, user, timestamp)
-- on one line, then give the contents separately.
-- ----------------------------------------------------------------------

-- Compile a format string, checking at the same time that it only uses
-- format letters we know about.
checkVersionInfoFormat :: String -> WithError CompiledFormatString
checkVersionInfoFormat formatStr =
   let
      compileFormat1WE = compileFormatString formatStr

      testCalendarTime = CalendarTime {
         ctYear = 2004,ctMonth = January,ctDay = 7,ctHour = 20,ctMin = 47,
         ctSec = 50,ctPicosec = 0,ctWDay = Wednesday,ctYDay = 6,
         ctTZName = "CET", ctTZ = 3600,ctIsDST = False
         }
   in
      mapWithError'
         (\ compileFormat1 ->
            -- We test the format on topVersionInfo
            mapWithError
               (const compileFormat1)
               (evalVersionInfoFormatWE compileFormat1
                  topVersionInfo testCalendarTime)
            )
         compileFormat1WE

-- Display a VersionInfo according to the given format (which should have
-- been produced with checkVersionInfoFormat)
evalVersionInfoFormat :: CompiledFormatString -> VersionInfo -> IO String
evalVersionInfoFormat compileFormat versionInfo =
   do
      calendarTime <- toCalendarTime (timeStamp (server versionInfo))
      let
         stringWE
            = evalVersionInfoFormatWE compileFormat versionInfo calendarTime
      coerceWithErrorIO stringWE

-- Only used internally.
evalVersionInfoFormatWE :: CompiledFormatString -> VersionInfo
   -> CalendarTime -> WithError String
evalVersionInfoFormatWE compileFormat versionInfo calendarTime =
   let
      user1 = user versionInfo
      server1 = server versionInfo

      expander ch = case ch of
         'V' -> Just (vToS (version user1))
         'L' -> Just (label user1)
         'C' -> Just (contents user1)
         'P' -> Just (vsToS (parents user1))
         'U' -> Just (userId server1)
         'T' -> Just (calendarTimeToString calendarTime)
         'A' -> Just (show (versionAttributes user1))
         _ -> Nothing

      -- Functions stolen from displayVersionInfo function
      vToS :: ObjectVersion -> String
      vToS (ObjectVersion v) = show v

      vsToS :: [ObjectVersion] -> String
      vsToS [] = ""
      vsToS vs = unsplitByChar ' ' (map vToS vs)
   in
      runFormatString compileFormat expander
