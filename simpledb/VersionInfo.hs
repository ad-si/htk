{- This module maintains the extra information that is maintained by the
   server about each version, such as parent versions, user-id of
   creator, and so on. -}
module VersionInfo(
   ObjectVersion(..),
   -- firstVersion points to the first version that will be
   -- allocated.
   firstVersion,

   UserInfo(..),
   ServerInfo(..),
   VersionInfo(..),

   -- 
   -- Server-side interface.
   --

   mkVersionInfo,
      -- :: VersionState -> PasswordFile.User 
      -- -> Either UserInfo VersionInfo -> IO (WithError VersionInfo)
      -- :: PasswordFile.User -> Either UserInfo VersionInfo 
      -- -> IO (WithError VersionInfo)
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

   VersionState,

   mkVersionState, -- :: Bool -> IO VersionState
      -- The Bool should be True for an internal server, False otherwise.
   addVersionInfo, 
      -- :: VersionState -> VersionInfo -> IO ()
      -- Modify the VersionInfos. 
   lookupVersionInfo,
      -- :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)
   lookupServerInfo,
      -- :: VersionState -> ServerInfo -> IO (Maybe ObjectVersion) 

   getVersionInfos, -- :: VersionState -> IO [VersionInfo]
      -- get all the version infos, in undefined order.


   registerAct, -- :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
      -- Register an action to be done each time we add a new versionInfo.
      -- NB.  The action must always execute its argument exactly once.


   registerAndGet, 
      -- :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) 
      -- -> IO [VersionInfo]
      -- Combine getVersionInfos and registerAct, so that even with 
      -- concurrency, we can be sure no other updates will be allowed to get 
      -- inbetween the two actions.

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
   
   ) where

import IO
import Time
import Maybe

import System.IO.Unsafe
import Data.IORef
import Data.FiniteMap
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Computation
import BinaryAll
import WBFiles
import ExtendedPrelude
import Dynamics
import HostName
import Posix(getProcessID)
import AtomString(StringClass(..))
import CommandStringSub
import Messages
import DeepSeq

import DialogWin
import SimpleForm
import MarkupText
import HTk(text,height,width,value,background)

import qualified PasswordFile

import LogFile

-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------

newtype ObjectVersion = ObjectVersion Int deriving (Eq,Ord,Typeable,DeepSeq)
   -- Type of handle referring to a version.

firstVersion :: ObjectVersion
firstVersion = ObjectVersion 0

-- | The information that must be specified on each commit.
data UserInfo = UserInfo {
   label :: String, 
      -- ^ Label for this version in the graph.
   contents :: String, 
      -- ^ More detailed text about this version
   private :: Bool,
      -- ^ If set, do not send this version to other servers automatically.
   version :: ObjectVersion,
      -- ^ The objectVersion for this commit.
   parents :: [ObjectVersion]
      -- ^ Parents of this version.  The first parent, if any, is special
      -- as on commit we describe the entries by their difference from
      -- this first parent.
   } deriving Show

-- | The information that can either be automatically constructed by the
-- server, or specified on commit.  Once constructed the ServerInfo value
-- is not supposed to change, even when we transmit the version to another
-- server.  Thus it provides, we hope, a reliable way of globally (for
-- all MMiSS servers in the world) distinguishing between different versions.
data ServerInfo = ServerInfo {
   serverId :: String, 
      -- ^ Globally unique id for the originating server, as generated by
      -- WBFiles.getServerId.
   serialNo :: Int,
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

-- ----------------------------------------------------------------------
-- Instances of HasBinary
-- ----------------------------------------------------------------------
   
instance Monad m => HasBinary ObjectVersion m where
   writeBin = mapWrite (\ (ObjectVersion i) -> (Unsigned i))
   readBin = mapRead (\ (Unsigned i) -> (ObjectVersion i))
   
instance Monad m => HasBinary UserInfo m where
   writeBin = mapWrite 
      (\ (UserInfo {label = label,contents = contents,private = private,
            version = version,parents = parents}) 
         ->
         (label,contents,private,version,parents)
         )
   readBin = mapRead
      (\ (label,contents,private,version,parents) 
         ->
         (UserInfo {label = label,contents = contents,private = private,
            version = version,parents = parents}) 
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
-- Code for filling in the ServerInfo, if necessary, also checking
-- that the user meets the appropriate criteria.
-- We set isPresent to be False if necessary.  To set this to True,
-- the caller (SimpleDBServer) needs to use commitVersionInfo on commit.
-- ----------------------------------------------------------------------

mkVersionInfo :: VersionState -> PasswordFile.User 
   -> Either UserInfo VersionInfo -> IO (WithError VersionInfo)
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

      return (hasValue versionInfo)
mkVersionInfo versionState user (Right versionInfo) =
   return (
      let
         thisUser = userId (server versionInfo)
      in
         if thisUser == PasswordFile.userId user || PasswordFile.isAdmin user 
            then
               hasValue (versionInfo {isPresent = False})
            else
               hasError ("You cannot copy versions from " 
                  ++ thisUser ++ " into this repository.")
      )

-- Returns Nothing if the version is already committed.
commitVersionInfo :: VersionInfo -> Maybe VersionInfo
commitVersionInfo versionInfo0 =
   if isPresent versionInfo0
      then
         Nothing
      else 
         Just (versionInfo0 {isPresent = True})

changeUserInfo :: PasswordFile.User -> VersionInfo -> UserInfo 
   -> WithError VersionInfo
changeUserInfo user1 versionInfo userInfo =
   let
      thisUser = userId (server versionInfo)
   in
      if thisUser == PasswordFile.userId user1 || PasswordFile.isAdmin user1 
         then
            hasValue (versionInfo {user = userInfo})
         else
            hasError ("You cannot modify the version information for "
               ++ thisUser ++ ".")


-- | Modify a VersionInfo, checking that the user is
-- allowed to do this.  (We do not allow system data to be changed.)
changeVersionInfo :: PasswordFile.User -> VersionInfo -> VersionInfo 
   -> WithError VersionInfo
changeVersionInfo user1 versionInfo0 versionInfo1 =
   let
      server0 = server versionInfo0
      server1 = server versionInfo1
   in
      if server0 /= server1 
         then
            hasError ("Cannot change fundamental system parameters of version")
         else
            changeUserInfo user1 versionInfo0 (user versionInfo1)

-- ----------------------------------------------------------------------
-- Accessing the version data
-- ----------------------------------------------------------------------

data VersionState = VersionState {
   versionInfosRef :: MVar VersionInfosMaps,
      -- all VersionInfos so far.
      -- also used as a lock on various operations.
   versionInfoActRef :: IORef ( IO (Bool,VersionInfo) -> IO ()),
   logFile :: LogFile VersionInfo,
   thisServerId :: String
   }

data VersionInfosMaps = VersionInfosMaps {
   fromVersion :: FiniteMap ObjectVersion VersionInfo,
   fromServer :: FiniteMap ServerInfo ObjectVersion
   }

objectVersion :: VersionInfo -> ObjectVersion 
objectVersion = version . user

mkVersionState :: Bool -> IO VersionState
mkVersionState isInternal =
   do
      (logFile,versionInfosList) <- openLog "versionInfos"
      let
         -- we have to be careful here that later elements of the list override
         -- earlier ones.
         fromVersion1 = foldl
            (\ fromVersion0 versionInfo 
               -> addToFM fromVersion0 (objectVersion versionInfo) 
                  versionInfo
               )
            emptyFM
            versionInfosList

         fromServer1 = foldl
            (\ fromVersion0 versionInfo
               -> addToFM fromVersion0 (server versionInfo) 
                  (version (user versionInfo))
               )
            emptyFM
            versionInfosList

         versionInfosMaps = VersionInfosMaps {
            fromVersion = fromVersion1,
            fromServer = fromServer1
            }

      versionInfosRef <- newMVar versionInfosMaps
      versionInfoActRef <- newIORef (\ act 
         -> do
               act
               done
         )

      thisServerId <- mkServerId isInternal

      return (VersionState {
         versionInfosRef = versionInfosRef,
         versionInfoActRef = versionInfoActRef,
         logFile = logFile,
         thisServerId = thisServerId
         })

addVersionInfo :: VersionState -> VersionInfo -> IO ()
addVersionInfo versionState versionInfo =
   do
      -- We do everything inside the "External" action, thus ensuring that
      -- we do not conflict with any simultaneous client action.
      -- This also means we can be sure the access to versionInfosRef
      -- will not deadlock.
      versionInfoAct <- readIORef (versionInfoActRef versionState)
      versionInfoAct
         (do
            writeLog (logFile versionState) versionInfo
            isEdit <- modifyMVar (versionInfosRef versionState) 
               (\ versionInfosMaps0 ->
                  return (
                     let
                        objectVersion = version (user versionInfo)

                        fromVersion0 = fromVersion versionInfosMaps0

                        isEdit = isJust (lookupFM fromVersion0 objectVersion)

                        fromVersion1 = addToFM fromVersion0 
                           objectVersion versionInfo
                        fromServer1 = addToFM (fromServer versionInfosMaps0) 
                           (server versionInfo) objectVersion

                        versionInfoMaps1 = VersionInfosMaps {
                           fromVersion = fromVersion1,fromServer = fromServer1}
                     in
                        (versionInfoMaps1,isEdit)
                     )
                  )
            return (isEdit,versionInfo)
            )

lookupVersionInfo :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)
lookupVersionInfo versionState objectVersion =
   do
      versionInfosMaps <- readMVar (versionInfosRef versionState)
      return (lookupFM (fromVersion versionInfosMaps) objectVersion)   

lookupServerInfo :: VersionState -> ServerInfo -> IO (Maybe ObjectVersion) 
lookupServerInfo versionState serverInfo =
   do
      versionInfosMaps <- readMVar (versionInfosRef versionState)
      return (lookupFM (fromServer versionInfosMaps) serverInfo)   


getVersionInfos :: VersionState -> IO [VersionInfo]
getVersionInfos versionState = 
   do
      versionInfosMaps <- readMVar (versionInfosRef versionState)
      return (eltsFM (fromVersion versionInfosMaps))

registerAct :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
registerAct versionState actFn =
   modifyMVar_ (versionInfosRef versionState) 
      (\ versionInfosMaps0 ->
         do
            writeIORef (versionInfoActRef versionState) actFn
            return versionInfosMaps0
         )

registerAndGet :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> 
   IO [VersionInfo]
registerAndGet versionState actFn =
   modifyMVar (versionInfosRef versionState) 
      (\ versionInfosMaps0 ->
         do
            writeIORef (versionInfoActRef versionState) actFn
            return (versionInfosMaps0,eltsFM (fromVersion versionInfosMaps0))
         )



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
      private = False,
      version = firstVersion,
      parents = []
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
            n (if private user1 then "not for export" else "for export")
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
      user1 = user0 {label = "",contents = ""}
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

         privateForm :: Form Bool
         privateForm =
            let
               entry1 = ("Export",False)
               entry2 = ("Don't Export",True)
            in
               newFormOptionMenu2
                  (if private user0
                     then
                        [entry2,entry1]
                     else
                        [entry1,entry2]
                     )

      newInfoOpt <- doForm title
         (labelForm // contentsForm // privateForm)

      return (
         case newInfoOpt of
            Nothing -> Nothing
            Just (label1,(contents1,private1)) ->
               let
                  user1 = user0 {
                     label = label1,contents = contents1,private = private1}
               in
                  Just user1
         )

-- ----------------------------------------------------------------------
-- Generating a unique server identifier.
-- ----------------------------------------------------------------------

-- | The argument specifies whether the server is internal or not.
mkServerId :: Bool -> IO String
mkServerId isInternal =
   do
      serverIdOpt <- getServerId
      case serverIdOpt of
         Just serverId -> return serverId
         Nothing ->
            do
               fullHostName <- getFullHostName
               if isInternal
                  then
                     do
                        pID <- getProcessID
                        return(fullHostName ++ ":#" ++ show pID)
                  else
                     do
                        port <- getPort
                        return (if port == 11393
                           then
                              fullHostName
                           else
                              fullHostName ++ ":" ++ show port
                           )

-- ----------------------------------------------------------------------
-- We also support textual display of a VersionInfo using format letters
-- (the CommandStringSub method).  Here
--    %V is expanded to the version number
--    %L to the label
--    %C to the contents (detailed description)
--    %P to the parents (as a list of version numbers)
--    %U to the committing user
--    %T to the timestamp
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
         _ -> Nothing

      -- Functions stolen from displayVersionInfo function
      vToS :: ObjectVersion -> String
      vToS (ObjectVersion v) = show v

      vsToS :: [ObjectVersion] -> String
      vsToS [] = ""
      vsToS vs = unsplitByChar ' ' (map vToS vs)
   in
      runFormatString compileFormat expander