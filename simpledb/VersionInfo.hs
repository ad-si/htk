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
      -- :: PasswordFile.User -> Either UserInfo VersionInfo 
      -- -> IO (WithError VersionInfo)
      -- Fill out the files in a VersionInfo, checking that the user is
      -- allowed to check in this version.

   changeUserInfo,
      -- :: PasswordFile.User -> VersionInfo -> UserInfo 
      -- -> WithError VersionInfo
      -- Modify the UserInfo for a VersionInfo, checking that the user is
      -- allowed to do this.

   cleanVersionInfo, 
      -- :: VersionInfo -> VersionInfo
      -- Clear settable fields (used on checkout).

   VersionState,
   mkVersionState, -- :: IO VersionState
   addVersionInfo, 
      -- :: VersionState -> (Bool,VersionInfo) -> IO ()
      -- Modify the VersionInfos.  The Bool should be set if and only if
      --    the given VersionInfo already exists.
   lookupVersionInfo,
      -- :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)

   getVersionInfos, -- :: VersionState -> IO [VersionInfo]
      -- get all the version infos, in undefined order.


   registerAct, -- :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
      -- Register an action to be done each time we add a new versionInfo.

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

   ) where

import IO
import Time

import System.IO.Unsafe
import Data.IORef
import Data.FiniteMap
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Computation
import BinaryIO
import WBFiles
import ExtendedPrelude
import Dynamics

import DialogWin
import SimpleForm
import HTk(text,height,width,value,background)

import qualified PasswordFile

import LogFile

-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------

newtype ObjectVersion = ObjectVersion Int deriving (Eq,Ord,HasBinaryIO)
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
   } deriving Show

data VersionInfo = VersionInfo {
   user :: UserInfo,
   server :: ServerInfo
   } deriving Show

-- ----------------------------------------------------------------------
-- Instances of Show
-- ----------------------------------------------------------------------

instance Show ObjectVersion where
   showsPrec n (ObjectVersion v) acc = "[" ++ show v ++ "]" ++ acc

-- ----------------------------------------------------------------------
-- Instances of HasBinaryIO
-- ----------------------------------------------------------------------
   
instance HasBinaryIO UserInfo where
   hPut = mapHPut 
      (\ (UserInfo {label = label,contents = contents,private = private,
            version = version,parents = parents}) 
         ->
         (label,contents,private,version,parents)
         )
   hGetIntWE = mapHGetIntWE 
      (\ (label,contents,private,version,parents) 
         ->
         (UserInfo {label = label,contents = contents,private = private,
            version = version,parents = parents}) 
         )

instance HasBinaryIO ServerInfo where
   hPut = mapHPut 
      (\ (ServerInfo {serverId = serverId,serialNo = serialNo,
            timeStamp = timeStamp,userId = userId})
         ->
         (serverId,serialNo,timeStamp,userId)
         )
   hGetIntWE = mapHGetIntWE 
      (\ (serverId,serialNo,timeStamp,userId)
         ->
         (ServerInfo {serverId = serverId,serialNo = serialNo,
            timeStamp = timeStamp,userId = userId})
         )

instance HasBinaryIO VersionInfo where
   hPut = mapHPut 
      (\ (VersionInfo {user = user,server = server})
         ->
         (user,server)
         )
   hGetIntWE = mapHGetIntWE 
      (\ (user,server)
         ->
         (VersionInfo {user = user,server = server})
         )

-- ----------------------------------------------------------------------
-- Instances of Eq and Ord
-- There are two comparison methods, one for ServerInfo, and one for 
-- UserInfo/VersionInfo.
--
-- The one for ServerInfo should be globally unique, but the user can
--    subvert this by committing versions with explicit values which override
--    those of an existing version, by hacking the Haskell code for the
--    Workbench (for example).  
-- The one for UserInfo/VersionInfo is only unique for this server, on the
--    other hand it is guaranteed to be really unique, whatever the user does.
-- ----------------------------------------------------------------------

mapServerInfo :: ServerInfo -> (Int,String)
mapServerInfo serverInfo = (serialNo serverInfo,serverId serverInfo)

instance Eq ServerInfo where
   (==) = mapEq mapServerInfo

instance Ord ServerInfo where
   compare = mapOrd mapServerInfo

instance Eq UserInfo where
   (==) = mapEq version

instance Ord UserInfo where
   compare = mapOrd version

instance Eq VersionInfo where
   (==) = mapEq user

instance Ord VersionInfo where
   compare = mapOrd user

-- ----------------------------------------------------------------------
-- Instances of Typeable
-- ----------------------------------------------------------------------

versionInfo_tyRep = mkTyRep "VersionInfo" "VersionInfo"

instance HasTyRep VersionInfo where
   tyRep _ = versionInfo_tyRep

-- ----------------------------------------------------------------------
-- Code for filling in the ServerInfo, if necessary, also checking
-- that the user meets the appropriate criteria.
-- ----------------------------------------------------------------------

mkVersionInfo :: PasswordFile.User -> Either UserInfo VersionInfo 
   -> IO (WithError VersionInfo)
mkVersionInfo user (Left versionUser) =
   do
      timeStamp <- getClockTime
      let
         ObjectVersion serialNo = version versionUser
         userId = PasswordFile.userId user

         versionServer = ServerInfo {
            serverId = thisServerId,
            serialNo = serialNo,
            timeStamp = timeStamp,
            userId = userId
            }

         versionInfo = VersionInfo {
            user = versionUser,
            server = versionServer
            }

      return (hasValue versionInfo)
mkVersionInfo user (Right versionInfo) =
   return (
      let
         thisUser = userId (server versionInfo)
      in
         if thisUser == PasswordFile.userId user || PasswordFile.isAdmin user 
            then
               hasValue versionInfo
            else
               hasError ("You cannot copy versions from " 
                  ++ thisUser ++ " into this repository.")
      )

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

thisServerId :: String
thisServerId = unsafePerformIO getServerId
{-# NOINLINE thisServerId #-}

-- ----------------------------------------------------------------------
-- Accessing the version data
-- ----------------------------------------------------------------------

data VersionState = VersionState {
   versionInfosRef :: MVar (FiniteMap ObjectVersion VersionInfo),
      -- all VersionInfos so far.
      -- also used as a lock on various operations.
   versionInfoActRef :: IORef ( IO (Bool,VersionInfo) -> IO ()),
   logFile :: LogFile VersionInfo
   }

objectVersion :: VersionInfo -> ObjectVersion 
objectVersion = version . user

mkVersionState :: IO VersionState
mkVersionState =
   do
      (logFile,versionInfosList) <- openLog "versionInfos"
      let
         -- we have to be careful here that later elements of the list override
         -- earlier ones.
         versionInfos = foldl
            (\ versionInfos0 versionInfo 
               -> addToFM versionInfos0 (objectVersion versionInfo) 
                  versionInfo
               )
            emptyFM
            versionInfosList

      versionInfosRef <- newMVar versionInfos
      versionInfoActRef <- newIORef (\ _ -> done)

      return (VersionState {
         versionInfosRef = versionInfosRef,
         versionInfoActRef = versionInfoActRef,
         logFile = logFile
         })

addVersionInfo :: VersionState -> (Bool,VersionInfo) -> IO ()
addVersionInfo versionState (iv @ (isEdit,versionInfo)) =
   do
      -- We do everything inside the "External" action, thus ensuring that
      -- we do not conflict with any simultaneous client action.
      -- This also means we can be sure the access to versionInfosRef
      -- will not deadlock.
      versionInfoAct <- readIORef (versionInfoActRef versionState)
      versionInfoAct
         (do
            writeLog (logFile versionState) versionInfo
            modifyMVar_ (versionInfosRef versionState) 
               (\ map0 -> return (
                  addToFM map0 (objectVersion versionInfo) versionInfo
                  ))
            return  iv
            )

lookupVersionInfo :: VersionState -> ObjectVersion -> IO (Maybe VersionInfo)
lookupVersionInfo versionState objectVersion =
   do
      map <- readMVar (versionInfosRef versionState)
      return (lookupFM map objectVersion)   

getVersionInfos :: VersionState -> IO [VersionInfo]
getVersionInfos versionState = 
   do
      map <- readMVar (versionInfosRef versionState)
      return (eltsFM map)

registerAct :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> IO ()
registerAct versionState actFn =
   modifyMVar_ (versionInfosRef versionState) 
      (\ map0 ->
         do
            writeIORef (versionInfoActRef versionState) actFn
            return map0
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
      }
   }
   


displayVersionInfo :: Bool -> VersionInfo -> IO ()
displayVersionInfo allowSystem versionInfo =
   do
      let
         user1 = user versionInfo
         server1 = server versionInfo
      -- We don't use createDialogWin' and fancy markup text because 
      -- DialogWin constrains this only to fit into a window of size (30,5).
      showSystem <- createDialogWin
         (("Quit",False) :
             if allowSystem 
                then
                   [("Show System Info",True)]
                else
                   []
            )
         Nothing
         [text (label user1 ++ "\n"
            ++ contents user1 ++ "\n"
            ++ if private user1 then "not for export" else "for export"
            )]
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

            createDialogWin
               [("Quit",())]
               Nothing
               [text (
                  "Version: " ++ vToS (version user1) ++ "\n"
                  ++ "Parents: " ++ vsToS (parents user1) ++ "\n"
                  ++ "ServerId: " ++ serverId server1 ++ "\n"
                  ++ "Serial No: " ++ show (serialNo server1) ++ "\n"
                  ++ "Created by: " ++ userId server1 ++ "\n"
                  ++ "Created on: " ++ calendarTimeToString calendarTime
                  )]
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
editVersionInfo title versionInfo = 
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