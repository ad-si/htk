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

   cleanVersionInfo, 
      -- :: VersionInfo -> VersionInfo
      -- Clear settable fields (used on checkout).

   VersionState,

   mkVersionState, -- :: Bool -> IO VersionState
      -- The Bool should be True for an internal server, False otherwise.
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
   ) where

import IO
import Time

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

import DialogWin
import SimpleForm
import MarkupText
import HTk(text,height,width,value,background)

import qualified PasswordFile

import LogFile

-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------

newtype ObjectVersion = ObjectVersion Int deriving (Eq,Ord,Typeable)
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
   writeBin = mapWrite (\ (ObjectVersion i) -> i)
   readBin = mapRead ObjectVersion
   
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
      (\ (VersionInfo {user = user,server = server})
         ->
         (user,server)
         )
   readBin = mapRead 
      (\ (user,server)
         ->
         (VersionInfo {user = user,server = server})
         )

-- ----------------------------------------------------------------------
-- Comparing VersionInfo in a globally unique way, assuming the
-- client doesn't rig the VersionInfos.
-- ----------------------------------------------------------------------

type VersionInfoKey = (Int,String,String,ClockTime) -- must be instance of Ord

mapVersionInfo :: VersionInfo -> VersionInfoKey
mapVersionInfo versionInfo =
   let
      server1 = server versionInfo
   in
      (serialNo server1,serverId server1,userId server1,timeStamp server1)
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
            server = versionServer
            }

      return (hasValue versionInfo)
mkVersionInfo versionState user (Right versionInfo) =
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

-- ----------------------------------------------------------------------
-- Accessing the version data
-- ----------------------------------------------------------------------

data VersionState = VersionState {
   versionInfosRef :: MVar (FiniteMap ObjectVersion VersionInfo),
      -- all VersionInfos so far.
      -- also used as a lock on various operations.
   versionInfoActRef :: IORef ( IO (Bool,VersionInfo) -> IO ()),
   logFile :: LogFile VersionInfo,
   thisServerId :: String
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
         versionInfos = foldl
            (\ versionInfos0 versionInfo 
               -> addToFM versionInfos0 (objectVersion versionInfo) 
                  versionInfo
               )
            emptyFM
            versionInfosList

      versionInfosRef <- newMVar versionInfos
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

registerAndGet :: VersionState -> (IO (Bool,VersionInfo) -> IO ()) -> 
   IO [VersionInfo]
registerAndGet versionState actFn =
   modifyMVar (versionInfosRef versionState) 
      (\ map0 ->
         do
            writeIORef (versionInfoActRef versionState) actFn
            return (map0,eltsFM map0)
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
                        return(fullHostName ++ ":" ++ show pID)
                  else
                     do
                        port <- getPort
                        return (if port == 11393
                           then
                              fullHostName
                           else
                              fullHostName ++ ":" ++ show port
                           )
