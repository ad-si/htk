-- | API for MMiSS.  This gives basic text-only access to the repository.
--
-- NB.  This module is now largely obsolete, and will be replaced for
-- practical purposes by the XML interface in the other modules in this
-- directory.  Still it may still be useful for testing and so on.
module MMiSS.API.Interface(
   -- General Idea.  We maintain lots of "current" things,  the
   -- "current server", the "current version", the "current directory", and
   -- the "current variants".
   -- Many operations are provided in two (or more) forms;
   -- the simple one which just uses the current server (or current whatever),
   -- and the more complex one which takes an argument.  Then the more complex
   -- operation will have the same name as the simple operation, only with a
   -- number attached, for example "listVersions" (the simple version) and
   -- "listVersions1" (the more complex version).
   --
   -- Each Version maintains its own current directory.  However the
   -- current variants and current server are global.

   open,
      -- :: String -> IO Server
      -- open a connection to a server and make it current
      -- The String should be the hostname of the server, possibly followed
      -- by a colon and the port-number.
   listVersions,
      -- :: IO ()
      -- list versions available on current server (with their numbers).
   listVersions1,
      -- :: Server -> IO ()
      -- list versions available on server.
   setVersionFormat,
      -- :: String -> IO ()
      -- Set format by which version information is displayed by listVersions
      -- and friends.

      -- (copied from VersionInfo.hs)
      -- Here
      --    %V is expanded to the version number
      --    %L to the label
      --    %C to the contents (detailed description)
      --    %P to the parents (as a list of version numbers)
      --    %U to the committing user
      --    %T to the timestamp
      --
      -- Example: a format of "%V %L %P\n" will give a short description of the
      -- version (version number, label, and parents).  "%V %L %P %U %T\n%C\n"
      -- will put the short information (version number, label, parents, user,
      -- timestamp) on one line, then give the contents separately.

   cdServer,
      -- :: Server -> IO ()
      -- change the current server.
   checkOut,
   checkOut1,
      -- :: Server -> Int -> IO Version
      -- :: Int -> IO Version
      -- check out a version and make it current, with the current directory
      -- the top object.
   changeVersionInfo,
      -- :: IO ()
      -- Allow the user to change the version information (label and
      -- description) of the current version.
   commitVersion,
      -- :: IO ()
      -- Commit the current version
   commitVersion1,
      -- :: Version -> IO ()
      -- Commit the version

   cdVersion,
      -- :: Version -> IO ()
      -- Change current version.  (This will not change the current
      -- server or current variants.)

   ls,
      -- :: IO ()
      -- List contents of current directory in version.
   cd,
      -- :: String -> IO ()
      -- Change current directory.
      -- The string has the form A.B.[blah] or Root.A.B.[blah] or
      -- Parent.Parent.A.B.[blah], like search-names we use for import
      -- statements.  It is evaluated using the current directory as a
      -- starting point.

   get,
      -- :: String -> IO ()
      -- Export the given object to the current Unix directory on the current
      -- system, as a LaTeX file.  (Also export all associated graphics)
   getXML,
      -- :: String -> IO ()
      -- Like get, but retrives an MMiSS-XML file instead.

   put,
      -- :: String -> IO ()
      -- Read the given Unix MMiSS LaTeX or MMiSS XML file in,
      -- which should be new, and put it in the current directory.

   reput,
      -- :: String -> IO ()
      -- Read the given file into the current directory, which should
      -- be the corresponding MMiSSPackageFolder

   lcd,
      -- :: String -> IO ()
      -- Change directory on the local (Unix or whatever) filing system.
   shell,
      -- :: String -> IO ()
      -- Execute the given String in a shell on the current system.

   lsVariants,
      -- :: String -> IO ()
      -- Look up the given MMiSS object or file, and list its available
      -- variants
   set,
      -- :: String -> String -> IO Variant
      -- (set a b) alters the current variant setting variant (a) to value (b),
      -- and returns the new variant.
   unset,
      -- :: String -> IO Variant
      -- (unset a) alters the current variant setting to unset variant a.
   cdVariant,
      -- :: Variant -> IO ()
      -- Change the current variant.
   whereami,
      -- IO ()
      -- Display information about the current server, directory and
      -- variants.

   -- Types returned by functions.
   Server,
   Version,
   Variant,
   ) where

import Directory
import System

import System.IO.Unsafe
import Control.Concurrent.MVar
import qualified Data.Map as Map
import qualified Control.Exception

import Util.Object
import Util.Computation
import Util.ExtendedPrelude
import Util.CommandStringSub
import Util.Sources
import Util.AtomString
import Util.Messages
import Util.FileNames
import Util.IntPlus

import Server.HostsPorts

import Posixutil.CopyFile

import Graphs.EmptyGraphSort

import SimpleDB.VersionInfo hiding (changeVersionInfo)
import Types.VersionDB hiding (listVersions)

import Imports.EntityNames
import Imports.FolderStructure

import Types.VersionGraph
import Types.VersionGraphClient
import Types.View hiding (Version)
import Types.ViewType(viewInfoBroadcaster)
import Types.Folders
import Types.LinkManager
import Types.Link
import Types.ObjectTypes
import Types.Registrations

import MMiSS.Bundle
import MMiSS.Registrations
import MMiSS.PackageFolder
import MMiSS.Variant
import MMiSS.VariantObject
import MMiSS.Format
import MMiSS.ObjectType
import MMiSS.ObjectExtract
import MMiSS.ExportFiles
import MMiSS.PackageFolder

-- -------------------------------------------------------------------------
-- User Datatypes
-- -------------------------------------------------------------------------

data Server = Server {
   versionGraph :: VersionGraph,
   serverDesc :: String
   } deriving (Eq,Ord)

newtype Version = Version View deriving (Eq,Ord)

newtype Variant = Variant MMiSSVariantSearch

-- -------------------------------------------------------------------------
-- Private Datatypes
-- -------------------------------------------------------------------------

-- Used to represent our current state
data State = State {
   currentServer :: Maybe Server,
   currentVariant :: Variant,
   currentVersion :: Maybe Version,
   currentDirs :: Map.Map Version Dir,
   versionInfoFormat :: CompiledFormatString,
   registrationsDone :: Bool
   }

-- Used to represent a directory.
data Dir =
      Folder (Link Folder)
   |  Package (Link MMiSSPackageFolder)

-- -------------------------------------------------------------------------
-- Initialisation
-- -------------------------------------------------------------------------

stateMVar :: MVar State
stateMVar = unsafePerformIO (newMVar initialState)
{-# NOINLINE stateMVar #-}

initialState :: State
initialState = State {
   currentServer = Nothing,
   currentVariant = Variant emptyMMiSSVariantSearch,
   currentVersion = Nothing,
   currentDirs = Map.empty,
   versionInfoFormat = initialVersionInfoFormat,
   registrationsDone = False
   }

initialVersionInfoFormat :: CompiledFormatString
initialVersionInfoFormat = coerceWithError (checkVersionInfoFormat (
   "Version: %V Label: %L Date: %T\n"))

getState :: IO State
getState = readMVar stateMVar

getStateValue :: (State -> a) -> IO a
getStateValue gv =
   do
      state <- getState
      return (gv state)

getCurrentServer :: IO Server
getCurrentServer =
   do
      currentServerOpt <- getStateValue currentServer
      case currentServerOpt of
         Nothing -> apiError "No server open yet!"
         Just currentServer -> return currentServer

getCurrentVersion :: IO Version
getCurrentVersion =
   do
      currentVersionOpt <- getStateValue currentVersion
      case currentVersionOpt of
         Nothing -> apiError "No current version! (server not open yet?)"
         Just version -> return version

getCurrentDir :: Version -> IO Dir
getCurrentDir version =
   do
      dirs <- getStateValue currentDirs
      case Map.lookup version dirs of
         Nothing -> apiError "No current directory for this version!"
         Just dir -> return dir

setCurrentDir :: Version -> Dir -> IO ()
setCurrentDir version dir =
   modifyMVar_ stateMVar
      (\ state -> return (state {
         currentDirs = Map.insert version dir (currentDirs state)})
         )

getCurrentVariants :: IO Variant
getCurrentVariants = getStateValue currentVariant


-- --------------------------------------------------------------------------
-- Opening the server
-- --------------------------------------------------------------------------

-- | open a connection to a server and make it current
-- The String should be the hostname of the server, possibly followed
-- by a colon and the port-number.
open :: String -> IO Server
open serverString =
   printError (
      do
         modifyMVar_ stateMVar
            (\ state -> if registrationsDone state
               then
                  return state
               else
                  do
                     doRegistrations
                     doMMiSSRegistrations
                     return (state {registrationsDone = True})
               )
         hostPortWE <- fromHostDescription serverString
         hostPort <- coerceWithErrorOrBreakIO apiError hostPortWE
         versionGraph <-
            let
               ?server = hostPort
            in
               do
                  repository <- initialise
                  newVersionGraph emptyGraphSort repository
         let
            server = Server {
               versionGraph = versionGraph,
               serverDesc = serverString
               }

         cdServer server

         return server
      )


-- --------------------------------------------------------------------------
-- Listing versions
-- --------------------------------------------------------------------------

-- | list versions available on current server (with their numbers).
listVersions :: IO ()
listVersions =
   printError (
      do
         server <- getCurrentServer
         listVersions1' server
      )

-- | list versions available on server.
listVersions1 :: Server -> IO ()
listVersions1 server = printError (listVersions1' server)

listVersions1' :: Server -> IO ()
listVersions1' (Server {versionGraph = versionGraph}) =
   do
      let
         graphClient :: VersionGraphClient
         graphClient = toVersionGraphClient versionGraph

      (versionInfo1s :: [VersionInfo1])
         <- getVersionInfos graphClient

      let
         versionInfos :: [VersionInfo]
         versionInfos = map toVersionInfo versionInfo1s

      format <- getStateValue versionInfoFormat
      (strs :: [String]) <- mapM (evalVersionInfoFormat format) versionInfos
      putStr (concat strs)

setVersionFormat :: String -> IO ()
setVersionFormat formatStr =
   printError (
      do
         let
            formatWE = checkVersionInfoFormat formatStr
         format <- coerceWithErrorOrBreakIO apiError formatWE
         modifyMVar_ stateMVar
            (\ state ->
               return (state {versionInfoFormat = format})
               )
      )

-- --------------------------------------------------------------------------
-- Checking out
-- --------------------------------------------------------------------------

-- | check out a version from the current server and make it current,
-- with the current directory the top object.
checkOut :: Integer -> IO Version
checkOut v =
   printError (
      do
         server <- getCurrentServer
         checkOut1' server v
      )

-- | check out a version and make it current, with the current directory
-- the top object.
checkOut1 :: Server -> Integer -> IO Version
checkOut1 server v = printError (checkOut1' server v)

checkOut1' :: Server -> Integer -> IO Version
checkOut1' (Server {versionGraph = versionGraph}) v =
   do
      let
         objectVersion = ObjectVersion v

         repository = toVersionGraphRepository versionGraph
         graphClient = toVersionGraphClient versionGraph

      viewOpt <- catchNotFound (
         getView repository graphClient objectVersion)
      case viewOpt of
         Nothing -> apiError "Version not found"
         Just view ->
            do
               let
                  version = Version view

               topFolderLink <- getTopFolder view
                  -- getTopFolder will create the top folder, which will be
                  -- necessary if we have just checked out the empty version.

               modifyMVar_ stateMVar
                  (\ state ->
                     return (state {
                        currentVersion = Just version,
                        currentDirs =
                           Map.insert version
                              (Folder topFolderLink) (currentDirs state)
                        })
                     )

               return version

-- --------------------------------------------------------------------------
-- Changing things that are easy to change.
-- --------------------------------------------------------------------------

-- | change the current server.
cdServer :: Server -> IO ()
cdServer server = modifyMVar_ stateMVar
   (\ state -> return (state {currentServer = Just server}))

-- | Change current version.  (This will not change the current
-- server or current variants.)
cdVersion :: Version -> IO ()
cdVersion version = modifyMVar_ stateMVar
   (\ state -> return (state {currentVersion = Just version}))


-- --------------------------------------------------------------------------
-- Directory operations
-- --------------------------------------------------------------------------

-- | List contents of current directory in version.
ls :: IO ()
ls =
   printError (
      do
         (version @ (Version view)) <- getCurrentVersion
         dir <- getCurrentDir version
         linkedObject <- getLinkedObject view dir

         contents <- readContents (listObjectContents linkedObject)
         putStrLn (unlines (
            map
               (\ (entityName,_) -> toString entityName)
               contents
            ))
      )

-- | Change current directory.
-- The string has the form A.B.[blah] or Root.A.B.[blah] or
-- Parent.Parent.A.B.[blah], like search-names we use for import
-- statements.  It is evaluated using the current directory as a
-- starting point.
cd :: String -> IO ()
cd newDir =
   printError (
      do
         (version @ (Version view)) <- getCurrentVersion
         linkedObject1 <- getObjectByName version newDir

         let
            wrappedLink1 = toWrappedLink linkedObject1

            unpacked1 = unpackWrappedLink wrappedLink1
            unpacked2 = unpackWrappedLink wrappedLink1

         dir <- case (unpacked1,unpacked2) of
            (Just (link :: Link Folder),_) -> return (Folder link)
            (_,Just (link :: Link MMiSSPackageFolder)) ->
               return (Package link)
            (Nothing,Nothing) ->
               apiError ("Found " ++ newDir ++ " but it is neither a folder \n"
                  ++ " nor a package folder.  Its internal type is "
                  ++ wrappedLinkTypeName wrappedLink1 ++ ".")

         setCurrentDir version dir
      )

getLinkedObject :: View -> Dir -> IO LinkedObject
getLinkedObject view dir = case dir of
   Folder folderLink ->
      do
         folder <- readLink view folderLink
         return (toLinkedObject folder)
   Package packageLink ->
      do
         package <- readLink view packageLink
         return (toLinkedObject package)

getLinkedObjectName :: View -> LinkedObject -> IO EntityFullName
getLinkedObjectName view linkedObject =
   do
      folderStructure <- getFolderStructure view
      getName folderStructure linkedObject


-- | General utility for retrieving an object by version and
-- string (representing a search name).
getObjectByName :: Version -> String -> IO LinkedObject
getObjectByName (version @ (Version view)) searchNameStr =
   do
      let
         searchNameWE = fromStringWE searchNameStr
      (searchName :: EntitySearchName)
         <- coerceWithErrorOrBreakIO apiError searchNameWE

      dir <- getCurrentDir version
      linkedObject0 <- getLinkedObject view dir

      folderStructure <- getFolderStructure view

      linkedObject1OptSource <- lookupSearchName folderStructure
         linkedObject0 searchName
      linkedObject1Opt <- readContents linkedObject1OptSource
      case linkedObject1Opt of
         Nothing -> apiError ("Unable to find " ++ searchNameStr)
         Just linkedObject1 -> return linkedObject1

getFolderStructure :: View -> IO (FolderStructure LinkedObject)
getFolderStructure view =
   do
      topLinkedObject <- getTopLinkedObject view
      return (toFolderStructure topLinkedObject)

-- | General utility for retrieving an MMiSSObject's link by name.
getMMiSSObjectLinkByName :: Version -> String -> IO (Link MMiSSObject)
getMMiSSObjectLinkByName (version @ (Version view)) name =
   do
      linkedObject <- getObjectByName version name
      let
         wrappedLink = toWrappedLink linkedObject
      case unpackWrappedLink wrappedLink of
         Just mmissObjectLink -> return mmissObjectLink
         Nothing ->
            apiError ("Found " ++ name ++ " but it is not an MMiSS "
               ++ " object.  Its internal type is "
               ++ wrappedLinkTypeName wrappedLink ++ ".")


-- --------------------------------------------------------------------------
-- Retrieving from the repository
-- --------------------------------------------------------------------------

-- | Export the given object to the current Unix directory on the current
-- system, as a LaTeX file.  (Also export all associated graphics)
get :: String -> IO ()
get = getFormat LaTeX


-- | Like get, but retrives an MMiSS-XML file instead.
getXML :: String -> IO ()
getXML = getFormat XML

getFormat :: Format -> String -> IO ()
getFormat format name =
   printError (
      do
         (version @ (Version view)) <- getCurrentVersion
         -- (1) lookup object
         mmissObjectLink <- getMMiSSObjectLinkByName version name

         -- (2) extractMMiSSObject
         (Variant variantSearch) <- getCurrentVariants
         let
            exportOpts = ExportOpts {
               getText = True,
               format = format,
               recurseDepth = infinity
               }

         extracted0 <- extractMMiSSObject1 view True mmissObjectLink
            (Just variantSearch) exportOpts
         (fileContents,exportedFiles) <- case fromWithError extracted0 of
            Left mess -> apiError mess
            Right extracted1 -> return extracted1

         -- (3) write out object contents
         let
            searchName = fromString name
            -- getObjectByName has already done this (and thrown an error if it
            -- didn't work).  Oh well.

            base = case searchNameDirBase searchName of
               Just (_,Just base) -> base
               _ -> apiError ("Can't get file name to write out " ++ name)
                  -- I think this shouldn't happen anyway.
            fileName = unsplitExtension (toString base) (toExtension format)


         copiedWE <- copyStringToFileCheck fileContents fileName
         coerceWithErrorOrBreakIO apiError copiedWE

         -- (4) write the attached files.
         thisDir <- getCurrentDirectory
         exportFiles view thisDir exportedFiles
      )

-- --------------------------------------------------------------------------
-- Importing to the repository
-- --------------------------------------------------------------------------

-- | Read the given Unix MMiSS LaTeX or MMiSS XML file in,
-- which should be new, and put it in the current directory.
put :: String -> IO ()
put fileName =
   printError (
      do
         (version @ (Version view)) <- getCurrentVersion
         dir <- getCurrentDir version
         linkedObject <- getLinkedObject view dir

         success <- importMMiSSPackage1 view linkedObject (Just fileName)
         if success
            then
               apiError "Put not successful"
            else
               done
      )

-- | Read the given file into the current directory, which should
-- be the corresponding MMiSSPackageFolder
reput :: String -> IO ()
reput fileName =
   printError (
      do
         (version @ (Version view)) <- getCurrentVersion
         dir <- getCurrentDir version
         linkedObject <- getLinkedObject view dir
         packageFolderLink <-
            case unpackWrappedLink (toWrappedLink linkedObject) of
               Just packageFolderLink -> return packageFolderLink
               Nothing -> apiError
                  "Current directory needs to be package folder to re-import."
         reimportMMiSSPackage1 view packageFolderLink (Just fileName)
      )

-- --------------------------------------------------------------------------
-- Variant Operations
-- --------------------------------------------------------------------------

-- | Look up the given MMiSS object or file, and list its available
-- variants
lsVariants :: String -> IO ()
lsVariants name =
   do
      (version @ (Version view)) <- getCurrentVersion
      -- (1) lookup object
      mmissObjectLink <- getMMiSSObjectLinkByName version name
      -- (2) read object
      mmissObject <- readLink view mmissObjectLink
      -- (3) list variants
      displayObjectVariants (variantObject mmissObject)

-- | (set a b) alters the current variant setting variant (a) to value (b),
-- and returns the new variant.
set :: String -> String -> IO Variant
set key0 value0 =
   printError (
      modifyMVar stateMVar
         (\ state0 ->
            do
               let
                  (Variant variantSearch0) = currentVariant state0
                  variantSearch1WE
                     = addToVariantSearch variantSearch0 key0 value0

               variantSearch1
                  <- coerceWithErrorOrBreakIO apiError variantSearch1WE
               let
                  variant1 = Variant variantSearch0

               return (state0 {currentVariant = variant1},variant1)
            )
      )

-- | (unset a) alters the current variant setting to unset variant a.
unset :: String -> IO Variant
unset key0 =
   printError (
      modifyMVar stateMVar
         (\ state0 ->
            do
               let
                  (Variant variantSearch0) = currentVariant state0
                  variantSearch1WE
                     = removeFromVariantSearch variantSearch0 key0

               variantSearch1
                  <- coerceWithErrorOrBreakIO apiError variantSearch1WE
               let
                  variant1 = Variant variantSearch0

               return (state0 {currentVariant = variant1},variant1)
            )
      )

-- | Change the current variant.
cdVariant :: Variant -> IO ()
cdVariant variant1 =
   modifyMVar_ stateMVar
      (\ state0 -> return (state0 {currentVariant = variant1}))

-- --------------------------------------------------------------------------
-- Information about the current state
-- --------------------------------------------------------------------------

-- | Display information about the current server, directory and
-- variants.
whereami :: IO ()
whereami =
   do
      state <- readMVar stateMVar
      let
         serverStr = case currentServer state of
            Nothing -> "Not connected"
            Just server -> "Server: " ++ serverDesc server

      versionDirStr <- case currentVersion state of
         Nothing -> return "No current checked-out version"
         Just (version @ (Version view)) ->
            do
               versionInfo <- readContents (viewInfoBroadcaster view)
               versionStr <- evalVersionInfoFormat (versionInfoFormat state)
                  versionInfo

               dir <- getCurrentDir version
               linkedObject <- getLinkedObject view dir
               fullName <- getLinkedObjectName view linkedObject

               return ("Version: " ++ versionStr ++ "\n Dir "
                  ++ toString (FromRoot fullName)
                  )

      let
         (Variant variantSearch) = currentVariant state

         variantsStr = show variantSearch

      messageMess (unlines ["Current State:\n",serverStr,versionDirStr,
         variantsStr])

-- --------------------------------------------------------------------------
-- Editing the current version information
-- --------------------------------------------------------------------------

-- | Allow the user to change the version information (label and
-- description) of the current version.
changeVersionInfo :: IO ()
changeVersionInfo =
   printError (
      modifyMVar_ stateMVar
         (\ state ->
            case currentVersion state of
               Nothing -> apiError "No current checked-out version"
               Just (Version view) ->
                  do
                     versionInfo0 <- readContents (viewInfoBroadcaster view)
                     userInfo1Opt <- editVersionInfo "Change version info:"
                        versionInfo0
                     case userInfo1Opt of
                        Nothing -> done
                        Just userInfo1 -> setUserInfo view userInfo1
                     return state
            )
         )

-- --------------------------------------------------------------------------
-- Committing the current version
-- --------------------------------------------------------------------------

-- | Commit the current version
commitVersion :: IO ()
commitVersion =
   printError (
      do
         version <- getCurrentVersion
         commitVersion1' version
      )

-- | Commit the version
commitVersion1 :: Version -> IO ()
commitVersion1 version = printError (commitVersion1' version)

commitVersion1' :: Version -> IO ()
commitVersion1' (Version view) =
   do
      commitView view
      done

-- --------------------------------------------------------------------------
-- System commands
-- --------------------------------------------------------------------------

-- | Change directory on the local (Unix or whatever) filing system.
lcd :: String -> IO ()
lcd newDir = (printError . anyErrorToAPI) (setCurrentDirectory newDir)

-- | Execute the given String in a shell on the current system.
shell :: String -> IO ()
shell command =
   (printError . anyErrorToAPI) (
      do
         exitCode <- system command
         case exitCode of
            ExitSuccess -> done
            ExitFailure i ->
               errorMess ("Command returned exit code " ++ show i)
      )

-- --------------------------------------------------------------------------
-- Error handling
-- --------------------------------------------------------------------------

fallOut :: (ObjectID,IO a -> IO (Either String a))
fallOut = unsafePerformIO newFallOut

printError :: IO a -> IO a
   -- we go via anyErrorToAPI for historical reasons
printError act =
   do
      stringOrA <- snd fallOut (anyErrorToAPI act)
      case stringOrA of
         Left mess -> error ("API error: " ++ mess)
         Right a -> return a


apiError :: BreakFn
apiError = mkBreakFn (fst fallOut)

anyErrorToAPI :: IO a -> IO a
anyErrorToAPI act =
   do
      (exOrA) <- Control.Exception.try (snd fallOut act)
      case exOrA of
         Right (Right a) -> return a
         Right (Left apiMess) -> apiError apiMess
         Left excep -> case ourExcepToMess excep of
            Just mess -> apiError ("Uncaught exception: " ++ mess)
            Nothing -> apiError ("Uncaught and unknown exception: "
               ++ show excep)
