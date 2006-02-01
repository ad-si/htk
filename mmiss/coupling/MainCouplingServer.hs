{- This module contains the main function for the MMiSS coupling server, and does the
   business of accepting sockets, authentication and so on.

   Some code has been poached from server/Server.hs.
   -}
module Main(main) where

import IO
import Time
import System
import Maybe

import Data.List
import Control.Concurrent
import qualified Control.Exception
import Network
import System.Directory

import ExtendedPrelude
import BinaryAll
import Computation hiding(try)
import WBFiles -- (getTOP,getCouplingPort,getCouplingDir,getparseArgumentsRequiring)
import FileNames
import EmptyGraphSort
import AtomString
import LinkManager
import Link
import EntityNames
import Folders
import Messages
import CopyFile

import CallServer(tryConnect)
import qualified VersionInfo
import qualified VersionDB
import VersionGraph
import VersionGraphClient
import View
-- import Aliases

import BlockSigPIPE
import Destructible
import Crypt
import PasswordFile
import HostsPorts

import Registrations

import MMiSSRunCommand
import MMiSSRegistrations
import MMiSSImportExportErrors
import MMiSSPackageFolder
import MMiSSSplitLink
import MMiSSObjectExtract
import MMiSSFormat(Format(..))
import MMiSSObjectType
import {-# SOURCE #-} MMiSSExportFiles
import OntologyImport


main :: IO ()
main =
   do
      parseArgumentsRequiring [
         "top",
         "couplingPort",
         "couplingDir"
         ]

      blockSigPIPE

      doRegistrations
      doMMiSSRegistrations

      portDesc <- getCouplingPort
      portNumber <- getPortNumber portDesc
      couplingDir <- getCouplingDir
      reposServerOpt <- getServer
      reposPort <- getPort
      let
         reposServer = fromMaybe "localhost" reposServerOpt
 
      socket <- listenOn (PortNumber portNumber)

      let
         serverAction =
            do
               (handle,hostName,_) <- accept socket
               hSetBuffering handle LineBuffering
--               forkIO (mainHandle handle hostName couplingDir reposServer reposPort)
               mainHandle handle hostName couplingDir reposServer reposPort
               
               serverAction
 
      serverAction

mainHandle :: Handle -> String -> String -> String -> Int -> IO ()
mainHandle handle hostName couplingDir server port =
   do
      userPasswordWE <- addFallOutWE (\ break -> breakOtherExceps break (
         do
            service <- hGetLine handle
--            service <- coerceWithErrorOrBreakIO break serviceWE

            userStr <- hGetLine handle
--            userId <- coerceWithErrorOrBreakIO break userIdWE

            password <- hGetLine handle
--            password <- coerceWithErrorOrBreakIO break passwordWE

            if (service /= "MMiSS-SVN")
               then
                  break ("Service: " ++ service ++ " not recognised")
               else
                  done

            let
               authError = break ("Unable to authenticate user: '" ++ userStr ++ "' with password '"
                                  ++ password  ++ "' for Service '" ++ service ++ "'")

            userOpt <- getUserEntry userStr
            user <- case userOpt of
               Nothing -> authError
               Just user -> return user

            passwordOK <- verifyPassword password 
               (encryptedPassword user)
            if passwordOK 
               then
                  return (userStr,password)
               else
                  authError
         ))

      couplingMess <- newCouplingMessages
      setMessFns (apiMessFns couplingMess)

      Control.Exception.try ( 
         -- general wrapper to catch IO errors
         case fromWithError userPasswordWE of
            Right (user,pwd) ->
               do
                  top <- getTOP

                  clockTime <- getClockTime
                  calendarTime <- toCalendarTime clockTime
                  putStrLn "----------------------------------------------------------------------"
                  putStrLn (user ++ "@" ++ hostName ++ ":"
                             ++ calendarTimeToString calendarTime) 
                  let 
                    scriptDir = (trimDir top) `combineNames`
                                ("mmiss" `combineNames` "scripts")
                    dosvnup = (scriptDir `combineNames` "dosvnup ") ++ couplingDir

                  exitcode <- system dosvnup

                  if (exitcode /= ExitSuccess)
                    then 
                      do 
                        writeStringH handle ("ERROR: SVN update failed with exit code " ++ (show exitcode))
                        hClose handle
                    else
                      do
                        hPutStrLn handle "OK"
                        hFlush handle

                        calendarTime2 <- toCalendarTime clockTime
                        putStrLn (calendarTimeToString calendarTime2) 

                        versionGraph <- connectToReposServer user pwd server port
                        lastVersion <- getLastVersion versionGraph 
                        let 
                          userInfo = VersionInfo.user lastVersion
                          objectVersion = VersionInfo.version userInfo
                        view <- checkout versionGraph objectVersion

                        hPutStrLn stdout ("  Last Version is " ++ (show objectVersion))
                        hFlush stdout
                        hFlush handle         

                        packages <- doUpdates handle versionGraph view couplingDir scriptDir couplingMess []
                        let
                            paths = map (toString . fst)  packages
                        if (packages == [])
                           then do
                                  putStrLn "No Packages have been updated."
                                  putStrLn "-----------------------------------------------------------------------"
                                  hFlush stdout
                                  closeServer versionGraph
                                  hClose handle
                                  done
                           else do
                                  mapM_ (exportPackage view couplingDir couplingMess) packages
                                  mapM_ (doAddPackage scriptDir) packages 
                                  newVersion <- commitView view
                                  putStrLn ("Update finished. New version is: " ++ (show newVersion))
                                  putStrLn "Successfully re/imported packages:"
                                  mapM_ (\ path -> putStrLn path) paths

                                  calendarTime3 <- toCalendarTime clockTime
                                  putStrLn (calendarTimeToString calendarTime3) 

                                  let
                                     commitMess = "Corresponding MMiSS version: " ++ (show newVersion)
                                  exitcode <- system (scriptDir `combineNames` 
                                                        ("dosvncommit " ++ couplingDir ++ " " ++ commitMess))
                                  putStrLn "-----------------------------------------------------------------------"
                                  hFlush stdout
                                  closeServer versionGraph
                                  hFlush stdout
                                  hClose handle
                                  done
                        
            Left mess ->
               do
                  putStrLn (hostName ++ ": Connection failed")
                  putStrLn mess
                  hFlush stdout
                  writeStringH handle ("ERROR: " ++ mess)
                  hClose handle
         )

      hClose handle
      done

  where
     doAddPackage :: String -> (EntityFullName, String) -> IO()
     doAddPackage scriptDir (packagePath, packageFSPath) =
       do
         let 
           filename1 = packageFSPath ++ ".tex"
           filename2 = packageFSPath ++ ".imp"
           dosvnadd1 = scriptDir `combineNames` ("dosvnadd " ++ couplingDir ++ " " ++ filename1)
           dosvnadd2 = scriptDir `combineNames` ("dosvnadd " ++ couplingDir ++ " " ++ filename2)
         exitcode <- system dosvnadd1
         exitcode <- system dosvnadd2
         return()


exportPackage :: View -> String -> CouplingMessages -> (EntityFullName,String) -> IO()
exportPackage view couplingDir messages (packagePath, packageFSPath) = 
  do
    putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn ("  Exporting Package " ++ (toString packagePath) ++ "\n    into file " ++ packageFSPath)
    putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    linkedObjectOpt <- getLinkedObject view packagePath
    case linkedObjectOpt of
      Nothing -> return()
      Just(linkedObject) ->
        do 
          let 
             filename = couplingDir `combineNames` (packageFSPath ++ ".tex") 
             errFilename = couplingDir `combineNames` (packageFSPath ++ ".err")
             packageNameOpt = entityBase packagePath
          case packageNameOpt of
             Nothing -> importExportError "  doExport: Could not find package name in EntityFullName."
             Just(packageName) ->
               do
                 -- Try to find the Package object in the Package folder:
                 packageLinkOpt <- lookupObjectInFolder linkedObject packageName
                 case  packageLinkOpt of
                   Nothing -> importExportError "  doExport: Could not find package object inside package folder."
                   Just packageLink -> 
                     do ok <- exportMMiSSObjectNoFiles  view packageLink filename (toString packageName)
                        result <- printMessages messages (Just errFilename) False
                        deleteMessages messages
                        if ok 
                          then do ok <- printImports linkedObject couplingDir packagePath view
                                  return()
                          else return()



{-- doUpdates nimmt zeilenweise Namen von mmisslatex-Package-Files (mit komplettem Pfad
zum Root-Verzeichnis entgegen und importiert diese, falls sie noch nicht im Repos. waren
oder reimportiert sie, wenn sie schon vorhanden sind.
Die Funktion erwartet in einer Zeile folgendes Format:

<path-to-package-file>

Beispiel:

/Test/Test1/MyPackage.ptx
/MyPackage2.ptx

Des weiteren wird angenommen, dass das Label eines Packages identisch mit dem Filename (ohne
.ptx) ist, da es unter diesem Namen gesucht bzw. angelegt wird.

TODO: In die Files reingucken und den Packagenamen extrahieren.
--}

doUpdates :: Handle -> VersionGraph -> View -> String -> String -> CouplingMessages 
              -> [(EntityFullName,String)] -> IO ([(EntityFullName,String)])
doUpdates handle versionGraph view couplingDir scriptDir messages packages =
  do
    line <- hGetLine handle
    hPutStrLn stdout ("- " ++ line)
    hFlush stdout
    if (line == "commit")
      then return(packages)
      else    
        do 
          let
             completePath = unbreakName ((breakName couplingDir) ++ (breakName line))
             packagePathWOExt = if (isSuffixOf "_ptx.tex" line)
                                     then take ((genericLength line) - 8) line
                                     else if (isSuffixOf ".tex" line)
                                            then take ((genericLength line) - 4) line
                                            else line

          packagePathStripped <- filenameToPackagename completePath line
          fullPackageName <-
            case (fromWithError packagePathStripped) of
               Left mess ->  do putStrLn ("fromWithError packagePathStripped:  " ++ mess)
                                hFlush stdout
			        fail "Error: Can't create package path!"
               Right packagePathStr ->
		 case fromWithError (fromStringWE packagePathStr) of
		   Left mess -> do
				  putStrLn ("fromStringWE:  " ++ mess)
                                  hFlush stdout
				  fail "Error: Can't create package path!"
		   Right fullPackage -> return(fullPackage)

          putStrLn ("fullPackageName: " ++ (toString fullPackageName))
	  linkedObjectOpt <- getLinkedObject view fullPackageName
	  importedPackageOpt <-
	    case linkedObjectOpt of
	      Nothing ->
		do
		   let
		     dirPart = fromMaybe trivialFullName (entityDir fullPackageName)

		   parentFolderLinkWE <- findOrCreateFolder view dirPart

		   case fromWithError parentFolderLinkWE of
		     Left mess -> do
				    putStrLn ("  " ++ mess)
				    errorMess ("Error:\n" ++ mess)
				    return(Nothing)
		     Right parentFolderLink ->
		       do 
			 ok <- importMMiSSPackage1 view parentFolderLink (Just completePath)
			 case ok of
			    True -> return(Just(fullPackageName))
			    False -> do
				       putStrLn "Error: Import has failed!"
				       return(Nothing)

             --   Package has been found:
             --
	      (Just linkedObject)  -> 
		   do 
		     folderLink1 <- case splitLinkedObject linkedObject of
			MMiSSPackageFolderC folderLink -> return folderLink
			_ -> do
				putStrLn ("  " ++ (toString fullPackageName) ++ " is not a package!")
				errorMess ("Error: " ++ (toString fullPackageName)
					     ++ " is not a package!")
				fail ""
		     reimportMMiSSPackage1 view folderLink1 (Just completePath)
		     return(Just(fullPackageName))

	  (newPackageList, fullPathWithoutExt) <- 
	     case importedPackageOpt of
	       Nothing -> return((packages,""))
	       Just(name) -> return((packages ++ [(name, packagePathWOExt)]), 
                                    (couplingDir `combineNames` packagePathWOExt))
	  hFlush handle
	  hFlush stdout
	  result <- try (printMessages messages (Just (fullPathWithoutExt ++ ".err")) False)
	  exitcode <- 
	    case importedPackageOpt of
	       Nothing -> return(ExitSuccess)
	       Just(_) -> system (scriptDir `combineNames` 
				    ("dosvnadd " ++ couplingDir ++ " " ++ (fullPathWithoutExt ++ ".err")))
	  case result of
	     Left err -> do 
			   putStrLn (show err)
			   hFlush stdout
			   deleteMessages messages
			   doUpdates handle versionGraph view couplingDir scriptDir
				     messages newPackageList

	     Right _ -> do
			  deleteMessages messages
			  doUpdates handle versionGraph view couplingDir scriptDir
				    messages newPackageList
  where
    filenameToPackagename :: String -> String -> IO (WithError String)
    filenameToPackagename cpath reppath = 
      do 
        ok <- doesFileExist cpath
	case ok of
	  False -> return (hasError ("File " ++ cpath ++ " doesn't exist!"))
	  True -> 
	    do 
               let cmdStr = (scriptDir `combineNames` ("dopackagelabel " ++ couplingDir ++ " " ++ reppath))
               (exitcode, output) <- runTool "PackageNameExtraction" cmdStr
					     
	       case exitcode of
		 ExitSuccess ->
		    do let 
			 (repdir,_) = splitName reppath
			 newpath = filter (/= '\n') (unbreakName ((breakName repdir) ++ (breakName output)))
		       return(hasValue(newpath)) 
		 otherwise -> return(hasError ("Package name extraction returned: " ++ (show exitcode)))            


-- ----------------------------------------------------------------------
-- Functions for Connecting to the MMiSS Repository Server
-- ----------------------------------------------------------------------


connectToReposServer :: String -> String -> String -> Int -> IO (VersionGraph)
connectToReposServer user password server port =
   do
      putStrLn ("Repository Server is: " ++ server)
      putStrLn ("Repository Port is: " ++ (show port))
      putStrLn ("Repository User is: " ++ user)
      putStrLn ("Repository Password is: " ++ password)
      hFlush stdout

      hostPortWE <- fromHostDescription1 (server ++ ":" ++ (show port)) 
                       (HostsPorts.LoginInfo {
                        HostsPorts.user = user,
                        HostsPorts.password = password
                        })
      hostPort <- coerceWithErrorOrBreakIO importExportError hostPortWE

      let
         ?server = hostPort
      errOrRepository <- tryConnect VersionDB.initialise
      repository <- case errOrRepository of
                       Left err -> importExportError err
                       Right repository -> return repository

      versionGraph <- newVersionGraph emptyGraphSort repository
      return(versionGraph)

--      serverRef <- setServer state serverRefOpt versionGraph
--      return (ConnectResponse serverRef)


closeServer :: VersionGraph -> IO ()
closeServer versionGraph = destroy versionGraph


-- getLastVersion retrieves the list of versions from the versionGraph
-- and return the 'maximum version'. Because VersionInfo is an instance
-- of Ord, we assume that this is the version with the highest ObjectVersion
-- number. Of cause, this normally doesn't ensures that there are parallel
-- versions ('branches' or more generally: There could be more leafs in the
-- Version DAG). But the Subversion-MMiSS-Coupling assures that only one
-- version at a time is created and that is always a successor of the latest
-- version, so we can assure that for Repos. which has been populated only by
-- the coupling mechanism, the maximum ObjectVersion number always give the
-- last checked in version:

getLastVersion :: VersionGraph -> IO (VersionInfo.VersionInfo)
getLastVersion versionGraph =
   do
     let
         graphClient :: VersionGraphClient 
         graphClient = toVersionGraphClient versionGraph

     (versionInfos1 :: [VersionGraphClient.VersionInfo1]) 
         <- getVersionInfos graphClient
     let
         versionInfos2 :: [VersionInfo.VersionInfo]
         versionInfos2 = map toVersionInfo versionInfos1

     return(maximum versionInfos2)


checkout :: VersionGraph -> VersionInfo.ObjectVersion -> IO (View)
checkout versionGraph objectVersion =
   do
      let
         repository = toVersionGraphRepository versionGraph
         versionGraphClient = toVersionGraphClient versionGraph

      viewOpt <- VersionDB.catchNotFound (
         getView repository versionGraphClient objectVersion)
      case viewOpt of
         Nothing -> importExportError "Version not found"
         Just view -> return view


getLinkedObject :: View -> EntityFullName -> IO (Maybe LinkedObject)
getLinkedObject view fullName =
   lookupLinkedObjectByFullName view fullName


findOrCreateFolder :: View -> EntityFullName -> IO (WithError LinkedObject)
findOrCreateFolder view (EntityFullName entityNames) =
  case length entityNames of
     0 -> do
             rootFolderOpt <- lookupLinkedObjectByFullName view (EntityFullName [])
             case rootFolderOpt of
               Nothing -> return(hasError("findOrCreateFolder: Can't find Root Folder!") )
               Just rootFolder -> return(hasValue(rootFolder))

     _ -> findOrCreateFolder1 view (EntityFullName entityNames)


findOrCreateFolder1 :: View -> EntityFullName -> IO (WithError LinkedObject)
findOrCreateFolder1 view (EntityFullName entityNames) =
  do
     linkedObjectOpt <- lookupLinkedObjectByFullName view (EntityFullName entityNames)
     case linkedObjectOpt of
       Just (linkedObject) ->
         return (hasValue linkedObject)
       Nothing ->
         do
            parentLinkedObjectOpt <- 
              findOrCreateFolder view  
                                 (EntityFullName (take ((length entityNames)-1) entityNames))
            case fromWithError parentLinkedObjectOpt of
                Left err -> return(hasError (err))
                Right (parentObject) ->
                  do
                     let
                       name = toString (last entityNames)
                     linkedObjectOpt <- createNewFolder view parentObject name (EntityFullName entityNames)
                     hFlush stderr
                     case linkedObjectOpt of
                       Nothing -> return(hasError ("Folder '" ++  (toString (EntityFullName entityNames)) 
                                                    ++ "' could not be found or created"))
                       Just lo -> return(hasValue(lo))



createNewFolder :: View -> LinkedObject -> String -> EntityFullName -> IO(Maybe LinkedObject)
createNewFolder view parentLinkedObject folderName newFoldersFullName =
  newEmptyFolder1 plainFolderType view parentLinkedObject folderName


exportMMiSSObjectNoFiles :: View -> Link MMiSSObject -> FilePath -> String -> IO (Bool)
exportMMiSSObjectNoFiles view link filePath packageName =
   do
      result <- addFallOut (\ break ->
         do
            object <- readLink view link

            (result1WE :: WithError (String,ExportFiles))
               <- extractMMiSSObject view link LaTeX
            let
               (string,_) 
                  = coerceWithErrorOrBreak break result1WE
            -- Write to the file
            resultWE <- copyStringToFileCheck string filePath
            coerceWithErrorOrBreakIO break resultWE
         )

      case result of
         Right () -> return(True)
         Left mess -> do errorMess mess
                         return(False)

-- ----------------------------------------------------------------------------
-- Functions for reading and writing Strings during login.
-- ----------------------------------------------------------------------------

{--
readString :: Handle -> IO (WithError String)
readString = hReadLtd (maxLen + 1)
--}

writeStringH :: Handle -> String -> IO ()
writeStringH handle str = hWrite handle str

{--
maxLen :: Int
maxLen = 127
--}


apiMessFns :: CouplingMessages -> MessFns
apiMessFns (state @ (CouplingMessages mVar)) =
   let
      alertFn mess = addMessage (Alert, mess)
      warningFn mess = addMessage (Warning, mess)
      messageFn mess = addMessage (InfoMessage, mess)
      errorFn mess = addMessage (Error, mess)

      confirmFn mess =
         do
            errorFn ("API Bug: asked to confirm " ++ mess)
            return False

      addMessage :: (MessageType, String) -> IO ()
      addMessage message =
         modifyMVar_ mVar
            (\ state0 ->
               let
                  messages0 = messages state0
                  messages1 = message : messages0
               in
                  return (state0 {
                     messages = messages1
                     })
               )
   in
      MessFns {
         alertFn = alertFn,
         errorFn = errorFn,
         warningFn = warningFn,
         confirmFn = confirmFn,
         messageFn = messageFn,
         htkPres = False
         }



newtype CouplingMessages = CouplingMessages (MVar CouplingMessagesValue)

data MessageType = Alert | Warning | InfoMessage | Error | Confirm deriving (Eq,Show)

data CouplingMessagesValue = CouplingMessagesValue {
   messages :: [(MessageType,String)]
   }

initialCouplingMessages :: CouplingMessagesValue
initialCouplingMessages = CouplingMessagesValue {messages = []}


newCouplingMessages :: IO CouplingMessages
newCouplingMessages = 
   do
      mVar <- newMVar initialCouplingMessages
      return (CouplingMessages mVar)


printMessages :: CouplingMessages -> Maybe String -> Bool -> IO ()

printMessages (CouplingMessages mVar) filenameOpt append =
  do  couplingMessagesValue <- readMVar mVar
      let
         messageList = messages couplingMessagesValue
      case messageList of
        [] -> done
        ((mtype, str):rest) -> 
          do
            mapM_ (\ (mtype, str) ->
                        putStrLn ("  " ++ (show mtype) ++ ": " ++ str)
                  )
                  messageList
            hFlush stdout
            case filenameOpt of
              Nothing -> done
              Just filename ->
                do
                  result <- if append 
                              then try (openFile filename AppendMode)
                              else try (openFile filename WriteMode)
                  case result of
                    Left err -> do
                                  putStrLn ("Error opening output file: " ++ filename)
                                  hFlush stdout
                    Right handle ->
                      do 
                        mapM_ (\ (mtype, str) ->
                                      if ((mtype == InfoMessage) || (mtype == Confirm))
                                        then return()
                                        else hPutStrLn handle ((show mtype) ++ ": " ++ str)
                              )
                              messageList
                        hFlush handle
                        hClose handle
                  

deleteMessages :: CouplingMessages -> IO ()

deleteMessages (CouplingMessages mVar) =
   modifyMVar_ mVar 
      (\ couplingValue ->
            return (couplingValue {messages = []})
         )


