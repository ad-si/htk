{- This module contains the main function for the MMiSS coupling server, and does the
   business of accepting sockets, authentication and so on.

   Some code has been poached from server/Server.hs.
   -}
module Main(main) where

import IO
import Time
import System
import Maybe
import Monad

import Data.List
import Data.Graph.Inductive
import Control.Concurrent
import qualified Control.Exception
import Network
import System.Directory
import Debug(debugString)

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
import Aliases

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
import MMiSSPreamble
import MMiSSOntologyStore
import MMiSSOntologyParser
import LaTeXPreamble

-- import OntologyImport

type ImportDepGraph = Gr EntityFullName String



main :: IO ()
main =
   do
      parseArgumentsRequiring [
         "top",
         "couplingPort",
         "couplingDir"
         ]

      debugString "Start of Coupling-Server"
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
--               sClose socket
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

            passwordOK <- verifyPassword password (encryptedPassword user)
            if passwordOK 
               then
                  return (userStr,password)
               else
                  authError
         ))

      couplingMess <- newCouplingMessages
      setMessFns (apiMessFns couplingMess)

      result <-
	Control.Exception.try ( 
	   -- general wrapper to catch IO errors
	   case fromWithError userPasswordWE of
	      Right (user,pwd) ->
		 do
		    top <- getTOP

		    clockTime <- getClockTime
		    calendarTime <- toCalendarTime clockTime
		    putStrLn "======================================================================="
		    putStrLn "======================================================================="
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

                          lastVersionMaybe <- getLastVersion versionGraph
			  lastVersion <- case lastVersionMaybe of
                                            Just(o) -> return(o)
                                            Nothing -> do
                                                         putStrLn "There is no version to check out! PANIC!\n"
                                                         fail "CouplingServer can't work without a checked out version.\n"
			  let 
			    userInfo = VersionInfo.user lastVersion
			    objectVersion = VersionInfo.version userInfo
			  view <- checkout versionGraph objectVersion

			  hPutStrLn stdout ("  Last Version is " ++ (show objectVersion))
			  hFlush stdout
			  hFlush handle         
                       -- packages :: ([(EntityFullName, String, Maybe MMiSSOntology, [EntityFullName])])
			  packages <- doUpdates handle versionGraph view couplingDir scriptDir couplingMess []
			  let
			      paths = map (\(p,_,_,_) -> (toString p))  packages
                              packageFullNames = map (\(p,_,_,_) -> p) packages
			  if (isEmptyPList packages)
			     then do
				    putStrLn "No Packages have been updated."
       		                    putStrLn "======================================================================="
				    hFlush stdout
				    closeServer versionGraph
				    hClose handle
				    done
			     else do
                                    -- Änderungen der Importbeziehungen in den Präambeln nachziehen
                                    --
                                    mapM_ (updateImportInfo view couplingMess) packages
                                    -- Sortieren der geänderten Packages nach Abhängigkeit
                                    --
                                    dependGraph <- foldM (importDependencies view couplingMess packageFullNames) 
                                                         empty 
                                                         (zip packageFullNames packageFullNames)
                                    putStrLn ("\n  Dependency graph:\n" ++ (show dependGraph))
                                    sortedToUpdateList <- return(toSortedList dependGraph "vor" )
                                    putStrLn ("\n  UpdateList:\n" ++ (show sortedToUpdateList))
                                    hFlush stdout

                                    -- Update der Ontologien gemäß der Reihenfolge im Abhängigkeitsgraphen
--                                    mapM_ updateOntology view couplingMess packages sortedToUpdateList

				    mapM_ (exportPackage view couplingDir couplingMess) packages
				    mapM_ (doAddPackage scriptDir) packages 
				    newVersion <- commitView view
				    printMessages couplingMess Nothing True
				    putStrLn ("Update finished. New version is: " ++ (show newVersion))
				    putStrLn "Successfully re/imported packages:"
				    mapM_ (\ path -> putStrLn path) paths

				    calendarTime3 <- toCalendarTime clockTime
				    putStrLn (calendarTimeToString calendarTime3) 

				    let
				       commitMess = "Corresponding MMiSS version: " ++ (show newVersion)
				    exitcode <- system (scriptDir `combineNames` 
							  ("dosvncommit " ++ couplingDir ++ " " ++ commitMess))
       		                    putStrLn "======================================================================="
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

      case result of
        Left e -> do putStrLn (show e)
        Right _ -> do hClose handle
                      done
--      hClose handle
--      done

  where
     isEmptyPList :: [(EntityFullName, String, Maybe MMiSSOntology, [EntityFullName])] -> Bool
     isEmptyPList [] = True
     isEmptyPList _ = False

     doAddPackage :: String -> (EntityFullName, String, Maybe MMiSSOntology, [EntityFullName]) -> IO()
     doAddPackage scriptDir (packagePath, packageFSPath, _, _) =
       do
         let 
           filename1 = packageFSPath ++ ".tex"
           filename2 = packageFSPath ++ ".imp"
           dosvnadd1 = scriptDir `combineNames` ("dosvnadd " ++ couplingDir ++ " " ++ filename1)
           dosvnadd2 = scriptDir `combineNames` ("dosvnadd " ++ couplingDir ++ " " ++ filename2)
         exitcode <- system dosvnadd1
         exitcode <- system dosvnadd2
         return()


exportPackage :: View -> String -> CouplingMessages -> (EntityFullName,String,Maybe MMiSSOntology, [EntityFullName]) -> IO()
exportPackage view couplingDir messages (packagePath, packageFSPath, onto, _) = 
  do
    putStrLn "  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn ("  Exporting Package " ++ (toString packagePath) ++ "\n    into file " ++ packageFSPath)
    putStrLn "  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    linkedObjectOpt <- lookupLinkedObjectByFullName view packagePath
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
                        return()
{--
                        if ok 
                          then do ok <- displayImportExportErrors True 
                                           (printImports linkedObject couplingDir (packagePath, packageFSPath) view)
                                  return()
                          else return()
--}
{--
updateOntology :: View -> CouplingMessages ->  [(EntityFullName, String, Maybe MMiSSOntology)] 
                   -> EntityFullName -> IO()

updateOntology view messages updatedPackages p =
  do
     folderLinkOpt <- findPackageByFullName view messages p
     case folderLinkOpt of
       Nothing -> return()
       Just(folderLink) ->
         do 
           importedPackages <- getImportedPackages view folderLink p
--}
  
 

{--

updatedPackages : Liste der Pakete, die in diesem Commit geändert wurden
g : Der kumulierte DependencyGraph. In diesem Graphen ist am Ende für jedes Paar (Paket A, Paket B)
    (A und B aus updatedPackages) für das gilt: A importiert B (auch transitiv über ein nicht geändertes
    Paket) eine Kante 'B vor A'. Das 'vor' meint, dass beim Updaten der Ontologien in den Paketen
    das Paket B vor dem Paket A bearbeitet werden muss.
p:  Das Paket, das gerade aktuell betrachtet wird, d.h. die Import-Beziehungen, die von p ausgehen
    werden daraufhin überprüft, ob ein geändertes Paket importiert wird.
x:  x dient zur Speicherung des Vorgängers im Import-Graphen, zu dem eine Kante eingefügt werden soll.
    Der Algotihmus geht rekursiv vor und betrachtet von p aus alles p's, die dort importiert werden.
    Stösst er auf ein Paket, dass nicht geändert wurde, steigt er aber auch in dessen Import-Beziehungen
    ab, da dort ja noch geänderte Pakete auftauchen können. Trifft er also irgendwann auf ein
    geändertes Paket i, dann muss eine Kante zwischen i und p in den Graphen eingefügt werden, auch wenn
    zwischendrin andere Pakete auf dem Import-Pfad liegen. x dient zur Speicherung von p, wenn rekursiv
    in die Importbeziehungen abgestiegen wird.
     
--}

importDependencies :: View -> CouplingMessages -> [EntityFullName] -> ImportDepGraph 
                        -> (EntityFullName, EntityFullName) -> IO (ImportDepGraph)

importDependencies view messages updatedPackages g (p,x) =
  case (findLNode g p) of
    Just(_) -> return(g)
    Nothing ->  
      do
         let g' = if (p `elem` updatedPackages)
                    then insNode ((head (newNodes 1 g)), p) g
                    else g 
         folderLinkOpt <- findPackageByFullName view messages p
         case folderLinkOpt of
           Nothing -> return(g')
           Just(folderLink) ->
             do importedPackagesOpt <- getImportedPackages view folderLink p
                case importedPackagesOpt of
                  Nothing -> return(g')
                  Just(impPackages) -> foldM insertAndDecend g' impPackages

  where                  
    insertAndDecend :: ImportDepGraph -> EntityFullName -> IO (ImportDepGraph)
    insertAndDecend graph p' =
      if (p' `elem` updatedPackages) 
        then do 
                graph' <- importDependencies view messages updatedPackages graph (p',p')
                return(insertDepLink graph' p' x)
        else importDependencies view messages updatedPackages graph (p',x)

    insertDepLink thisGraph p1 p2 =
      let (thisGraph1, n1) = getInsNode thisGraph p1
          (thisGraph2, n2) = getInsNode thisGraph1 p2
      in insEdge (n1,n2,"vor") thisGraph2

    getInsNode gra label  =
        maybe (let n = head (newNodes 1 gra)
               in ((insNode (n,label) gra), n))
              (\node -> (gra, node))
              (findLNode gra label)



getImportedPackages :: View -> Link MMiSSPackageFolder -> EntityFullName -> IO(Maybe [EntityFullName])
getImportedPackages view folderLink thisPackage =
   do
     packageFolder <- readLink view folderLink
     let preambleLink = toMMiSSPreambleLink packageFolder
     mmisslatexPreamble <- readPreamble view preambleLink
     let importCmdsOpt = importCommands mmisslatexPreamble
     case importCmdsOpt of
       Nothing -> return(Nothing)
       (Just (ImportCommands cmdList)) -> 
         do
--                       putStrLn ("\n  importDependencies: List of import commands for " ++ toString(p) ++ ":\n")
--                       putStrLn ("    " ++ (show cmdList))
            let allImports = map snd (mapMaybe matchAnyImport cmdList)
            case fromWithError (mkAliases (ImportCommands cmdList)) of
              Left mess -> do errorMess ("  Error: getImportedPackages: mkAliases returned: " ++ mess)
                              return(Nothing)
              Right aliases -> 
                do 
                  importedPackages <- mapM (assemblePackagePath aliases thisPackage) allImports
                  return(Just(filter (/= trivialFullName) importedPackages))
--                             putStrLn ("\n  importDependencies: List of filtered imported Packages for " ++ toString(p) ++ ":\n")
--                             putStrLn ("    " ++ (show filteredImpPackages))
  where
    assemblePackagePath :: Aliases -> EntityFullName -> EntitySearchName -> IO (EntityFullName)
    assemblePackagePath aliases packageFullName name =
      do 
        let expandedName = expandAliases aliases name
        case (fromWithError (toEntityFullName packageFullName expandedName)) of
          Left mess -> do errorMess ("  Error: assemblePackagePath: " ++ mess)
                          return(trivialFullName)
          Right (a) -> return(a)


{--
  updateImportInfo bekommt die Infos zu einem geänderten oder neu importieren Package,
  sucht aus der LaTeX-Präambel die Import-Kommandos heraus und trägt in den importierten
  Packages die Beziehung zum improtierenden Package nach. 
  Wenn also 'A importiert B' gegeben ist, dann enthält hinterher die Präambel von B die Info
  'B wird importiert von A'.
--}

updateImportInfo :: View -> CouplingMessages -> (EntityFullName,String,Maybe MMiSSOntology, [EntityFullName]) -> IO()
updateImportInfo view messages (packageFullName, packagePath, ontologyMB, oldImpPackages) =
  do 
     folderLinkOpt <- findPackageByFullName view messages packageFullName
     case folderLinkOpt of
       Nothing -> return()
       Just(folderLink) ->
         do 
            importedPackagesOpt <- getImportedPackages view folderLink packageFullName
            case importedPackagesOpt of
              Nothing -> if (null oldImpPackages)
                           then  return()
                           else  mapM_ (updateImportInfo1 view messages (filter (/= packageFullName))) oldImpPackages
              Just (importedPackages) ->
                do 
                   deletedImports <- return(filter (`notElem` importedPackages) oldImpPackages)
                   mapM_ (updateImportInfo1 view messages (filter (/= packageFullName))) deletedImports
                   mapM_ (updateImportInfo1 view messages (++ [packageFullName])) importedPackages

  where 
    updateImportInfo1 :: View -> CouplingMessages -> ([EntityFullName] -> [EntityFullName]) -> EntityFullName -> IO ()
    updateImportInfo1 view messages operation importedPackagePath =
      do
         importedFolderLinkOpt <- findPackageByFullName view messages importedPackagePath
         case importedFolderLinkOpt of
           Nothing -> return()
           Just(importedFolderLink) ->
             do
                importedPackageFolder <- readLink view importedFolderLink
                let 
                   preambleLink = toMMiSSPreambleLink importedPackageFolder
                importedByList <- readImportedByList view preambleLink
                let 
                   newImportedByList = nub (operation importedByList)
                writeImportedByList preambleLink view newImportedByList
                return()


matchAnyImport :: ImportCommand -> Maybe ([Directive],EntitySearchName)
matchAnyImport (Import directives searchName) = Just((directives, searchName))
matchAnyImport _ = Nothing

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
              -> [(EntityFullName, String, Maybe MMiSSOntology, [EntityFullName])] 
              -> IO ([(EntityFullName, String, Maybe MMiSSOntology, [EntityFullName])])

doUpdates handle versionGraph view couplingDir scriptDir messages packages =
  -- packages = [(package entity name within repos., original file path within Repos. without ext.)]
  do
    line <- hGetLine handle
    hPutStrLn stdout ("\nSOCKET:   " ++ line)
    hFlush stdout
    if (line == "commit")
      then return(packages)
      else    
        do 
          let
             completePath = unbreakName ((breakName couplingDir) ++ (breakName line))
             filePathWOExt = if (isSuffixOf "_ptx.tex" line)
                                     then take ((genericLength line) - 8) line
                                     else if (isSuffixOf ".tex" line)
                                            then take ((genericLength line) - 4) line
                                            else line

          packagePathStripped <- filenameToPackagename completePath line
          fullPackageEntityName <-
            case (fromWithError packagePathStripped) of
               Left mess ->  do putStrLn ("fromWithError packagePathStripped:  " ++ mess)
                                hFlush stdout
			        fail ""
               Right packagePathStr ->
		 case fromWithError (fromStringWE packagePathStr) of
		   Left mess -> do
				  putStrLn ("fromStringWE:  " ++ mess)
                                  hFlush stdout
				  fail ""
		   Right fullPackage -> return(fullPackage)

          putStrLn ("  fullPackageEntityName: " ++ (toString fullPackageEntityName))
	  linkedObjectOpt <- lookupLinkedObjectByFullName view fullPackageEntityName
	  (importedPackageOpt, oldOntology, oldImportedPackages) <-
	    case linkedObjectOpt of
              --
              -- Package is new:
              --
	      Nothing ->
		do     
		   (dirAndPackageOpt :: Maybe (EntityFullName, EntityName)) <- return(entityDirBase fullPackageEntityName)
                   (dirPart, packagePart) <- 
                      case dirAndPackageOpt of
                        Nothing -> fail "  Internal error: Can't decompose path" 
                        Just(n) -> return(n)
                   putStrLn ("   Can't find linked object for " ++ (toString fullPackageEntityName) 
                            ++ "\n   -> Importing it now.")
		   parentFolderLinkWE <- findOrCreateFolder view dirPart
		   case fromWithError parentFolderLinkWE of
		     Left mess -> do
				    putStrLn ("  " ++ mess)
				    errorMess ("Error:\n" ++ mess)
				    return((Nothing,Nothing,[]))
		     Right parentFolderLink ->
                          -- Import the new Package
		       do ok <- importMMiSSPackage1 view parentFolderLink (Just completePath)
			  case ok of
			    True -> 
                               do  
                                  newPackageLinkOpt <- lookupObjectInFolder parentFolderLink packagePart
                                  case newPackageLinkOpt of
                                    Nothing -> return((Nothing,Nothing,[]))
                                    Just newPackageLink -> 
                                      do
                                        newPackageFolder <- readLink view newPackageLink
                                        preambleLink <- return(toMMiSSPreambleLink newPackageFolder)
                                        mmisslatexPreamble <- readPreamble view preambleLink 
                                        ontoWE <- parseMMiSSOntologyFile completePath
                                        importedPackagesOpt <- (getImportedPackages view 
                                                                                    newPackageLink 
                                                                                    fullPackageEntityName)
                                        importedPackages <- return(fromMaybe [] importedPackagesOpt)
                                        case (fromWithError ontoWE) of
                                           Left mess -> 
                                             do putStr ("  Error while parsing ontology:")
                                                putStrLn mess
                                                errorMess ("Error:\n" ++ mess)
                                                return((Just(fullPackageEntityName), Nothing, importedPackages))
                                           Right onto ->
                                               do writeOntology preambleLink view (toFlat onto)
                                                  return((Just(fullPackageEntityName), Nothing, importedPackages))
			    False -> do
				       putStrLn "  Error: Import has failed!"
				       return((Nothing,Nothing,[]))
             --
             --   Package has been found:
             --
	      (Just linkedObject)  -> 
		   do 
		     folderLink1 <- case splitLinkedObject linkedObject of
			MMiSSPackageFolderC folderLink -> return folderLink
			_ -> do
				putStrLn ("  " ++ (toString fullPackageEntityName) ++ " is not a package!")
				errorMess ("Error: " ++ (toString fullPackageEntityName)
					     ++ " is not a package!")
				fail ""
                     packageFolder <- readLink view folderLink1
                     preambleLink <- return(toMMiSSPreambleLink packageFolder)
                     mmisslatexPreamble <- readPreamble view preambleLink 
                     importedPackagesOpt <- getImportedPackages view folderLink1 fullPackageEntityName
                     importedPackages <- return(fromMaybe [] importedPackagesOpt)
                     importedByList <- readImportedByList view preambleLink
--                     putStrLn ("  ImportedByList: " ++ (show importedByList))

                     oldOnto <- readOntology view preambleLink
--                     putStrLn "  Ontology found:"
--                     putStrLn ("    " ++ (show (toFlat oldOnto)))
		     reimportMMiSSPackage1 view folderLink1 (Just completePath)
                     ontoWE <- parseMMiSSOntologyFile completePath
                     case (fromWithError ontoWE) of
                       Left mess -> 
                         do putStr ("  Error while parsing ontology:")
                            putStrLn mess
                            errorMess ("Error:\n" ++ mess)
                            return((Just(fullPackageEntityName)), (Just oldOnto), importedPackages)
                       Right onto ->
                         do writeOntology preambleLink view (toFlat onto)
                            putStrLn "  Written back new preamble successfully."
		            return(Just(fullPackageEntityName),(Just oldOnto), importedPackages)

	  (newPackageList, completeFilePathWOExt) <- 
	     case importedPackageOpt of
	       Nothing -> return((packages,""))
	       Just(name) -> return((packages ++ [(name, filePathWOExt, oldOntology, oldImportedPackages)]), 
                                    (couplingDir `combineNames` filePathWOExt))
	  hFlush handle
	  hFlush stdout
	  result <- try (printMessages messages (Just (completeFilePathWOExt ++ ".err")) False)
	  exitcode <- 
	    case importedPackageOpt of
	       Nothing -> return(ExitSuccess)
	       Just(_) -> system (scriptDir `combineNames` 
				    ("dosvnadd " ++ couplingDir ++ " " ++ (completeFilePathWOExt ++ ".err")))
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
    -- cpath = Complete file path
    -- reppath = File path within Repository
    -- returns: File path within Repository but with Package label as filename at the end.
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
			 newpath1 = filter (/= '\n') (unbreakName ((breakName repdir) ++ (breakName output)))
                         newpath = if (isPrefixOf "./" newpath1) 
                                     then (drop 2 newpath1)
                                     else newpath1
		       return(hasValue(newpath)) 
		 otherwise -> return(hasError ("Package name extraction returned: " ++ (show exitcode)))            



findPackageByFullName :: View -> CouplingMessages -> EntityFullName -> IO (Maybe (Link MMiSSPackageFolder))
findPackageByFullName view messages packageFullName  =
  do
     linkedObjectOpt <- lookupLinkedObjectByFullName view packageFullName
     case linkedObjectOpt of
       Nothing -> do errorMess ("  Error: updateImportInfo: Could not find package '" ++ toString(packageFullName) ++ "'")
                     return(Nothing)
       Just(linkedObject) ->
         case splitLinkedObject linkedObject of
            MMiSSPackageFolderC folderLink -> return (Just folderLink)
	    _ -> do errorMess ("  Error: updateImportInfo: " ++ (toString packageFullName)
	                       ++ " is not a package!")
		    return(Nothing)
           


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

getLastVersion :: VersionGraph -> IO (Maybe VersionInfo.VersionInfo)
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
 
     if (versionInfos2 == []) 
       then return(Nothing)
       else return(Just(maximum versionInfos2))


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

{--
getLinkedObject :: View -> EntityFullName -> IO (Maybe LinkedObject)
getLinkedObject view fullName =
   lookupLinkedObjectByFullName view fullName
--}

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


-- toEntityFullName bekommt den FullName eines PackageFolders und einen relativen
-- Pfad aus einem Import-Statement und liefert den FullName zum importieren Package zurück.
-- Beispiel: 
--     ProposalsAndReports/ProposalPhase2/I4_SPIN_Proposal/P1_Package
--     importiert Parent/P2_Package
--     liefert dann:  ProposalsAndReports/ProposalPhase2/I4_SPIN_Proposal/P2_Package
--
-- FromCurrent und FromHere dürfen eigentlich nicht als Präfix des Searchnames auftauchen,
-- weil der folgende FullName nur noch Objekte innerhalb des impotierenden Packages bezeichnen
-- können. Import-Statements dürfen aber nur Pfade auf ausserhalb liegende Packages enthalten,
-- müssen also mit FromParent oder FromRoot anfangen. FromAbsoulte ist auch nicht zu erwarten, 
-- weil dieses nur intern eingesetzt wird.

toEntityFullName :: EntityFullName -> EntitySearchName -> WithError(EntityFullName)

toEntityFullName _ (FromRoot fullName) = hasValue(fullName)
toEntityFullName _ (FromAbsolute fullName) = hasError("toEntityFullName: FromAbsolute is no valid path element for an imported Package.")
toEntityFullName (EntityFullName []) (FromParent _) = hasError("toEntityFullName: Can't add EntitySearchName to an empty EntityFullName")
toEntityFullName (EntityFullName fullName1) (FromHere (EntityFullName fullName2)) =  
  hasValue(EntityFullName (fullName1 ++ fullName2))
toEntityFullName (EntityFullName fullName1) (FromCurrent (EntityFullName fullName2)) = 
  hasValue(EntityFullName (fullName1 ++ fullName2))
toEntityFullName (EntityFullName fullName) (FromParent s) =
  toEntityFullName (EntityFullName (take ((genericLength fullName)-1) fullName)) s

-- catchErrors also catches other Haskell exceptions.
-- catchErrors :: IO a -> IO (Either String a)
-- catchErrors act = catchImportExportErrors (makeOtherExcepsToOurs act)



findLNode :: Gr EntityFullName String -> EntityFullName -> Maybe Node

findLNode gr label = case (gsel (\(p,v,l,s) -> l == label) gr) of
                      [] -> Nothing
                      conList -> Just(node' (head conList))               



toSortedList :: Eq b => Gr a b -> b -> [a]

toSortedList graph edgeLabel = mapMaybe (lab graph) (foldl insertOrdNode [] (nodes graph))
  where
    insertOrdNode :: [Node] -> Node -> [Node]
    insertOrdNode ordList node =
      if (node `elem` ordList)
        then ordList
        else let newList = foldl insertOrdNode ordList (successors node)
             in (node:newList)
    successors n = map fst (filter (\(_,b) -> (b == edgeLabel)) (lsuc graph n))
