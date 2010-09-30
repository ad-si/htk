module OntologyImport (
  printImports -- :: LinkedObject -> EntityFullName -> View -> IO()
) where

import IO
import System
import Data.Maybe
import Data.List

import View
import Aliases
import Computation
import AtomString
import ExtendedPrelude
import FileNames
import CopyFile

import MMiSSSplitLink
import MMiSSPackageFolder
import Link
import LinkManager
import EntityNames
import MMiSSPreamble
import LaTeXPreamble
import OntoParser



printImports :: LinkedObject -> String -> (EntityFullName,String) -> View -> IO(Bool)
printImports sourcePackageLinkedObject couplingDir (packageFullName,packageFSPath) view =
  do
     -- Link to PackageFolder des importierenden Paketes:
     folderLink1Opt <- case splitLinkedObject sourcePackageLinkedObject of
                           MMiSSPackageFolderC folderLink -> return (Just folderLink)
                           _ -> do
                                  putStrLn "Link is not a PackageFolder."
                                  hFlush stdout
                                  return(Nothing)
     case folderLink1Opt of
        Nothing -> return(False)
        Just(folderLink1) ->
          do
            packageFolder <- readLink view folderLink1
            let preambleLink = toMMiSSPreambleLink packageFolder
            mmisslatexPreamble <- readPreamble view preambleLink
            let importCmdsOpt = importCommands mmisslatexPreamble
                ontFilename = couplingDir `combineNames` (packageFSPath ++ ".imp")
            -- Gibt es Import-Kommandos in diesem Paket?:
            case importCmdsOpt of
              Nothing -> do resultWE <- copyStringToFileCheck " " ontFilename
                            return(False)
              (Just (ImportCommands commandList)) ->
                do
                  let -- globalImportCommands :: [([Directive],EntitySearchName)]
                      globalImports = map snd (mapMaybe (matchImport True) commandList)
                  case fromWithError (mkAliases (ImportCommands commandList)) of
                    Left mess -> do resultWE <- copyStringToFileCheck " " ontFilename
                                    putStrLn mess
                                    hFlush stdout
                                    return(False)
                    Right aliases ->
                      do
--                        let
--                           expandedImports = map (expandAliases aliases) globalImports
                        -- �ber alle importierten Packages hinweg die Ontologien zusammensammeln:
                        resultList <- mapM (printImportedPackage view packageFolder
                                                                 packageFullName aliases) globalImports
                        let (latexStrings,commandStrings) = unzip resultList
                            cmdStr = concat commandStrings
                            ontStr = concat latexStrings
                        let
                            externalStr = ontStr ++ "\n\\def\\Basis{\\jobname .pdf}\n\\def\\CheckImport#1{" ++ cmdStr
                                          ++ "                  }\n"
                        resultWE <- copyStringToFileCheck externalStr ontFilename
--                        putStrLn externalStr
--                        putStrLn ontFilename
--                        hFlush stdout
                        return(True)
       --                 case fromWithError resultWE of
       --                    Left mess -> do putStrLn mess
       --                                    hFlush stdout
       --                    Right _ -> return()



printImportedPackage :: View -> MMiSSPackageFolder -> EntityFullName -> Aliases -> EntitySearchName -> IO(String,String)
printImportedPackage view packageFolder packageFullName aliases name =
  do
     let
        expanded = expandAliases aliases name

     importedPackagePath <- case (fromWithError (toEntityFullName packageFullName expanded)) of
                               Left mess -> do putStrLn ("  printImportedPackage: " ++ mess)
                                               hFlush stdout
                                               return(trivialFullName)
                               Right a -> return(a)

     linkedObjectOpt <- lookupLinkedObjectByFullName view importedPackagePath

     case linkedObjectOpt of
        Nothing -> do putStrLn ("  printImportedPackage: Imported Package not found:\n     "
                                 ++ (toString importedPackagePath))
                      hFlush stdout
                      return("","")

        (Just linkedObject)  ->
          do
            folderLink <- case splitLinkedObject linkedObject of
                              MMiSSPackageFolderC folderLink -> return folderLink
                              _ -> do
                                     putStrLn ("  printImportedPackage:" ++ (toString importedPackagePath)
                                               ++ "\n     is not a package!")
                                     hFlush stdout
                                     fail ""
            let relative = case (toRelativeSearchName packageFullName expanded) of
                              Nothing -> ""
                              Just(s) -> toString s
            putStrLn ("    Imported Package with fullName " ++ (toString importedPackagePath) ++ " found")
            hFlush stdout
            pairResult <- generateExternalOntStr view folderLink relative
            hFlush stdout
            return(pairResult)


generateExternalOntStr :: View -> Link MMiSSPackageFolder -> String -> IO(String,String)
generateExternalOntStr view folderLink relativePath =
 do
     newRelativePath <- return(convertParentStrings relativePath)
{--     folderLink <- case splitLinkedObject link of
                     MMiSSPackageFolderC folderLink -> return folderLink
                     _ -> do
                            putStrLn "Link is not a PackageFolder."
                            hFlush stdout
                            fail ""
--}
     packageFolder <- readLink view folderLink
     let preambleLink = toMMiSSPreambleLink packageFolder
     mmisslatexPreamble <- readPreamble view preambleLink
     case (ontologyFrags mmisslatexPreamble) of
        [] -> do putStrLn "     No Ontology found!"
                 hFlush stdout
        list -> putStrLn "      Ontology found."
     let ontoFrags = map read (ontologyFrags mmisslatexPreamble)
         ifStatements = map (createLaTeX (newRelativePath ++ ".pdf")) ontoFrags
         (latexStrings, cmdStrings) =  unzip ifStatements
         latexStr = concat latexStrings
         cmdStr = concat cmdStrings
     return(latexStr, cmdStr)

  where
     convertParentStrings :: String -> String
     convertParentStrings source =
        let newStrList = [replaceParent x | x <- breakName source]
        in unbreakName newStrList

     replaceParent :: String -> String
     replaceParent "Parent" = ".."
     replaceParent s = s


createLaTeX :: String -> OFrag -> (String,String)
createLaTeX filename (ClassDeclFrag (classDecl)) =
  let name = className classDecl
      latexStr = "\\Class{" ++ name ++ "}{" ++ (classText classDecl) ++ "}{"
                    ++ (fromMaybe "" (super classDecl)) ++ "}\n"
      commandStr = "                  \\ifthenelse{\\equal{#1}{" ++ name ++ "}}%\n"
                   ++ "                                 {\\def\\Basis{" ++ filename ++ "}}%\n"
                   ++ "                                 {}%\n"
  in
    (latexStr,commandStr)

createLaTeX filename (ObjectDeclFrag objectDecl) =
  let name = objName objectDecl
      latexStr = "\\Object{" ++ name ++ "}{" ++ (objectText objectDecl) ++ "}{"
                    ++ (instanceOf objectDecl) ++ "}\n"
      commandStr = "                  \\ifthenelse{\\equal{#1}{" ++ name ++ "}}%\n"
                   ++ "                                 {\\def\\Basis{" ++ filename ++ "}}%\n"
                   ++ "                                 {}%\n"
  in
    (latexStr,commandStr)

createLaTeX filename (BaseRelationDeclFrag relDecl) =
  let name = baseRelName relDecl
      latexStr = "\\RelationName{" ++ name ++ "}{" ++ (baseRelationText relDecl) ++ "}\n"
      commandStr = "                  \\ifthenelse{\\equal{#1}{" ++ name ++ "}}%\n"
                   ++ "                                 {\\def\\Basis{" ++ filename ++ "}}%\n"
                   ++ "                                 {}%\n"
  in
    (latexStr,commandStr)


createLaTeX filename (RelationTypeDeclFrag relTypeDecl) =
  let name = nameOfRel relTypeDecl
      card = case fromMaybe "" (multiplicities relTypeDecl) of
               "" -> ""
               str -> "[" ++ str ++ "]"
      source = nameOfSource relTypeDecl
      target = nameOfTarget relTypeDecl
      superRelation = fromMaybe "" (superRel relTypeDecl)
      latexStr = "\\Relation" ++ card ++ "{" ++ name ++ "}{" ++ source ++ "}{" ++ target ++ "}{"
                    ++ superRelation ++ "}\n"
      commandStr = ""
  in
    (latexStr,commandStr)

createLaTeX _ _ = ("","")




matchImport :: Bool -> ImportCommand -> Maybe ([Directive],EntitySearchName)
-- If import command is global and bool is true, or import command is local
-- and bool is false, return its directives and search name.
matchImport mustBeGlobal (Import directives searchName)
      | isGlobal directives == mustBeGlobal
      = Just (directives,searchName)
   where
      isGlobal :: [Directive] -> Bool
      isGlobal directives =
         fromMaybe False
            (findJust
               (\ directive -> case directive of
                  Global -> Just True
                  Local -> Just False
                  _ -> Nothing
                  )
               directives
               )
matchImport _ _ = Nothing




toRelativeSearchName :: EntityFullName -> EntitySearchName ->  Maybe EntitySearchName
toRelativeSearchName _ s@(FromParent s1) = Just(s1)
toRelativeSearchName (EntityFullName []) _ = Nothing
toRelativeSearchName _ s@(FromHere _) = Just(s)
toRelativeSearchName _ s@(FromCurrent _) = Just(s)
toRelativeSearchName _ s@(FromAbsolute _) = Just(s)
toRelativeSearchName fullName (FromRoot searchFullName) = toRelativeSearchName1 fullName searchFullName


toRelativeSearchName1 :: EntityFullName -> EntityFullName ->  Maybe EntitySearchName
toRelativeSearchName1 (EntityFullName []) s@(EntityFullName (firstOfSearch:searchNames)) = Just((FromHere s))

toRelativeSearchName1 f@(EntityFullName (firstOfBase:baseNames)) s@(EntityFullName (firstOfSearch:searchNames)) =
    if (firstOfBase == firstOfSearch)
      then toRelativeSearchName1 (EntityFullName baseNames) (EntityFullName searchNames)
      else let str = createRelPath baseNames s
               fullNameOpt = fromStringWE str
           in case fromWithError fullNameOpt of
                Left mess -> Nothing
                Right fullName -> Just(fullName)
toRelativeSearchName1 _ _ = Nothing



createRelPath :: [EntityName] -> EntityFullName -> String
createRelPath baseNames searchName =
  let parentsStr = foldl toParent "" baseNames
      searchNameStr = case searchName of
                         EntityFullName [] -> ""
                         _ -> toString searchName
  in parentsStr ++ searchNameStr

  where
    toParent :: String -> EntityName -> String
    toParent str _ = str ++ "Parent/"


-- toEntityFullName bekommt den FullName eines PackageFolders und einen relativen
-- Pfad aus einem Import-Statement und liefert den FullName zum importieren Package zur�ck.
-- Beispiel:
--     ProposalsAndReports/ProposalPhase2/I4_SPIN_Proposal/P1_Package
--     importiert Parent/P2_Package
--     liefert dann:  ProposalsAndReports/ProposalPhase2/I4_SPIN_Proposal/P2_Package
--
-- FromCurrent und FromHere d�rfen eigentlich nicht als Pr�fix des Searchnames auftauchen,
-- weil der folgende FullName nur noch Objekte innerhalb des impotierenden Packages bezeichnen
-- k�nnen. Import-Statements d�rfen aber nur Pfade auf ausserhalb liegende Packages enthalten,
-- m�ssen also mit FromParent oder FromRoot anfangen. FromAbsoulte ist auch nicht zu erwarten,
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



{--
data EntitySearchName =
      FromParent EntitySearchName -- go up one directory.
   |  FromHere EntityFullName
   |  FromCurrent EntityFullName
   |  FromRoot EntityFullName
   |  FromAbsolute EntityFullName
   deriving (Eq,Ord,Show)
--}
