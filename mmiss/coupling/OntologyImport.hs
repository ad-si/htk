module OntologyImport (
  printImports -- :: LinkedObject -> EntityFullName -> View -> IO()
) where

import IO
import System
import Maybe
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



printImports :: LinkedObject -> String -> EntityFullName -> View -> IO(Bool)
printImports sourcePackageLinkedObject couplingDir packageFullName view = 
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
                ontFilename = couplingDir `combineNames` ((toString packageFullName) ++ ".imp")
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
                        -- Über alle importierten Packages hinweg die Ontologien zusammensammeln: 
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
--     result <- lookupLinkedObject view sourcePackageLinkedObject name
      -- -> IO (WithError (Maybe LinkedObject))
     result <- lookupMMiSSPackageFolder view packageFolder name
      -- -> IO (WithError (Maybe (Link MMiSSPackageFolder)))

     case fromWithError result of
        Left mess -> do putStrLn ("printImportedPackage: lookupMMiSSPackageFolder returned an error in Package\n  '" 
                                  ++ (toString packageFullName) ++ "': " ++ mess)
                        hFlush stdout
                        return("","")
        Right packageLinkOpt ->
          case packageLinkOpt of
             Nothing -> do putStrLn ("    Imported Package " ++ (toString name) ++ " could not be found")
                           hFlush stdout
                           return("","")
             Just link -> 
                do
                  let   
                      expanded = expandAliases aliases name
                      relative = case (toRelativeSearchName packageFullName expanded) of
                                    Nothing -> ""
                                    Just(s) -> toString s
                  putStrLn ("    Imported Package with fullName " ++ (toString expanded) ++ " found")
                  hFlush stdout
                  pairResult <- generateExternalOntStr view link relative
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