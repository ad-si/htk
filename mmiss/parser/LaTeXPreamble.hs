module LaTeXPreamble (

   MMiSSLatexPreamble,
   MMiSSExtraPreambleData(..),
   emptyMMiSSLatexPreamble,
   emptyLaTeXPreamble, 
   parsePreamble,
   extractPreamble,  -- :: [Frag] -> WithError(Maybe MMiSSLatexPreamble, Frag)
   mergePreambles,   -- :: [MMiSSLatexPreamble] -> (MMiSSLatexPreamble,[String]) 

   importCommands, -- :: MMiSSLatexPreamble -> Maybe ImportCommands

  )
where

import ExtendedPrelude(mapEq)
import List
import Computation hiding (try)
import Dynamics
import AtomString

import LaTeXParserCore
import Parsec
import ParsecError
import EntityNames
import CodedValue


type PackageName = String
type Options = [String]
type Versiondate = String

data Package = Package Options PackageName Versiondate deriving (Show,Typeable)
type DocumentClass = Package


data MMiSSLatexPreamble = MMiSSLatexPreamble { 
  latexPreamble :: LaTeXPreamble,
  importCommands :: Maybe ImportCommands
} deriving (Typeable)

emptyMMiSSLatexPreamble = MMiSSLatexPreamble {latexPreamble = emptyLaTeXPreamble, importCommands = Nothing}

data LaTeXPreamble = Preamble DocumentClass [Package] String 
   deriving (Show,Typeable)

emptyLaTeXPreamble = Preamble (Package [] "mmiss" "") [] ""

newtype MMiSSExtraPreambleData = MMiSSExtraPreambleData {
   callSite :: Maybe EntitySearchName
      -- How the element to which this preamble belongs was referred to
      -- originally by the user.
      -- Nothing means that this is the head element.
   }

-- Die beiden folgenden Funktionen legen den String fest, der den Anfang und das Ende der
-- von MMiSS generierten Input-Preamble markiert. Zum Vergleich wird der gesamte String
-- verwendet:

startInputPragma = "%% MMiSSLaTeX input preamble"
endInputPragma = "%% End of MMiSSLaTeX input preamble"

specialTreatmentInPreamble = ["documentclass", "usepackage", "Path", "Import"]


mergePreambles :: [MMiSSLatexPreamble] -> (MMiSSLatexPreamble,[String])
mergePreambles [preamble] = (preamble,[])
mergePreambles (preamble:_) = (preamble,[
   "All preambles but the first thrown away; complain to Achim!!!"])
mergePreambles [] = (emptyMMiSSLatexPreamble,[])


extractPreamble :: [Frag] -> WithError(Maybe MMiSSLatexPreamble, Frag)
extractPreamble fs = findFirstEnv fs [] True  


{-- findFirstEnv geht den vom Parser erzeugten abstrakten Syntaxbaum (AST) durch, extrahiert die Preamble
    und gibt das Root-Fragment zurück. Die Funktion sucht innerhalb des Environments 'Root' (vom Parser obligatorisch
    erzeugte Wurzel jedes AST) nach dem ersten MMiSSLatex-Environment oder dem 'document'
    Environment (\begin{document}). 
    Findet es zuerst das document-Env., wird die Suche nach einem MMiSSLatex-Env. in dessen
    Kindern fortgesetzt. Gleichzeitig wird das Aufsammeln von Preamble-Fragmenten beendet.
    Findet es innerhalb des document-Envs ein package-Env, dann wird dieses als Wurzel-Fragment
    zurückgegeben. Am Ende wird die Präambel erzeugt. Enthaelt die document-Umgebung nach dem Package-Env.
    noch MMiSSLatex- oder Latex-Fragmente, so werden diese ignoriert.
    Trifft die Funktion innerhalb des Root- oder document-Fragments auf eine andere MMiSSLatex-Umgebung
    als 'package', dann wird angenommen, dass es sich um ein Teildokument handelt, das ausgecheckt wurde.
    Der Inhalt dieses Elementes (das kann eine Unit sein, aber auch ein Atom - z.B. ein TextFragment)
    wird als Wurzel-Fragment zurückgegeben.
    Hier wird jedoch keine Praeambel hinzugefuegt, weil es sich nur um ein Teildokument handelt,
    das ohne Praeambel ausgecheckt wurde.
--}

findFirstEnv :: [Frag] -> [Frag] -> Bool -> WithError (Maybe MMiSSLatexPreamble, Frag)

findFirstEnv ((Env "Root" _ fs):[]) preambleFs _  = findFirstEnv fs preambleFs True
findFirstEnv ((Env "document" _ fs):_) preambleFs _ = findFirstEnv fs preambleFs False
findFirstEnv ((Env "Package" ps@(LParams _ packAtts _ _) fs):_) preambleFs beforeDocument = 
  let (newPreambleFs, atts1) = addPropertiesFrag preambleFs packAtts
      latexPre = makePreamble (filterGeneratedPreambleParts newPreambleFs)
      importCmds = makeImportCmds newPreambleFs []
  in case fromWithError latexPre of
	Left str -> hasError(str)
	Right(lp) -> 
	  case lp of 
	     Just(p) -> 
	       case fromWithError(importCmds) of
		 Right(impCmds) -> hasValue (Just(MMiSSLatexPreamble {
						   latexPreamble = p,
						   importCommands = impCmds
						  }),
                                             (Env "Package" ps fs))
		 Left str -> hasError(str)
	     Nothing -> 
	       case fromWithError(importCmds) of
		 Right(_) -> 
		   hasError ("Insufficient preamble: Only found import commands.")
		 Left(err) -> hasError("Insufficient preamble: Only found import commands.\n" ++ err)

findFirstEnv ((Env name ps fs):rest) preambleFs beforeDocument = 
  if (name `elem` (map fst (mmissPlainTextAtoms ++ envsWithText ++ envsWithoutText))) 
    then hasValue(Nothing, (Env name ps fs))
    else if (name `elem` (map fst mmiss2EnvIds)) 
           -- Env must be a link or Reference-Element: ignore it
           then findFirstEnv rest preambleFs beforeDocument
           -- Env ist plain LaTeX:
           else if (not beforeDocument)
	          -- Env is in document-Env but is not MMiSSLatex: pull out content and search there as well.
                  -- Throws away this env:
                  then findFirstEnv (fs ++ rest) preambleFs False
                  -- We are before document env and it is no MMiSSLatex: add to preamble-Fragments
                  else findFirstEnv rest (preambleFs ++ [(Env name ps fs)]) True

-- Frag is no Environment: Must be Command, Other or Escaped Char.
-- We are before \begin{document}, so add to preamble: 
findFirstEnv (f:fs) preambleFs True = findFirstEnv fs (preambleFs ++ [f]) True

-- We are in the document but before the package env. or some other env. We decided to pull the Fragments
-- found here out to the Preamble. So they will be listed before the \begin{document} once the user
-- checks out the MMiSSLaTeX document:
findFirstEnv (f:fs) preambleFs False = findFirstEnv fs (preambleFs ++ [f]) False

findFirstEnv [] _ _  = hasError("No root environment ('package' or some other env.) found!")           



-- makePreamble erzeugt aus den Präambel-Fragmenten die LaTeXPreamble-Datenstruktur.
-- Die von MMiSS erzeugten Input-Kommandos müssen vorher ausgefiltert worden sein.
-- Erkannt wird der \documentclass sowie die \usepackage-Befehle. Alle anderen in der Fragmentliste
-- befindlichen Kommandos, Strings oder EscapedChars, die der Author später wieder in der Latex-Quell
-- benötigt,  werden im String 'rest' aufgesammelt.  
-- Die Funktion ignoriert dabei alle Kommandos, deren Namen in der List 'specialTreatmentInPreamble'
-- auftauchen, da diese gesondert behandelt werden. Diese Kommandos werden auch nicht in den 
-- 'rest'-String übernommen.

makePreamble :: [Frag] -> WithError (Maybe LaTeXPreamble)
makePreamble [] = hasValue(Nothing)
makePreamble (f:fs) =
  case f of
    (Command "documentclass" ps) ->
      let packages = makePrePackages fs []
          rest = makePreRest fs ""
          documentClass = makePrePackage f
      in hasValue(Just(Preamble documentClass packages rest))
    (Command _ _) -> hasError("LaTeX-Preamble must begin with \\documentclass!")
    _ -> makePreamble fs


makePrePackages :: [Frag] -> [Package] -> [Package]
makePrePackages [] inList = inList
makePrePackages (f1:fs) inList = 
  case f1 of
     (Command "usepackage" ps) -> makePrePackages fs (inList ++ [(makePrePackage f1)])
     _ -> makePrePackages fs inList

makePrePackage :: Frag -> Package
makePrePackage (Command name (LParams _ atts _ _)) = 
  let options = [ op | (t, op) <- atts, t=="option"]
      versiondate = case (lookup "versiondate" atts) of
                      (Just(date)) -> date
                      Nothing -> ""
      packageName = case (lookup "name" atts) of
                      (Just(date)) -> date
                      Nothing -> ""
  in Package options packageName versiondate
makePrePackage _ = Package [] "" ""

makePreRest :: [Frag] -> String -> String
makePreRest [] inStr = inStr
makePreRest (f:[]) inStr =
  case f of
    (Command name _) ->
       if (name `elem` specialTreatmentInPreamble) 
         then inStr
         else inStr ++ (makeTextElem [f] "")
    _ -> inStr

makePreRest (f1:(f2:fs)) inStr =
  case f1 of
     (Command name _) -> 
        if (name == "documentclass") || (name == "usepackage") 
          then case f2 of
                 (Other str) -> if (length (filter (not . (`elem` "\n\t ")) str) == 0)
                                  then makePreRest fs inStr
                                  else makePreRest fs (inStr ++ str)
                 (EscapedChar c) -> makePreRest fs inStr
                 _ -> makePreRest (f2:fs) inStr
          else 
            if (name `elem` specialTreatmentInPreamble) 
              then makePreRest (f2:fs) inStr
	      else makePreRest (f2:fs) (inStr ++ (makeTextElem [f1] ""))
     _ -> makePreRest (f2:fs) (inStr ++ (makeTextElem [f1] ""))


unionPreambles :: [MMiSSLatexPreamble] -> Maybe MMiSSLatexPreamble
unionPreambles [] = Nothing
unionPreambles (p:[]) = Just(p)
unionPreambles (p1:p2:ps) = 
  let latexPre1 = latexPreamble p1
      latexPre2 = latexPreamble p2
      unionPre = union2Preambles latexPre1 latexPre2
      newPreamble = MMiSSLatexPreamble {latexPreamble = unionPre, 
                                        importCommands = (importCommands p1)}
  in unionPreambles (newPreamble:ps)

union2Preambles :: LaTeXPreamble -> LaTeXPreamble -> LaTeXPreamble
union2Preambles (Preamble (Package classOpt1 className versiondate) packages1 rest1) (Preamble (Package classOpt2 _ _) packages2 rest2) = Preamble (Package (nub (classOpt1 ++ classOpt2)) className versiondate) (unionPackages packages1 packages2) (rest1 ++ rest2)

unionPackages :: [Package] -> [Package] -> [Package]
unionPackages ps1 ps2 = nubBy eqPackage (ps1 ++ ps2)

eqPackage :: Package -> Package -> Bool
eqPackage (Package opt1 name1 version1) (Package opt2 name2 version2)  = 
  (opt1 == opt2) && (name1 == name2) && (version1 == version2)
             


{--
   parsePreamble is used as fromStringWE-method in the instanciation for
   MMiSSLatexPreamble as StringClass. 
--}

parsePreamble :: String -> WithError MMiSSLatexPreamble
parsePreamble s = 
  let result = parseFrags s
  in
    case result of
      Right fs  -> 
         let preambleEl = makePreamble fs
             impCmds = makeImportCmds fs []
             bothEl = fromWithError(pairWithError preambleEl impCmds)
         in case bothEl of
              Right (preambleEl, impCmds) -> 
                case preambleEl of
                  Just(p) -> hasValue( MMiSSLatexPreamble { 
                                             latexPreamble = p, 
                                             importCommands = impCmds
                                       })
                  Nothing -> hasError("Strange: makePreamble returns no error and no preamble.")
	      Left err -> hasError(show err)
      Left err -> hasError (show err)


{-- addPropertiesFrag bekommt die Fragmente der Präambel sowie die Attribute, die am Package-Env.
definiert wurden übergeben und erzeugt daraus eine geänderte Liste von Präambel-Fragmenten.
Dazu wird das \Properties-Kommando aus der Präambel herausgefiltert und dessen Attributwerte mit denen
des Packages vereiningt. Diese neuen Attributwerte werden wiederum als \Properties-Fragment codiert
und in die zurückgegebene Fragment-Liste eingefügt. Im Prinzip werden also die Package-Attributwerte
in das \Properties-Fragment, das in der Präambel steht, hineingesetzt.
--}
addPropertiesFrag :: [Frag] -> Attributes -> ([Frag], Attributes)
addPropertiesFrag preambleFs packAtts =
  let propAtts = case (getProperties preambleFs) of 
                   Just((LParams _ atts _ _)) -> atts
                   Nothing -> []
      atts1 = filter ((/= "Label").fst) (unionAttributes packAtts propAtts)
      newPropertiesFrag = (Command "Properties" (LParams [] atts1 Nothing Nothing))
  in ((filterProperties preambleFs) ++ [newPropertiesFrag], atts1)


filterProperties :: [Frag] -> [Frag]
filterProperties ((Command "Properties" _):fs) = fs
filterProperties (f:fs) = [f] ++ (filterProperties fs)
filterProperties [] = []


getProperties :: [Frag] -> Maybe Params
getProperties ((Command "Properties" ps):fs) = Just(ps)
getProperties (f:fs) = getProperties fs
getProperties [] = Nothing



-- filterGeneratedPreambleParts filtert aus den Präambelfragmenten die von MMiSS beim Auschecken
-- generierten Anteile heraus. Dies sind zur Zeit die Include-Kommandos etc., die auf LaTeX-Ebene
-- benötigt werden, um die importierten semantischen Elemente (für die auch LaTeX-Kommandos definiert
-- sind), in dieser LaTeX-Quelle bekfindIndex (stringInFrag inputPragmaStart) fsannt zu machen.
-- TODO:  Es muss noch überprüft werden, ob Start- und Endpragma überhaupt vorkamen und ob sie in 
-- der richtigen Reihenfolge aufgetaucht sind.

filterGeneratedPreambleParts :: [Frag] ->[Frag]

filterGeneratedPreambleParts fs = 
  let firstPart = takeWhile (not . (stringInFrag startInputPragma)) fs
      secondPart = dropWhile (not . (stringInFrag endInputPragma)) fs
  in firstPart ++ secondPart


stringInFrag :: String -> Frag -> Bool

stringInFrag inStr (Other str) =  (str == inStr) 
stringInFrag _ _ = False




-- makeImportCmds bekommt die Präambel-Fragmente übergeben, sucht darin die
-- Import-Kommandos (\Path und \Import), parsed deren Argumente und generiert
-- daraus die ImportCommands:

makeImportCmds :: [Frag] -> [ImportCommand] -> WithError (Maybe ImportCommands)

makeImportCmds (cmd@(Command "Path" (LParams singleParams _ _ _)):fs) importCmds =
  case singleParams of
    (SingleParam ((Other aliasStr):_) _) : (SingleParam ((Other packageNameStr):_) _)  : _
      -> let aliasEl = parse entityNameParser1 "" aliasStr
             packageNameEl = parse entitySearchNameParser1 "" packageNameStr
         in case aliasEl of
               Right alias  -> 
                 case packageNameEl of 
                   Right packageName -> makeImportCmds fs (importCmds ++ [(PathAlias alias packageName)])
                   Left err -> hasError ("Parse error in second argument of Path-Command:\n"
                                     ++ ((show cmd) ++ "\nError:\n")
                                     ++ show err)
               Left err -> hasError ("Parse error in first argument of Path-Command:\n"
                                   ++ ((show cmd) ++ "\nError:\n")
                                   ++ show err)
    otherwise -> hasError("The following Path-Command in the Import-preamble has to few or wrong arguments:\n"                      ++ (show cmd))

makeImportCmds (cmd@(Command "Import" (LParams singleParams _ _ _)):fs) importCmds =
  case singleParams of
    -- Matcht auf \Import[]{packageName}: Der leere optionale Parameter führt beim Parsen
    -- zu einem SingleParam mit leerem String:
    (SingleParam [] _) : (SingleParam ((Other packageNameStr):_) _)  : _
       -> genPackageName packageNameStr

    -- Matcht \Import{packageName}  (keine Direktiven angegeben): 
    (SingleParam ((Other packageNameStr):_) _) : []
       -> genPackageName packageNameStr

    -- Matcht auf \Import[directives]{packageName}:
    (SingleParam ((Other directivesStr):_) _) : (SingleParam ((Other packageNameStr):_) _)  : _
      -> let packageNameEl = parse entitySearchNameParser1 "" packageNameStr
             directivesEl = parse (directivesParser []) "" directivesStr
         in case directivesEl of
               Right directives  -> 
                 case packageNameEl of 
                   Right packageName -> makeImportCmds fs (importCmds ++ [(Import directives packageName)])
                   Left err -> hasError ("Parse error in second argument of Import-Command:\n"
                                     ++ ((show cmd) ++ "\nError:\n")
                                     ++ show err)
               Left err -> hasError ("Parse error in directives of Import-Command:\n"
                                   ++ ((show cmd) ++ "\nError:\n")
                                   ++ show err)
    otherwise -> hasError("The following Import-Command in the Import-preamble has to few or wrong arguments:\n"
                          ++ (show cmd))
    where
       genPackageName :: String -> WithError (Maybe ImportCommands)
       genPackageName packageNameStr = 
         let packageNameEl = parse entitySearchNameParser1 "" packageNameStr
             cmdStr = makeTextElem [cmd] ""
         in case packageNameEl of 
              Right packageName -> makeImportCmds fs (importCmds ++ [(Import [] packageName)])
              Left err -> hasError ("Parse error in Package-name argument of Import-Command:\n"
                                     ++ cmdStr ++ "\nError:\n"
                                     ++ show err)

makeImportCmds (f:fs) importCmds = makeImportCmds fs importCmds

makeImportCmds [] importCmds = if ((genericLength importCmds) == 0)
                                 then hasValue(Nothing)
                                 else hasValue(Just((ImportCommands importCmds)))


-- -------------------------------------------------------------------------------------------
--
-- Die nachfolgenden Parser werden zum parsen der Import-Statements in der Präambel benutzt:
--
-- simpleDirectiveParser erkennt die einfachen Import-Direktiven: Global, Local, Qualified, Unqualified:
--
-- -------------------------------------------------------------------------------------------

simpleDirectiveParser :: GenParser Char st [Directive]
simpleDirectiveParser = 
  try(do spaces
         str <- choice ((try (string "Global"))
                        :(try (string "Local"))
			:(try (string "Qualified"))
			:(try (string "Unqualified")):[])
         spaces
         case str of
           "Global" -> return([Global])
           "Local" ->  return([Local])
           "Qualified" -> return([Qualified])
           "Unqualified" -> return([Unqualified]))

-- Parst Hide- und Reveal-Direktiven, die eine Liste von Namen als Parameter haben:
hideRevealDirectiveParser :: GenParser Char st [Directive]
hideRevealDirectiveParser =
  try(do spaces
         string "Hide"
         char '{'
         nameList <- commaSep (entityFullNameParser)
         char '}'
         spaces
         return([Hide nameList]))
  <|> try(do spaces
             string "Reveal"
             char '{'
             nameList <- commaSep (entityFullNameParser)
             char '}'
             spaces
             return([Reveal nameList]))

-- Parst die Liste der Umbenennungen von Namen (Rename-Direktive):
renameDirectiveParser :: GenParser Char st [Directive]
renameDirectiveParser =
  try(do spaces
         dirStr <- string "Rename"
         char '{'
         nameList <- commaSep namePairParser
         char '}'
         spaces
         return(nameList))

-- Parst ein name=name-Paar aus der Rename-Liste (siehe renameDirectiveParser):
namePairParser ::  GenParser Char st Directive
namePairParser = try( do spaces
                         firstName <- entityNameParser
                         spaces
                         char '='
                         spaces
                         secondName <- entityFullNameParser
                         spaces
                         return(Rename firstName secondName))

-- Parst eine einzelne Direktive:
directiveParser :: GenParser Char st [Directive]
directiveParser = renameDirectiveParser
                  <|> hideRevealDirectiveParser 
                  <|> simpleDirectiveParser
                  <?> "valid directive in import command."

-- directivesParser parst die Direktiven eines Import-Statements
directivesParser :: [Directive] -> GenParser Char st [Directive]
directivesParser ds = 
  do d <- directiveParser
     sep <- option "" (string ",")
     if (sep == "")
       then return(ds ++ d)
       else directivesParser (ds ++ d)




-- makePreambleText erzeugt aus einer MMiSSLatexPreamble-Datenstruktur einen String.
-- Wird als toString-Funktion in der Deklarierung von MMiSSLatexPreamble als Instanz von
-- StringClass verwendet.

makePreambleText :: MMiSSLatexPreamble -> String
makePreambleText mmissPreamble = 
  let (Preamble documentClass packages rest) = latexPreamble mmissPreamble
      str1 = (makePackageText "documentclass" documentClass)
               ++ (concat (map (makePackageText "usepackage") packages)) 
               ++ rest
      impCmds = importCommands mmissPreamble
      str2 = case impCmds of
               Just(cmds) -> makeImportsText cmds
               Nothing -> ""
  in str1 ++ "\n" ++ str2 ++ "\n"

makePackageText :: String -> Package -> String
makePackageText commandName (Package options name versiondate) =
  let optsRaw = concat (map ("," ++) options)
      opts = if (optsRaw == "")
               then "[]"
               else ("[" ++ (drop 1 optsRaw) ++ "]") 
      optStr = if (opts == "[]") then "" else opts
      versionStr = if (versiondate == "") then "" else ("[" ++ versiondate ++ "]")
  in "\\" ++ commandName ++ optStr ++ "{" ++ name ++ "}" ++ versionStr ++ "\n"


makeImportsText :: ImportCommands -> String
makeImportsText (ImportCommands cmds) = 
  let impStrs = map makeImportCmdText cmds
  in concat (map (++ "\n") impStrs)

makeImportCmdText :: ImportCommand -> String
makeImportCmdText (Import ds packageFullName) =
  let renStr = collectRenames ds ""
      newDs = filter (not.isRename) ds
      dirStr = if ((genericLength newDs) > 0)
                 then drop 2 (concat (map makeDirectiveText newDs))
                 else ""
      tmpStr = if ((genericLength renStr) > 1) && ((genericLength dirStr) > 1)
                 then ", "
                 else ""
      packageNameStr = toString packageFullName 
      directivesStr = if (dirStr ++ tmpStr ++ renStr) == "" 
                        then ""
                        else "[" ++ (dirStr ++ tmpStr ++ renStr) ++ "]"
  in "\\Import" ++ directivesStr ++ "{" ++ packageNameStr ++ "}"    

makeImportCmdText (PathAlias alias searchName) =
  "\\Path{" ++ toString alias ++ "}{" ++ toString searchName ++ "}"


makeDirectiveText :: Directive -> String
makeDirectiveText Qualified = ", Qualified"
makeDirectiveText Unqualified = ", Unqualified"
makeDirectiveText Global = ", Global"
makeDirectiveText Local = ", Local"
makeDirectiveText (Hide entityFullNames) = 
  let nameStrList = map toString entityFullNames
      str = if (nameStrList == []) 
              then "" 
              else init (concat (map (++ ",") nameStrList))
  in ", Hide{" ++ str ++ "}"
makeDirectiveText  (Reveal entityFullNames) =  
  let nameStrList = map toString entityFullNames
      str = if (nameStrList == []) 
              then "" 
              else init (concat (map (++ ",") nameStrList))
  in ", Reveal{" ++ str ++ "}"

collectRenames :: [Directive] -> String -> String
collectRenames ((Rename newName oldName):ds) str = 
  collectRenames ds (str ++ ", " ++ (toString newName) ++ "=" ++ (toString oldName))
collectRenames (d:ds) str = collectRenames ds str 
collectRenames [] str = 
  if ((genericLength str) > 0) 
    then let str1 = drop 2 str
         in "Rename{" ++ str1 ++ "}"
    else str


isRename :: Directive -> Bool
isRename (Rename _ _) = True
isRename _ = False


{--
unionAttributes :: [Attribute] -> [Attribute] -> [Attribute]
unionAttributes xs ys = unionBy (eqAttPair) xs ys
--}

unionAttributes :: Attributes -> Attributes -> Attributes
unionAttributes xs ys = unionBy (eqAttPair) xs ys

eqAttPair :: Eq a => (a, b) -> (a, b) -> Bool 
eqAttPair x y = (fst x) == (fst y)


---------------------------------------------------------------------------------------------
--
-- MMiSSLatexPreamble is an instance of StringClass
--
---------------------------------------------------------------------------------------------

instance StringClass MMiSSLatexPreamble where
   fromStringWE string = parsePreamble string
   toString preamble = makePreambleText preamble


-- ----------------------------------------------------------------------------------
-- Instances of HasCodedValue and Eq for MMiSSLatexPreamble (added by George)
-- ----------------------------------------------------------------------------------

instance Eq MMiSSLatexPreamble where
   (==) = mapEq 
      (\ (MMiSSLatexPreamble latexPreamble importCommands) ->
         (latexPreamble,importCommands))

instance Monad m => CodedValue.HasBinary MMiSSLatexPreamble m where
   writeBin = mapWrite
      (\ (MMiSSLatexPreamble latexPreamble importCommands) ->
         (latexPreamble,importCommands))
   readBin = mapRead
      (\ (latexPreamble,importCommands) ->
         (MMiSSLatexPreamble latexPreamble importCommands))

instance Eq LaTeXPreamble where
   (==) = mapEq 
      (\ (Preamble documentClass packages string) -> 
         (documentClass,packages,string))   

instance Monad m => CodedValue.HasBinary LaTeXPreamble m where
   writeBin = mapWrite
      (\ (Preamble documentClass packages string) -> 
         (documentClass,packages,string))
   readBin = mapRead
      (\ (documentClass,packages,string) ->
         (Preamble documentClass packages string))


-- ----------------------------------------------------------------------------------
-- Instances of HasCodedValue and Eq for Package (added by George)
-- ----------------------------------------------------------------------------------

instance Eq Package where
   (==) = mapEq
      (\ (Package options packageName versionData) -> (options,packageName,versionData))

instance Monad m => CodedValue.HasBinary Package m where
   writeBin = mapWrite 
      (\ (Package options packageName versionData) -> (options,packageName,versionData))
   readBin = mapRead
      (\ (options,packageName,versionData) -> Package options packageName versionData)

