module LaTeXParser (
   latexDoc,
   parseMMiSSLatex, 
   -- new :: String -> WithError (Element, Maybe MMiSSLatexPreamble)
   -- Turn MMiSSLaTeX into an Element.   
   parseMMiSSLatexFile, 
   -- new :: String -> WithError (Element, Maybe MMiSSLatexPreamble)
   -- The same, for a file.
   makeMMiSSLatex,
   -- :: (Element, Bool, [MMiSSLatexPreamble]) -> WithError (EmacsContent ((String, Char), [Attribute]))
   -- Turns an Element into a MMiSSLaTeX source
   -- If the Bool is set, attaches a preamble.

   classifyLabelledTag, -- :: String -> Maybe Char
   -- Maps an Xml tag to its corresponding mini-type if it has one.
   -- (The mini-type is just something that identifies what sort of
   -- include the String needs.)

   fromIncludeStr, --  String -> Char
   fromIncludeStrOpt, -- String -> Maybe Char
   toIncludeStr, -- Char -> String
   -- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
   -- the corresponding includeXXX command.
   showElement, -- WithError Element -> String
   mapLabelledTag,
   MMiSSLatexPreamble, 
   parseAndShow,
   parseAndMakeMMiSSLatex,
   parsePreamble
   )
 where

-- module LaTeXParser where

#include "config.h"

import IOExts
import List
import Parsec
import Char

#if HAXMLINT
import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.Pretty as PP
import Text.PrettyPrint.HughesPJ hiding (char,space)
#else
import XmlTypes
import qualified XmlPP as PP
import Pretty hiding (char, spaces, space)
#endif

import Dynamics
import Computation hiding (try)
import ParsecError
import EmacsContent
import EntityNames
import AtomString
import CodedValue
-- import EmacsEdit(TypedName)


type EnvId = String
type Command = String
type FormId = String
type LabelId = String
type Title = String
type Attributes = [(String, String)]
type Other = String
type Delimiter = String
type PackageName = String
type Options = [String]
type Versiondate = String

data SingleParam = SingleParam [Frag] Char    deriving Show

data Textmode = TextAllowed | NoText | TextFragment

data Package = Package Options PackageName Versiondate deriving Show
type DocumentClass = Package

data MMiSSLatexPreamble = MMiSSLatexPreamble { 
  latexPreamble :: LaTeXPreamble,
  importCommands :: Maybe ImportCommands
}

data LaTeXPreamble = Preamble DocumentClass [Package] String deriving Show

{--  These structures should be included in MMiSSLatexPreamble : -}

data MMiSSOntology = MMiSSOntology {
  classes :: [ClassDecl],
  objects :: [ObjectDecl],
  relations :: [RelationDecl],
  objectLinks :: [ObjectLink]
} deriving(Show)

data ClassDecl = ClassDecl {
  className :: String,
  classText :: String,
  super :: Maybe String
} deriving(Show)

data ObjectDecl = ObjectDecl {
  objName :: String,
  objectText :: String,
  instanceOf :: String
} deriving(Show)

data RelationDecl = RelationDecl {
  multiplicities :: Maybe String,
  relName :: String,
  relationText :: String,
  source :: String,
  target :: String
} deriving(Show)

data ObjectLink = ObjectLink {
  sourceObj :: String,
  targetObj :: String,
  linkRelation :: String
} deriving(Show)



{--------------------------------------------------------------------------------------------

 Ein LaTeX-Dokument besteht aus Fragmenten. Dies sind:
   Environments   Beginnen mit \begin{id} und enden mit \end{id}, oder unnamed: {..}
   Commands       Alles, was mit einem \ beginnt, ausser escaped Chars
   Escaped Chars  Sonderzeichen, bei denen die Befehlswirkung mit einem \ aufgehoben wurde
   Other          Textfragmente, die keiner der anderen Kategorien zugeordnet werden konnten

---------------------------------------------------------------------------------------------}

data Frag = Env EnvId Params [Frag]               -- Environments e.g. \begin{document}..
          | Command Command Params                 -- \name params
          | EscapedChar Char                       -- Sonderzeichen: #$&~_^%{} 
          | Other Other deriving Show

data Params = LParams [SingleParam] Attributes (Maybe Delimiter) (Maybe Delimiter) -- Parameter of LateX-Envs and Commands
              deriving Show
--MParams Attributes (Maybe Delimiter) (Maybe Delimiter)      -- Parameter of MMiSS-Envs




plainTextAtoms = [("Table","table"), ("Glossaryentry", "glossaryEntry"), ("Bibentry", "bibEntry")] ++
                 [("Figure", "figure"), ("ProgramFragment", "programFragment")] ++
                 [("Clause", "clause"), ("Step", "step"), ("Authorentry", "authorEntry")]

envsWithText = [("Section", "section"), ("Paragraph", "paragraph"), ("Abstract", "abstract")] ++
               [("Introduction", "introduction"),  ("Summary", "summary"), ("Program","program")] ++
	       [("Exercise","exercise"), ("Example","example"),  ("Definition","definition")] ++
               [("Theory","theory"), ("Theorem", "theorem"), ("Development","development")] ++
               [("Proof","proof"), ("Script","script"), ("List", "list"), ("ListItem", "listItem")] ++
               [("Conjecture", "conjecture"), ("Lemma", "lemma"), ("Corollary", "corollary")] ++
               [("Assertion", "assertion"), ("TextFragment","textFragment")]

envsWithoutText = [("Package", "package")]

includeCommands =  [("IncludeGroup", "includeGroup"), ("IncludeUnit", "includeUnit")] ++
                   [("IncludeAtom", "includeAtom"), ("IncludeTextFragment","includeTextFragment")] ++
		   [("IncludeSection", "includeSection"), ("IncludeAbstract","includeAbstract")] ++
                   [("IncludeIntroduction", "includeIntroduction"), ("IncludeSummary","includeSummary")] ++
                   [("IncludeFormalUnit", "includeFormalUnit"), ("IncludeAtom","includeAtom")] ++
                   [("IncludeConceptualUnit", "includeConceptualUnit")] ++
                   [("IncludeConceptualAtom", "includeConceptualAtom")] ++
                   [("IncludeProgramFragment","includeProgramFragment")] ++
                   [("IncludeProgram", "includeProgram"), ("IncludeClause","includeClause")] ++
                   [("IncludeTheory", "includeTheory"), ("IncludeStep","includeStep")] ++
                   [("IncludeProof", "includeProof"), ("IncludeScript","includeScript")] ++
                   [("IncludeDevelopment", "includeDevelopment")]

linkAndRefCommands = [("Link", "link"), ("ForwardLink","link"), ("Reference","reference")] ++
                     [("ForwardReference", "reference")]

embeddedElements = [("Emphasis","emphasis"), ("IncludeTextFragment","includeTextFragment")] ++
		   [("Link","link") , ("Define", "define"), ("Reference", "reference")] ++
                   [("ForwardLink","link"), ("ForwardReference", "reference")]

-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

mmiss2EnvIds = plainTextAtoms ++ envsWithText ++ envsWithoutText ++ linkAndRefCommands


-- LaTeX-Environments, deren Inhalt nicht geparst werden soll:
latexPlainTextEnvs = ["verbatim", "verbatim*"]


-- specialTreatmentInPreamble contains all Commands which are specially treated in the process
-- of generation a MMiSSLaTeX-Preamble out of the Fragments collected before the \begin{document}:

specialTreatmentInPreamble = ["documentclass", "usepackage", "Path", "Import"]


-- Die beiden folgenden Funktionen legen den String fest, der den Anfang und das Ende der
-- von MMiSS generierten Input-Preamble markiert. Zum Vergleich wird der gesamte String
-- verwendet:

startInputPragma = "%% MMiSSLaTeX input preamble"
endInputPragma = "%% End of MMiSSLaTeX input preamble"



---------------------------------------------------------------------------------------------
--
-- Hier beginnen die Parser
--
---------------------------------------------------------------------------------------------

backslash :: GenParser Char st Char
backslash = char '\\'

idParser :: GenParser Char st String
idParser = many (noneOf "}]")

-- commaSep :: GenParser Char st a
commaSep p = p `sepBy` (oneOf ",")

-- equalSep p = p `sepBy` (oneOf "=")

listTypeParser :: GenParser Char st String
listTypeParser = try(string "itemize") <|> try(string "enumerate") <|> return ("")


{- attParser parses the list of attributes belonging to an MMiSS-Environment -}

attParser :: GenParser Char st Attributes
attParser = commaSep attribute
            <|> return([])

attribute :: GenParser Char st (String, String)
attribute = do spaces
               key <- try(many1(noneOf " ,=}]")) <?> "attribute name"
               spaces
               char '=' <?> "value for attribute '" ++ key ++ "'"
               spaces
               v <- choice ((delimitedValue key):((value ",]"):[])) 
                    <?> "value for attribute '" ++ key ++ "'"
               spaces
               return (key, v)

delimitedValue :: String -> GenParser Char st String
delimitedValue key = between (char '{') (char '}') (value "")


{- The value parser accepts any String enclosed by {}. Furthermore it accepts
   Strings containing arbitrarily nested Substrings enclosed by {}. 
   It stops at characters, specified by 'rightClosure'. 
-}

value :: String -> GenParser Char st String
value rightClosure = 
  try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
         s2 <- try(between (char '{') (char '}') (try(value rightClosure)))
         s3 <- option "" (value rightClosure)
         return (s1 ++ "{" ++ s2 ++ "}" ++ s3))
  <|> try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
             s2 <- try(string "{}")
             s3 <- option "" (value rightClosure)
             return (s1 ++ "{}" ++ s3))
  <|> try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
             s2 <- char '\\'
             s3 <- anyChar
             s4 <- option "" (value rightClosure)
             return (s1 ++ [s2] ++ [s3] ++ s4))
  <|> try(do s1 <- try(many1 (noneOf ("{}\\" ++ rightClosure)))
             return s1)


-- other konsumiert solange Text, bis ein Backslash oder Kommentarzeichen auftaucht und
-- generiert ein Other-Fragment mit diesem Text.

other :: GenParser Char st Frag
other = fmap Other (many1 (noneOf "\\%[]{}"))


-- optionParser erkennt die Optionen fuer \documentclass[xx,yy,...]{classname}
-- und \usepackage[xx,yy,..]{packagename} Commands

optionParser :: GenParser Char st [String]
optionParser = commaSep singleOptParser

singleOptParser = many (noneOf "],")


-- **************************************************************************************
--
-- Hier 

-- begin erkennt den Namen einer Umgebung (id)
begin :: GenParser Char st String
begin = do  try (string "begin")
            spaces
            c <- between (char '{') (char '}') idParser
            return(c)
                            
-- end  ueberprueft, ob als naechstes ein \end{id} in der Source auftaucht.
end :: GenParser Char st String
end = do backslash
         string "end"
         spaces 
         c <- between (char '{') (char '}') idParser
         return(c)

endId :: String -> GenParser Char st String
endId id = do backslash
              str <- string ("end{" ++ id ++ "}")
              return(str)


-- continue wird vom Parser 'beginBlock' benutzt, um nach dem schliessenden Tag fuer
-- 'id' zu suchen. Der Inhalt der vom beginBlock erkannten Umgebung wird durch den
-- frag-Parser geschickt.

continue ::  [Frag] -> String -> GenParser Char st [Frag]
continue l id = (try (end) >>= 
                       (\ x -> if x == id then return l else fail ("no matching end-Tag for <" ++ id ++ ">")))
                <|> do f <- frag
                       continue (f:l) id

continuePlain ::  String -> String -> GenParser Char st [Frag]
continuePlain l id = (try (do endId id 
                              return ([(Other l)])))
                     <|> do bs <- backslash
                            continuePlain (l ++ [bs]) id
                     <|> do str <- plainText
                            continuePlain (l ++ str) id

plainText = many1 (noneOf "\\")


-- beginBlock erkennt den Start einer Umgebung (\begin{id}) und parst mittels
-- 'continue' den Inhalt der Umgebung. 'continue' achtet darauf, dass es zu der 
-- id aus dem begin-Block auch ein \end{id}-Block gibt.  
               
beginBlock :: GenParser Char st Frag
beginBlock = do id <- begin
                spaces
                p <- envParams id
--                beginDelim <-  option "" (try (many1 space))
--                l <- continue [] id
                l <- if (id `elem` ((map fst plainTextAtoms) ++ latexPlainTextEnvs))
                       then continuePlain "" id
                       else continue [] id
--                endDelim <-  option "" (try (many1 space))
--                params <- return (insertDelims p (Just(beginDelim)) (Just(endDelim)))
--                return (Env id params (reverse l))
                return (Env id p (reverse l))


-- envParams unterscheidet MMiSSLatex-Umgebungen von Latex-Umgebungen und stoesst
-- die passende Erkennungsfunktion fuer die Parameter an.

envParams :: String -> GenParser Char st Params
envParams id =  if (id `elem` (map fst mmiss2EnvIds)) 
		    then mEnvParams id 
                    else lParams id [] <|> unexpected ("Parameters for LaTeX-Environment <" ++ id ++ ">")


-- mListParams erkennt die Parameter, die zu einer MMiSSLatex-Umgebung gehoeren 
-- [Attributes]

mEnvParams :: String -> GenParser Char st Params

mEnvParams id = 
  do pos <- getPosition
     attributes <- option [] (try(between (char '[') (char ']') attParser))
--                                 <|> unexpected ("[attribute-list] for Environment <" ++ id ++ ">")
                   <?> (appendSourcePos pos "[attribute-list] for Environment <" ++ id ++ "> ")
     return(LParams [] attributes Nothing Nothing)


-- lParams erkennt Parameter eines Latex-Commands. Diese koennen in normalen, geschweiften
-- oder eckigen Klammern eingeschlossen sein. Es wird nicht geprueft, ob die Parameter
-- und ihre Reihenfolge den Latex-Definitionen entsprechen.
	    
lParams :: String -> [SingleParam] -> GenParser Char st Params
lParams id l
  | id == "Emphasis" = do spaces
                          str <- try (between (char '{') (char '}') (value ""))
                          p <- return [(Other str)]
                          return (LParams [(SingleParam p '{')] [] Nothing Nothing)

  | id == "Properties" =
      do pos <- getPosition
	 attributes <- try(between (char '[') (char ']') attParser)
		       <?> (appendSourcePos pos ("[attribute-list] for Command <" ++ id ++ ">")) 
         return (LParams [] attributes Nothing Nothing)

  | id `elem` (map fst includeCommands) =
      do pos <- getPosition
         labelId <-  try(between (char '{') (char '}') idParser)
	             <?> (appendSourcePos pos ("{referencedLabelID}{attribute-list} for Command <" ++ id ++ ">"))  
	 spaces
	 attributes <- try(between (char '{') (char '}') attParser)
		       <?> (appendSourcePos pos ("{attribute-list} for Command <" ++ id ++ ">")) 
         if (attributes == []) 
           then return (LParams [(SingleParam [(Other labelId)] '{')] [] Nothing Nothing)
           else return (LParams [(SingleParam [(Other labelId)] '{')] attributes Nothing Nothing)

  | id `elem` (map fst linkAndRefCommands) =
      do pos <- getPosition
	 attributes <- try(between (char '[') (char ']') attParser)
	               <?> (appendSourcePos pos ("[attribute-list] for Command <" ++ id ++ ">"))
         spaces
	 labelId <-  try(between (char '{') (char '}') idParser)
	             <?> (appendSourcePos pos ("[attribute list]{referencedLabelID} for Command <" ++ id ++ ">"))
	 spaces
         statusAtt <- if (isPrefixOf "Forward" id) 
			then return (("status", "absent"))
			else return (("status", "present"))
         attributes <- return (attributes ++ [statusAtt])
         return (LParams [(SingleParam [(Other labelId)] '{')] attributes Nothing Nothing)
 
 | id == "Define" = 
      do pos <- getPosition
         labelId <-  try(between (char '{') (char '}') idParser)
	             <?> (appendSourcePos pos ("{labelID}{definedName}{attribute-list} for Command <" ++ id ++ ">"))
         spaces
	 str <-  (try (between (char '{') (char '}') (value "")))
	         <?> (appendSourcePos pos ("{labelID}{definedName}{attribute-list} for Command <" ++ id ++ ">"))
         definedName <- return ([(Other str)])
   	 spaces
	 attributes <- try(between (char '{') (char '}') attParser)
		       <?> (appendSourcePos pos ("{attribute-list} for Command <" ++ id ++ ">"))   
         if (attributes == []) 
           then return (LParams [(SingleParam [(Other labelId)] '{'), (SingleParam definedName '{')] [] Nothing Nothing)
           else return (LParams [(SingleParam [(Other labelId)] '{'), (SingleParam definedName '{')] 
                          attributes Nothing Nothing)
 | id == "ListItem" = 
      do pos <- getPosition
         attributes <- option [] (try(between (char '[') (char ']') attParser))
		       <?> (appendSourcePos pos ("{attribute-list} for Command <" ++ id ++ "> "))
         if (attributes == []) 
           then return (LParams [] [] Nothing Nothing)
           else return (LParams [] attributes Nothing Nothing)

 | (id == "documentclass") || (id == "usepackage") =
      do pos <- getPosition
         options <- option [] (try(between (char '[') (char ']') optionParser))
                    <?> (appendSourcePos pos ("[options] for \\" ++ id ++ " "))
         spaces
         name <- try(between (char '{') (char '}') idParser)
                 <?> (appendSourcePos pos ("{name} for \\" ++ id ++ " "))
         versionDate <- option "" (try(between (char '[') (char ']') singleOptParser))
                        <?> (appendSourcePos pos ("[versiondate] for \\" ++ id ++ " "))
         attributes <- let optionAtts = [("option", opt) | opt <- options]
                           nameAtts = [("name", name)]
                           versionAtts = [("versiondate", versionDate)]
                       in return(optionAtts ++ nameAtts ++ versionAtts)
         return(LParams [] attributes Nothing Nothing)
{--
 | (id == "Import") = 
      do p <- try(between (char '{') (char '}') idParser)
                <?> ("Missing Argument for \\ImportPath.")
         return (LParams [(SingleParam [(Other p)] '{')] [] Nothing Nothing)
--} 
  | otherwise = do optionStr <- option "" (choice ((try (string "[]")):(try(between (char '[') (char ']') (value "]"))):[]))
                   options <- case optionStr of
                                "" -> return([])
                                "[]" -> return([])
                                otherwise -> return([(SingleParam [(Other optionStr)] '[')])
                   restParams <- genericParams []
                   return(LParams (options ++ restParams) [] Nothing Nothing)


genericParams :: [SingleParam] -> GenParser Char st [SingleParam]
genericParams l =
   do str <- try (try (between (char '{') (char '}') (value "")))
      p <- return [(Other str)]
      genericParams ((SingleParam p '{'):l)  
   <|>  do str <- try (try (between (char '(') (char ')') (value ")")))
           p <- return [(Other str)]
           genericParams ((SingleParam p '('):l)
   <|>  return (reverse l)

--               <|>  do str <- try (try (between (char '[') (char ']') (value "]")))
--                       p <- return [(Other str)]
--                       lParams id ((SingleParam p '['):l)


continueAdhocEnv closingChar l = 
  do try (char closingChar)
     return l
  <|> do f <- frag
         continueAdhocEnv closingChar (f:l) 
                         

-- adhocEnvironment erkennt Umgebungen ohne Namen: ...{text}...

adhocEnvironment =
   do char '{'
      l <- continueAdhocEnv '}' [] <?> "closing } for unnamed environment"
      return (Env "{}" (LParams [] [] Nothing Nothing) (reverse l))
   <|> do char '['
          l <- continueAdhocEnv ']' [] <?> "closing } for unnamed environment"
          return (Env "[]" (LParams [] [] Nothing Nothing) (reverse l))



-- command erkennt LaTeX-Kommandos. Escaped letters wie \{, \$ etc. werden hier 
-- nicht erkannt, sondern von escapedChar geparst.

command :: GenParser Char st Frag
command = do c <- try (many1 letter)
             star <- option "" (try (string "*"))
             l <- do braceDelim <- try (string "{}")
                     return (LParams [] [] (Just(braceDelim)) Nothing)
                 <|> do blankDelim <- try (many1 space)
                        return (LParams [] [] (Just(blankDelim)) Nothing)
                 <|> do p <- (lParams c [])
                        sp <- option "" (try (many1 space))
                        sp1 <- if (sp == "") then (return Nothing) else (return (Just sp))
                        return (insertDelims p sp1 Nothing)
             return (Command (c ++ star) l)


insertDelims :: Params -> (Maybe String) -> (Maybe String) -> Params
insertDelims (LParams sp attrs delim1 delim2) newDelim1 newDelim2 =
  let resDelim1 = case newDelim1 of
                     Nothing -> delim1
                     Just str -> if (str == "") then Nothing else (Just str)
      resDelim2 = case newDelim2 of
                    Nothing -> delim2
                    Just str -> if (str == "") then Nothing else (Just str)
  in LParams sp attrs resDelim1 resDelim2


-- escapedChar erkennt die mit Backslash ihrer Sonderbedeutung in LaTeX entbundenen
-- Zeichen sowie das geschuetzte Leerzeichen.

escapedChar :: GenParser Char st Frag
escapedChar = do c <- try (oneOf "\\#$&~_^%{} ")
                 return (EscapedChar c)


-- comment erkennt Kommentarzeilen. Kommentare werden als Other-Fragment behandelt
--

comment :: GenParser Char st Frag
comment = do char '%'
             s <- manyTill anyChar (try newline)
             return (Other ("%" ++ s ++ "\n"))


-- frag erkennt Latex-Fragmente: Kommentare, Environments, Commands und Escaped Chars. 
-- Alle anderen Zeichenfolgen werden in das Fragment 'other' verpackt.

frag :: GenParser Char st Frag
frag = comment
	 <|> do backslash
		beginBlock <|> escapedChar <|> command  <|> return (Other "\\")
         <|> adhocEnvironment
	 <|> other
		

-- latexDoc ist der Haupt-Parser. Er sammelt die Fragmente der Root-Ebene ein und kapselt
-- sie in eine virtuelle Root-Umgebung, damit eine saubere Baum-Struktur entsteht. 

latexDoc :: [Frag] -> GenParser Char st Frag
latexDoc l =  do f <-  frag <?> "Fragment"
		 latexDoc (f:l)
	      <|> return (Env "Root" (LParams [] [] Nothing Nothing) (reverse l))




-- **********************************************************************************************
--
-- Die nachfolgenden Parser werden werden zum Parsen der Path-Statements aus der Import-Präambel
-- benutzt
--
-- entityNameParser erkennt einen EntityName, der aus Buchstaben und anderen Zeichen bestehen
-- darf, ausser: ".\\{}"  Die {}-Klammern und der Backslash sind nicht erlaubt, weil sie 
-- in LaTeX zum Kennzeichnen von Token benutzt werden.

entityNameParser :: GenParser Char st String
entityNameParser = many1 (noneOf ".\\{},= \t\n")

identifierParser :: GenParser Char st String
identifierParser = try(do spaces 
                          str <- choice ((try(string "Root"))
                                         :(try(string "Current"))
                                         :(try(string "Parent"))
                                         :(try(entityNameParser)):[])
                          spaces
                          return(str))

entityFullNameParser :: [String] -> GenParser Char st EntityFullName
entityFullNameParser l = 
  do id <- try (identifierParser) 
     point <- option "" (string ".")
     if (point == ".") 
       then entityFullNameParser (l ++ [id])
       else return(EntityFullName (map EntityName (l ++ [id])))



-- ***********************************************************************************************
---
-- Die nachfolgenden Parser werden zum parsen der Import-Statements in der Präambel benutzt:
--
-- simpleDirectiveParser erkennt die einfachen Import-Direktiven: Global, Local, Qualified, Unqualified:
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
         nameList <- commaSep identifierParser
         char '}'
         spaces
         return([Hide (map EntityName nameList)]))
  <|> try(do spaces
             string "Reveal"
             char '{'
             nameList <- commaSep identifierParser
             char '}'
             spaces
             return([Reveal (map EntityName nameList)]))

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
                         firstName <- identifierParser
                         spaces
                         char '='
                         spaces
                         secondName <-identifierParser
                         spaces
                         return(Rename (EntityName firstName) (EntityName secondName)))

-- Parst eine einzelne Direktive:
directiveParser :: GenParser Char st [Directive]
directiveParser = simpleDirectiveParser 
                  <|> hideRevealDirectiveParser 

-- directivesParser parst die Direktiven eines Import-Statements
directivesParser :: GenParser Char st [Directive]
directivesParser = do listOfDirectiveLists <- commaSep (choice ((renameDirectiveParser):(directiveParser):[]))
                      return(foldl (++) [] listOfDirectiveLists)


{-- Main function: Parses the given MMiSSLatex-string and returns an Element which holds the
    XML-structure.  --}

parseMMiSSLatex :: String -> WithError (Element, Maybe MMiSSLatexPreamble)

parseMMiSSLatex s = 
  let result = parse (latexDoc []) "" s
  in case result of
--     Right ast  -> trace s (makeXML peamble ast)
       Right ast  -> makeXML ast
       Left err -> hasError (show err)


parseMMiSSLatexFile :: SourceName -> IO (WithError (Element, Maybe MMiSSLatexPreamble))
parseMMiSSLatexFile s = do result <- parseFromFile (latexDoc []) s
 		           case result of
			     Right ast  -> return(makeXML ast)
 		             Left err -> return(hasError (concat (map messageString (errorMessages(err)))))


{--
   parsePreamble is used as fromStringWE-method in the instanciation for
   MMiSSLatexPreamble as StringClass. 
--}

parsePreamble :: String -> WithError MMiSSLatexPreamble

parsePreamble s = 
  let result = parse (latexDoc []) "" s
  in
    case result of
      Right (Env _ _ fs)  -> 
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


{--
   parseImportCommands is used as fromStringWE-method in the instanciation for
   ImportCommands as StringClass. 


parseImportCommands :: String -> WithError ImportCommands

parseImportCommands s = 
  let result = parse (latexDoc []) "" s
  in case result of
       Right (Env _ _ fs)  -> 
          case (fromWithError (makeImportCmds fs [])) of
             Right pMaybe -> case pMaybe of
                               Just(p) -> hasValue(p)
                               Nothing -> hasError("Strange: makeImportCommands returns no error and no import commands.")
             Left err -> hasError(show err)
       Left err -> hasError (show err)
--}


parseAndShow :: SourceName -> IO ()
parseAndShow s = do result <- parseFromFile (latexDoc []) s
 		    case result of
	              Right ast  -> do resXML <- return (fromWithError (makeXML ast))
                                       case resXML of
                                         Right (e, mbPreamble) -> putStrLn (showElement (hasValue e))
                                         Left err -> print err
     	              Left err -> print err


showElement :: WithError Element -> String
showElement e = coerceWithError (mapWithError (render . PP.element) e)

showElement1 :: Content -> String
showElement1 (CElem e) = (render . PP.element) e


makeXML :: Frag -> WithError (Element, Maybe MMiSSLatexPreamble)
makeXML frag = findFirstEnv [frag] [] False

{-- findFirstEnv geht den vom Parser erzeugten abstrakten Syntaxbaum (AST) durch und erzeugt einen
    XML-Baum. Die Funktion sucht innerhalb des Environments 'Root' (vom Parser obligatorisch
    erzeugte Wurzel jedes AST) nach dem ersten MMiSSLatex-Environment oder dem ersten 'document'
    Env. Findet es zuerst das document-Env., wird die Suche nach einem MMiSSLatex-Env. in dessen
    Kindern fortgesetzt. Gleichzeitig wird das Aufsammeln von Praeambel-Fragmenten beendet.
    Findet es innerhalb des document-Envs ein package-Env, dann wird dieses als Wurzel des
    erzeugten XML-Baums eingesetzt und mittels 'makeContent' der Inhalt des packages in XML umgesetzt.
    Am Ende wird die Präambel erzeugt. Enthaelt die document-Umgebung nach dem Package-Env.
    noch MMiSSLatex- oder Latex-Fragmente, so werden diese ignoriert.
    Trifft die Funktion innerhalb des Root- oder document-Fragments auf eine andere MMiSSLatex-Umgebung
    als 'package', dann wird angenommen, dass es sich um ein Teildokument handelt, das ausgecheckt wurde.
    Der Inhalt dieses Elementes (das kann eine Unit sein, aber auch ein Atom - z.B. ein TextFragment)
    wird als Wurzel fuer den erzeugten XML-Baum eingesetzt und der Inhalt der Umgebung in XML umgewandelt.
    Hier wird jedoch keine Praeambel hinzugefuegt, weil es sich nur um ein Teildokument handelt,
    das mit Praeambel ausgecheckt wurde.
--}
    

findFirstEnv :: [Frag] -> [Frag] -> Bool -> WithError (Element, Maybe MMiSSLatexPreamble)

findFirstEnv ((Env "Root" _ fs):[]) preambleFs _  = findFirstEnv fs preambleFs True
findFirstEnv ((Env "document" _ fs):_) preambleFs _ = findFirstEnv fs preambleFs False
findFirstEnv ((Env "Package" ps@(LParams _ packAtts _ _) fs):_) preambleFs _ = 
  let (newPreambleFs, atts1) = addPropertiesFrag preambleFs packAtts
      latexPre = makePreamble (filterGeneratedPreambleParts newPreambleFs)
      importCmds = makeImportCmds newPreambleFs []
      atts2 = getPathAttrib preambleFs
      xmlAtts = map convertAttrib (atts1 ++ atts2)
      content = makeContent fs NoText "package"
  in case (fromWithError content) of
       Right c -> let elem = hasValue(Elem "package" xmlAtts c)
                      mmissPreamble = 
                              case fromWithError latexPre of
                                 Left str -> hasError(str)
                                 Right(lp) -> 
                                   case lp of
                                      Just(p) ->
                                        let impCmds = case fromWithError(importCmds) of
                                                         Right(v) -> v
                                                         Left str -> Nothing
                                        in  hasValue (Just(MMiSSLatexPreamble {
                                                             latexPreamble = p,
                                                             importCommands = impCmds
                                                           }))
                                      Nothing -> hasError("MMiSSLatexPreamble is empty!")
                  in pairWithError elem mmissPreamble 

       Left err -> let preEl = fromWithError(pairWithError latexPre importCmds)
                       mmissPreamble = case preEl of 
                                          Right _ -> hasValue(Nothing)
                                          Left str -> hasError(str)
                   in pairWithError (hasError(err)) mmissPreamble


findFirstEnv ((Env name ps fs):rest) preambleFs beforeDocument = 
  if (name `elem` (map fst (plainTextAtoms ++ envsWithText ++ envsWithoutText))) then
    let content = makeContent [(Env name ps fs)] (detectTextMode name) "Root"
        preambleDummy = Nothing
    in case (fromWithError content) of
         (Left str) -> hasError(str)
         (Right cs) -> 
            if ((genericLength cs) == 0) 
              then hasError("Internal Error: no XML content could be genereated for topmost Env. '" ++ name ++ "'")
              else let ce = head cs
                   in case ce of 
			(CElem e) -> hasValue(e, preambleDummy)
			_ -> hasError("Internal Error: no XML element could be genereated for topmost Env. '" ++ name ++ "'")
    else if (name `elem` (map fst mmiss2EnvIds)) 
           -- Env must be a link or Reference-Element: ignore it
           then findFirstEnv rest preambleFs beforeDocument
           -- Env ist plain LaTeX:
           else if (not beforeDocument)
	          -- Env is in document-Env but is not MMiSSLatex: pull out content and search there as well:
                  then findFirstEnv (fs ++ rest) preambleFs False
                  -- We are before document-Env. and it is no MMiSSLatex: add to preamble-Fragments
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
makePreRest (f@(Command name _):[]) inStr =
  if (name `elem` specialTreatmentInPreamble) 
     then inStr
     else inStr ++ (makeTextElem [f])

makePreRest (f1:f2:fs) inStr =
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
	      else makePreRest (f2:fs) (inStr ++ (makeTextElem [f1]))
     _ -> makePreRest (f2:fs) (inStr ++ (makeTextElem [f1]))


{-- addPropertiesFrag bekommt die Fragmente der Präambel sowie die Attribute, die am Package-Env.
definiert wurden übergeben und erzeugt daraus eine geänderte Liste von Präambel-Fragmenten.
Dazu wird das \Properties-Commando aus der Präambel herausgefiltert und dessen Attributwert mit denen
des Packages vereiningt. Diese neuen Attributwerte werden wiederum als \Properties-Fragment codiert
und in die zurückgegeben Fragement-Liste eingefügt. Im Prinzip werden also die Package-Attributwerte
in das \Properties-Fragment, das in der Präambel steht, hineingesetzt.
--}
addPropertiesFrag :: [Frag] -> Attributes -> ([Frag], Attributes)
addPropertiesFrag preambleFs packAtts =
  let propAtts = case (getProperties preambleFs) of 
                   Just((LParams _ atts _ _)) -> atts
                   Nothing -> []
      atts1 = unionAttributes packAtts propAtts
      newPropertiesFrag = (Command "Properties" (LParams [] atts1 Nothing Nothing))
  in ((filterProperties preambleFs) ++ [newPropertiesFrag], atts1)


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
      -> let aliasEl = parse entityNameParser "" aliasStr
             packageNameEl = parse (entityFullNameParser []) "" packageNameStr
         in case aliasEl of
               Right alias  -> 
                 case packageNameEl of 
                   Right packageName -> makeImportCmds fs (importCmds ++ [(PathAlias (EntityName alias) packageName)])
                   Left err -> hasError ("Parse error in second argument of Path-Command:\n"
                                     ++ (makeTextElem [cmd] ++ "\nError:\n")
                                     ++ show err)
               Left err -> hasError ("Parse error in first argument of Path-Command:\n"
                                   ++ (makeTextElem [cmd] ++ "\nError:\n")
                                   ++ show err)
    otherwise -> hasError("The following Path-Command in the Import-preamble has to few or wrong arguments:\n"                      ++ (makeTextElem [cmd]))

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
      -> let packageNameEl = parse (entityFullNameParser []) "" packageNameStr
             directivesEl = parse directivesParser "" directivesStr
         in case directivesEl of
               Right directives  -> 
                 case packageNameEl of 
                   Right packageName -> makeImportCmds fs (importCmds ++ [(Import directives packageName)])
                   Left err -> hasError ("Parse error in second argument of Import-Command:\n"
                                     ++ (makeTextElem [cmd] ++ "\nError:\n")
                                     ++ show err)
               Left err -> hasError ("Parse error in first argument of Import-Command:\n"
                                   ++ (makeTextElem [cmd] ++ "\nError:\n")
                                   ++ show err)
    otherwise -> hasError("The following Import-Command in the Import-preamble has to few or wrong arguments:\n"
--                          ++ (makeTextElem [cmd]))
                          ++ (show cmd))
    where
       genPackageName :: String -> WithError (Maybe ImportCommands)
       genPackageName packageNameStr = 
         let packageNameEl = parse (entityFullNameParser []) "" packageNameStr
         in case packageNameEl of 
              Right packageName -> makeImportCmds fs (importCmds ++ [(Import [] packageName)])
              Left err -> hasError ("Parse error in Package-name argument of Import-Command:\n"
                                     ++ (makeTextElem [cmd] ++ "\nError:\n")
                                     ++ show err)

makeImportCmds (f:fs) importCmds = makeImportCmds fs importCmds

makeImportCmds [] importCmds = if ((genericLength importCmds) == 0)
                                 then hasValue(Nothing)
                                 else hasValue(Just((ImportCommands importCmds)))


makeContent :: [Frag] -> Textmode -> String -> WithError [Content]

makeContent [] _ _ = hasValue([])
makeContent (f:frags) NoText parentEnv = 
   case f of
     (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                        in  mapWithError ([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))] ++)
                                         (makeContent frags NoText parentEnv)
     (Other str) -> if ((length (filter (not . (`elem` "\n ")) str) == 0) ||
			((head str) == '%'))
                      then mapWithError ([(CMisc (PI (piInsertLaTeX ,str)))] ++) (makeContent frags NoText parentEnv)
		      else hasError("No text allowed inside a " ++ parentEnv ++ "!")
		      -- TODO: Text, der nur aus Linefeeds besteht, muss erhalten bleiben, da er Einfluss
                      --       auf das von Latex erzeugte Layout haben kann.
     (Env name ps fs) -> 
       if (name `elem` (map fst plainTextAtoms))
         then hasError("No Environment '" ++ name ++ "' allowed inside a " ++ parentEnv ++ "!")
         else
           if (name == "TextFragment")
	     then hasError("No Environment 'TextFragment' allowed inside a " ++ parentEnv ++ "!")
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in myConcatWithError 
                             (cElemListWithError ename ps (makeAttribs ps name)
	                                         (makeContent fs (detectTextMode name) name))
                             (makeContent frags NoText parentEnv)
                 else  -- No MMiSS-Env.
                   let beginDelimStr = case ps of
                                         (LParams _ _ (Just delimStr) _) -> delimStr
                                         otherwise -> ""
                       endDelimStr = case ps of
                                       (LParams _ _ _ (Just delimStr)) -> delimStr
                                       otherwise -> ""
                       begin = case name of
                                 "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
                                 "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
                                 otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
                                                ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
                       end =  case name of 
                                 "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
                                 "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
                                 otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
	   	       body = (makeContent fs NoText parentEnv)
                       whole = myConcatWithError (myConcatWithError begin body) end
                   in myConcatWithError whole (makeContent frags NoText parentEnv)
--		   myConcatWithError (makeContent fs NoText parentEnv) (makeContent frags NoText parentEnv)
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
                   delimElem = case ps of
                                (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                                otherwise -> []
	       in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))] ++ delimElem))
                                    (makeContent frags NoText parentEnv)
	  else makeContent frags NoText parentEnv

makeContent (f:frags) TextAllowed "List" = 
   case f of
     (Other str) -> myConcatWithError (hasValue([(CMisc (Comment str))])) (makeContent frags TextAllowed "List")
     (Env name ps fs) -> 
        if (name `elem` (map fst mmiss2EnvIds))
          then hasError("Environment '" ++ name ++ "' is not allowed in lists. Wrap it up with a ListItem.")
          else  -- No MMiSS-Env.
            let beginDelimStr = case ps of
                                  (LParams _ _ (Just delimStr) _) -> delimStr
                                  otherwise -> ""
                endDelimStr = case ps of
                               (LParams _ _ _ (Just delimStr)) -> delimStr
                               otherwise -> ""
                begin = case name of
                          "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
                          "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
                          otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
                                         ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
                end =  case name of 
                          "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
                          "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
                          otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
	        body = (makeContent fs TextAllowed "List")
                whole = myConcatWithError (myConcatWithError begin body) end
            in myConcatWithError whole (makeContent frags TextAllowed "List")
--	    myConcatWithError (makeContent fs TextAllowed "List") (makeContent frags TextAllowed "List")
     (Command "ListItem" ps) -> 
         let (content, restFrags) = makeListItem ps frags []
         in myConcatWithError (hasValue([content])) (makeContent restFrags TextAllowed "List")
     _ -> makeContent frags TextAllowed "List"

makeContent (f:frags) TextAllowed parentEnv = 
   case f of
     (EscapedChar c) ->  let cstr = if (c == '\\') then "\\" else [c]
                         in myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))])) 
                                              (makeContent frags TextAllowed parentEnv)
     (Other str) -> if ((head str) == '%')
                      then myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX ,str)))])) 
                                             (makeContent frags TextAllowed parentEnv)
	              else if (genericLength (filter (not . isSpace) str) > 0)
                             then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
                                  in  myConcatWithError (hasValue([content])) 
				                        (makeContent restFrags TextAllowed parentEnv)
                             else myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX, str)))])) 
                                                    (makeContent frags TextAllowed parentEnv)

     (Env name ps fs) -> 
       if (name `elem` (map fst plainTextAtoms))
         then
           let ename = maybe "" snd (find ((name ==) . fst) plainTextAtoms)
           in  myConcatWithError
                  (cElemListWithError ename ps (makeAttribs ps name) (hasValue([CString True (makeTextElem fs)])))
                  (makeContent frags TextAllowed parentEnv)
         else
           if (name == "TextFragment")
	     then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                  in  myConcatWithError (hasValue([(makeTextFragment parentEnv ename (Just(ps)) fs [])])) 
                                        (makeContent frags TextAllowed parentEnv)
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in myConcatWithError (cElemListWithError ename ps (makeAttribs ps name)
	                                                       (makeContent fs (detectTextMode name) name))
                                           (makeContent frags TextAllowed parentEnv)
                 else  -- No MMiSS-Env.
                   let beginDelimStr = case ps of
                                         (LParams _ _ (Just delimStr) _) -> delimStr
                                         otherwise -> ""
                       endDelimStr = case ps of
                                         (LParams _ _ _ (Just delimStr)) -> delimStr
                                         otherwise -> ""
                       begin = case name of
                                 "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "[")))])
                                 "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "{")))])
                                 otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, 
                                               ("\\begin{" ++ name ++ "}" ++ (lparamsToString ps) ++ beginDelimStr))))])
                       end =  case name of 
                                "[]" -> hasValue([(CMisc (PI (piInsertLaTeX, "]")))])
                                "{}" -> hasValue([(CMisc (PI (piInsertLaTeX, "}")))])
                                otherwise -> hasValue([(CMisc (PI (piInsertLaTeX, ("\\end{" ++ name ++ "}" ++ endDelimStr))))])
	   	       body = (makeContent fs TextAllowed parentEnv)
                       whole = myConcatWithError (myConcatWithError begin body) end
                   in myConcatWithError whole (makeContent frags TextAllowed parentEnv)
--		   myConcatWithError (makeContent fs TextAllowed parentEnv) 
--                                     (makeContent frags TextAllowed parentEnv)
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
                   delimElem = case ps of
                                (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                                otherwise -> []
	       in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))]
                                              ++ delimElem))
                                    (makeContent frags TextAllowed parentEnv)
          else if (name `elem` (map fst embeddedElements))
                 then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
                      in  myConcatWithError (hasValue([content])) 
	   	                            (makeContent restFrags TextAllowed parentEnv)
                 else let delimStr = case ps of
                                (LParams _ _ (Just str) _) -> str
                                otherwise -> ""
                      in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
                                                                   ++ (lparamsToString ps) ++ delimStr))]))
                                            (makeContent frags TextAllowed parentEnv)
--                 else makeContent frags TextAllowed parentEnv


makeTextElem :: [Frag] -> String 

makeTextElem [] = ""
makeTextElem (f:fs) = 
  case f of
    (EscapedChar c) -> "\\" ++ (if (c == '\\') then "\\" else [c]) ++ (makeTextElem fs)
    (Other str) -> str ++ (makeTextElem fs)
    (Command name ps) -> let delimStr = case ps of
                                           (LParams _ _ (Just delimStr) _) -> delimStr
                                           otherwise -> "" 
                         in "\\" ++ name ++ (lparamsToString ps) ++ delimStr ++ (makeTextElem fs) 
    (Env name ps content) -> 
       let  beginDelimStr = case ps of
                              (LParams _ _ (Just delimStr) _) -> delimStr
                              otherwise -> ""
            endDelimStr = case ps of
                             (LParams _ _ _ (Just delimStr)) -> delimStr
                             otherwise -> ""
            begin = case name of
                      "[]" -> "["
                      "{}" -> "{"
                      otherwise -> "\\begin{" ++ name ++ "}" ++ (lparamsToString ps)
            end = case name of
                      "[]" -> "]"
                      "{}" -> "}"
                      otherwise -> "\\end{" ++ name ++ "}"
      in begin ++ beginDelimStr ++ (makeTextElem content) ++ end ++ endDelimStr ++ (makeTextElem fs) 


{-- lparamsToString formatiert Params (Command-Parameter) in die ursprüngliche Latex-Form --}
lparamsToString :: Params -> String
lparamsToString (LParams singleParams atts _ _) = 
  let pStr = (concat (map singleParamToString singleParams)) 
      attStr = (getAttribs (map convertAttrib atts) "" [])
      resultStr = if (attStr == "") 
                    then pStr
                    else pStr ++ "[" ++ attStr ++ "]"  
  in resultStr 

singleParamToString :: SingleParam -> String
singleParamToString (SingleParam f '{') = "{" ++ (makeTextElem f) ++ "}"
singleParamToString (SingleParam f '[') = "[" ++ (makeTextElem f) ++ "]" 
singleParamToString (SingleParam f '(') = "(" ++ (makeTextElem f) ++ ")"


makeNamelessTextFragment :: String -> [Frag] -> [Frag] -> (Content, [Frag])
makeNamelessTextFragment parentEnv [] textFrags = 
  ((makeTextFragment parentEnv "textFragment" Nothing textFrags []), [])
makeNamelessTextFragment parentEnv (f:frags) textFrags = 
  case f of
    (Env name _ fs) -> if (name `elem` (map fst embeddedElements))              
		         then makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
		         else 
                           if (name `elem` (map fst mmiss2EnvIds))
                             then let e1 = (makeTextFragment parentEnv "textFragment" Nothing textFrags [])  
				      c1 = case e1 of
					      (CElem (Elem "paragraph" _ ((CElem(Elem _ _ c)):[]))) -> c
					      (CElem (Elem _ _ c)) -> c
				  in if ((length c1) > 1) 
                                       then (e1, ([f] ++ frags))
				       else let c = head c1
					    in case c of
					       (CString _ str) -> 
						  if ((length (filter (not . (== '\n')) str) == 0) ||
			                              ((head str) == '%')) 
						    then ((CMisc (Comment str)), ([f] ++ frags))
                                                    else (e1, ([f] ++ frags))
                                               _ -> (e1, ([f] ++ frags))

--                             else makeNamelessTextFragment parentEnv (fs ++ frags) textFrags   -- Latex-Env.
                             else makeNamelessTextFragment parentEnv frags (textFrags ++ [f])  -- Latex-Env.
    (Command "ListItem" _ ) -> ((makeTextFragment parentEnv "textFragment" Nothing textFrags []), (f:frags))
    (Command "IncludeTextFragment" _) -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
    (Command name _) -> if (name `elem` (map fst includeCommands))
                             then let e1 = (makeTextFragment parentEnv "textFragment" Nothing textFrags [])  
				      c1 = case e1 of
					      (CElem (Elem "paragraph" _ ((CElem(Elem _ _ c)):[]))) -> c
					      (CElem (Elem _ _ c)) -> c
				  in if ((length c1) > 1) 
                                       then (e1, ([f] ++ frags))
				       else let c = head c1
					    in case c of
					       (CString _ str) -> 
						  if ((length (filter (not . (== '\n')) str) == 0) ||
			                              ((head str) == '%')) 
						    then ((CMisc (Comment str)), ([f] ++ frags))
                                                    else (e1, ([f] ++ frags))
                                               _ -> (e1, ([f] ++ frags))
                             else  makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
    _ -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])


makeTextFragment :: String -> String -> Maybe Params -> [Frag] -> [Content] -> Content

makeTextFragment parentEnv name params [] content = 
  let beginDelimElem = case params of
                         (Just (LParams _ _ (Just delimStr) _)) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                         otherwise -> []
      endDelimElem =   case params of
                         (Just (LParams _ _ _ (Just delimStr))) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                         otherwise -> []
      newContent = beginDelimElem ++ (concatTextElems(content)) ++ endDelimElem
  in if (parentEnv == "Section") 
       then (CElem (Elem "paragraph" [] 
                      [(CElem (Elem name (makeTextFragmentAttribs params) newContent))]))
       else (CElem (Elem name (makeTextFragmentAttribs params) newContent))

makeTextFragment parentEnv name params (f:frags) content =
  case f of
    (Other str) -> makeTextFragment parentEnv name params frags (content ++ [(CString True str)])
    (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                       in makeTextFragment parentEnv name params frags (content ++ [(CString True ("\\" ++ cstr))])
    (Command "Emphasis" ps) -> 
       let newElem = (CElem (Elem "emphasis" [] [(CString True (getEmphasisText ps))]))
           delimElem = case ps of
                          (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                          otherwise -> []
       in makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem) 
    (Command "IncludeTextFragment" ps) -> 
         let newElem = CElem (Elem "includeTextFragment" (makeIncludeAttribs ps) [])
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Link" ps) ->
         let newElem = CElem (Elem "link" (makeLinkAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "ForwardLink" ps) -> 
         let newElem = CElem (Elem "link" (makeLinkAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Reference" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "ForwardReference" ps) -> 
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Define" ps) ->
         let newElem = CElem (Elem "define" (makeDefineAttribs ps) (getDefineText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command cname ps) ->
         let delimStr = case ps of
                           (LParams _ _ (Just str) _) -> str
                           otherwise -> ""
             newElem = CMisc (PI (piInsertLaTeX, ("\\" ++ cname ++ (lparamsToString ps) ++ delimStr)))
 	 in  makeTextFragment parentEnv name params frags (content ++ [newElem])
    (Env ename ps fs) -> 
         let beginDelimStr = case ps of
                                (LParams _ _ (Just delimStr) _) -> delimStr
                                otherwise -> ""
             endDelimStr = case ps of
                              (LParams _ _ _ (Just delimStr)) -> delimStr
                              otherwise -> ""
             begin = case ename of
                       "[]" -> [CMisc (PI (piInsertLaTeX ,"[" ++ beginDelimStr))]
                       "{}" -> [CMisc (PI (piInsertLaTeX ,"{" ++ beginDelimStr))]
                       otherwise -> [CMisc (PI (piInsertLaTeX ,"\\begin{" ++ ename ++ "}" 
                                       ++ (lparamsToString ps) ++ beginDelimStr))]
             end = case ename of
                      "[]" -> [CMisc (PI (piInsertLaTeX ,"]" ++ endDelimStr))]
                      "{}" -> [CMisc (PI (piInsertLaTeX ,"}" ++ endDelimStr))]
                      otherwise -> [CMisc (PI (piInsertLaTeX ,"\\end{" ++ ename ++ "}" ++ endDelimStr))]
             (CElem (Elem _ _ c)) = makeTextFragment "TextFragment" name params fs []
         in makeTextFragment parentEnv name params frags (content ++ begin ++ c ++ end)


makeListItem :: Params -> [Frag] -> [Content] -> (Content, [Frag])

makeListItem params [] contentList = 
  let delimElem = case params of
                     (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                     otherwise -> []
  in ((CElem (Elem "listItem" (makeListItemAttribs params) (delimElem ++ contentList))), [])

makeListItem params (f:frags) contentList =
   case f of
     (EscapedChar c) -> let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
                        in makeListItem params restFrags (contentList ++ [content])
     (Other str) -> 
        if (str /= "")
	  then let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
	  else makeListItem params frags contentList
     (Env name ps fs) -> 
        if (name `elem` (map fst plainTextAtoms))
          then
            let ename = maybe "" snd (find ((name ==) . fst) plainTextAtoms)
            in  makeListItem params frags 
                             (contentList ++ [(CElem (Elem ename (makeAttribs ps name)
                                                          [CString True (makeTextElem fs)]))])
          else
            if (name == "TextFragment")
	      then makeListItem params frags 
                                (contentList ++ [(makeTextFragment "ListItem" "textFragment" (Just(ps)) fs [])])
              else if (name == "List")
                     then makeListItem params frags 
                                       (contentList ++ coerceWithError(makeContent [f] TextAllowed "ListItem"))
                     else if (not (name `elem` (map fst mmiss2EnvIds)))
                                 -- Latex-Env. Inhalt auf diese Ebene ziehen:
                            then let beginFrag = case name of
                                                    "{}" -> [(Other "{")]
                                                    "[]" -> [(Other "[")]
						    otherwise -> [(Other ("\\begin{" ++ name ++ "}"))]	    
                                     endFrag = case name of
                                                    "{}" -> [(Other "}")]
                                                    "[]" -> [(Other "]")]
						    otherwise -> [(Other ("\\end{" ++ name ++ "}"))]	    
                                 in makeListItem params (beginFrag ++ fs ++ endFrag ++ frags) contentList  
		                 -- MMiSSLatex-Env. Ignorieren:
                            else makeListItem params frags contentList
     (Command "IncludeTextFragment" ps) -> 
        let newElem = CElem (Elem "includeTextFragment" (makeIncludeAttribs ps) [])
            delimElem = case ps of
                          (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                          otherwise -> []
        in makeListItem params frags (contentList ++ [newElem] ++ delimElem)
     (Command "IncludeAtom" ps) ->
        let newElem = (CElem (Elem "includeAtom" (makeIncludeAttribs ps) []))
            delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
        in makeListItem params frags (contentList ++ [newElem] ++ delimElem)
     (Command "ListItem" ps) -> 
        let delimElem = case params of
                          (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                          otherwise -> []
        in ((CElem (Elem "listItem" (makeListItemAttribs params) (delimElem ++ contentList))), (f:frags))
     (Command _ _) -> let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
                      in makeListItem params restFrags (contentList ++ [content])

{--
     (Command name ps) -> 
        if (name `elem` (map fst embeddedElements))
          then let (content, restFrags) = makeNamelessTextFragment "ListItem" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
          else let newElem = CMisc (Comment ("\\" ++ name ++ (lparamsToString ps)))
               in makeListItem params frags (contentList ++ [newElem])
--}


concatTextElems :: [Content] -> [Content]

concatTextElems [] = []
concatTextElems ((CString True s1):((CString True s2):rest)) = concatTextElems ((CString True (s1 ++ s2)):rest)
concatTextElems ((CString True s1):((CElem e):rest)) = [(CString True s1), (CElem e)] ++ (concatTextElems rest)
concatTextElems ((CString True s1):((CMisc e):rest)) = [(CString True s1), (CMisc e)] ++ (concatTextElems rest)
concatTextElems ((CElem e):((CString True str):rest)) =
  [(CElem e)] ++ (concatTextElems ((CString True str):rest))
concatTextElems ((CElem e1):((CMisc e2):rest)) =
  [(CElem e1), (CMisc e2)] ++ (concatTextElems rest)
concatTextElems ((CMisc e1):e2:rest) = [(CMisc e1)] ++ (concatTextElems (e2:rest))
concatTextElems (e1:[]) = [e1]


-- detectTextMode ueberprueft anhand des uebergebenen Environment-Namens, ob darin laut MMiSS-Struktur
-- direkt Text enthalten sein darf. Es wird nicht ueberprueft, ob der Name ueberhaupt zu einem MMiSS-Env.
-- gehoert.
detectTextMode :: String -> Textmode
detectTextMode name = if (name `elem` (map fst (envsWithText ++ plainTextAtoms))) then TextAllowed
                       else NoText


-- getPathAttrib bekommt die in der Preamble aufgesammelten Fragmente und sucht darin
-- das \Import-Kommando, mit dem der Searchpath für importierte Elemente angebenen wird:

getPathAttrib :: [Frag] -> Attributes

getPathAttrib ((Command "Import" ps):fs) = 
  case ps of
    (LParams ((SingleParam ((Other str):[]) _):sps) _ _ _) -> [("path", str)]
    otherwise -> []
getPathAttrib (f:fs) = getPathAttrib fs
getPathAttrib [] = []


filterProperties :: [Frag] -> [Frag]
filterProperties ((Command "Properties" _):fs) = fs
filterProperties (f:fs) = [f] ++ (filterProperties fs)
filterProperties [] = []


getProperties :: [Frag] -> Maybe Params
getProperties ((Command "Properties" ps):fs) = Just(ps)
getProperties (f:fs) = getProperties fs
getProperties [] = Nothing


makeAttribs :: Params -> String -> [Attribute]

makeAttribs ps name = 
  if (name `elem` (map fst includeCommands))
    then makeIncludeAttribs ps
    else if (name == "TextFragment")
           then makeTextFragmentAttribs (Just(ps))
	   else if (name `elem` ["Link", "ForwardLink"])
	          then makeLinkAttribs ps
		  else if (name `elem` ["Reference", "ForwardReference"]) 
		         then makeRefAttribs ps
			 else if (name == "Define")
			        then makeDefineAttribs ps
				else case ps of
				       (LParams _ atts _ _) -> map convertAttrib atts
  

makeIncludeAttribs :: Params -> [Attribute]
makeIncludeAttribs (LParams ((SingleParam ((Other labelId):[]) _):[]) atts _ _) =
  [("included", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeIncludeAttribs _ = []


makeTextFragmentAttribs :: Maybe(Params) -> [Attribute]
makeTextFragmentAttribs (Just (LParams _ atts _ _)) = map convertAttrib atts
makeTextFragmentAttribs _ = []

makeListItemAttribs :: Params -> [Attribute]
makeListItemAttribs (LParams _ atts _ _) = (map convertAttrib atts)


makeLinkAttribs :: Params -> [Attribute]
makeLinkAttribs (LParams ((SingleParam ((Other labelId):[]) _):ps) atts _ _) =
  [("linked", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)
makeLinkAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)


makeRefAttribs :: Params -> [Attribute]
makeRefAttribs  (LParams ((SingleParam ((Other labelId):[]) _):ps) atts _ _) =
  [("referenced", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)
makeRefAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "LinkText")) . fst) atts)


makeDefineAttribs :: Params -> [Attribute]
makeDefineAttribs (LParams ((SingleParam ((Other labelId):[]) _):_) atts _ _) =
   [("label", (AttValue [Left labelId]))] ++ (map convertAttrib atts)
makeDefineAttribs _ = []

{--
unionAttributes :: [Attribute] -> [Attribute] -> [Attribute]
unionAttributes xs ys = unionBy (eqAttPair) xs ys
--}

unionAttributes :: Attributes -> Attributes -> Attributes
unionAttributes xs ys = unionBy (eqAttPair) xs ys

eqAttPair :: Eq a => (a, b) -> (a, b) -> Bool 
eqAttPair x y = (fst x) == (fst y)

-- getLinkText erwartet die Params eines Link oder Reference-Elementes und extrahiert daraus
-- den LinkText, der im ersten SingleParam steht und leer sein kann.

getLinkText :: Params -> [Content]
getLinkText (LParams _ atts _ _) =
  case (find ((== "LinkText") . fst) atts) of
    Just((_, linkText)) -> [(CString True linkText)]
    Nothing -> []

getDefineText :: Params -> [Content]
getDefineText  (LParams ps _ _ _) = 
  if (genericLength(ps) > 0) 
    then let (SingleParam fs _) = genericIndex ps 1
         in  [(CString True (makeTextElem fs))]
    else []

convertAttrib :: (String, String) -> Attribute
convertAttrib (l, r) = ((attNameToXML l), AttValue [Left r])


getEmphasisText :: Params -> String
getEmphasisText (LParams [] _ _ _) = ""
getEmphasisText (LParams ((SingleParam ((Other s):[]) _):ps) _ _ _) = s



{-- makeMMiSSLatex erzeugt aus einem XML-Element die zugehoerige MMiSSLatex-Repraesentation.
    Element ist das Root-Element des auszugebenden Dokumentbaumes, der Bool-Wert legt fest,
    ob eine Praeambel erzeugt werden soll (True) oder nicht (False).
--}

makeMMiSSLatex :: (Element, Bool, [MMiSSLatexPreamble]) -> WithError (EmacsContent ((String, Char), [Attribute]))

makeMMiSSLatex ((Elem name atts content), preOut, preambles) = 
  let items = fillLatex preOut [(CElem (Elem name atts content))] []
      p = unionPreambles preambles
      preambleItem = case p of
                       Nothing -> []
                       (Just(pre)) -> [(EditableText (makePreambleText pre))]
   in if preOut 
        then 
          let beginDocument = [EditableText "\\begin{document}\n"]
              endDocument = [EditableText "\n\\end{document}"]
           in hasValue((EmacsContent (preambleItem ++ beginDocument ++ items ++ endDocument)))
        else hasValue((EmacsContent items))


fillLatex :: Bool -> [Content] -> [EmacsDataItem ((String, Char), [Attribute])] 
               -> [EmacsDataItem ((String, Char), [Attribute])]

fillLatex out [] l = l

fillLatex out ((CElem (Elem "textFragment" atts contents)):cs) inList = 
  let s1 = "\\begin{TextFragment}" 
      s2 = "[" ++ (getAttribs atts "" []) ++ "]"
      s3 = "\\end{TextFragment}"
      items = if (s2 == "[]") 
                then (fillLatex out contents [])
                else [(EditableText (s1 ++ s2))] ++ (fillLatex out contents []) ++ [(EditableText s3)]
  in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem "listItem" atts contents)):cs) inList = 
   let s1 = "\\ListItem" 
       attrStr = (getAttribs atts "" [])
       s2 = if (attrStr == "") then " " else "[" ++ attrStr ++ "] "
       items = [EditableText (s1 ++ s2)] ++ (fillLatex out contents [])
   in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem "emphasis" _ ((CString _ str):_))):cs) inList = 
   fillLatex out cs (inList ++ [EditableText ("\\Emphasis{" ++ str ++ "}")]) 

fillLatex out ((CElem (Elem ('i':'n':'c':'l':'u':'d':'e':unit) atts _)):cs) inList = 
   if (out == True) 
     then
       let label = "{" ++ (getParam "included" atts) ++ "}"
           name = (elemNameToLaTeX ("include" ++ unit))
           s1 = "{" ++ (getAttribs atts "" ["included"]) ++ "}"
           items = [(EditableText ("\\" ++ name ++ label ++ s1))]
       in fillLatex out cs (inList ++ items)
     else
       let labelId = getParam "included" atts
           item = [EmacsLink ((labelId, (fromIncludeStr unit)), atts)]
       in fillLatex out cs (inList ++ item)

fillLatex out ((CElem (Elem name atts contents)):cs) inList
  | (name `elem` (map snd linkAndRefCommands)) =  
    let forwardStr = if ((getParam "status" atts) == "absent")
                       then "Forward"
                       else ""
        s1 = "\\" ++ forwardStr ++ (elemNameToLaTeX name) 
        linkText = if (length(contents) == 0) then "" 
                     else let c = head contents
                          in case c of
                              (CString _ body) -> "LinkText={" ++ body ++ "}"
                              _ -> ""
        s2 = "[" ++ (getAttribs atts "" ["linked", "referenced", "status"]) ++
                     if (linkText == "") then "]" else linkText ++ "]"
        s3 = "{" ++  if (name == "link") then (getParam "linked" atts) ++ "}" else  (getParam "referenced" atts) ++ "}"
        items = [(EditableText (s1 ++ s2 ++ s3))]
    in fillLatex out cs (inList ++ items)
  | (name == "define") =
    let s1 = "\\" ++ (elemNameToLaTeX name)
        s2 = "{" ++ (getParam "label" atts) ++ "}"
        str = if (length(contents) == 0) then "" 
                 else let c = head contents
                      in case c of
                           (CString _ body) -> body
                           _ -> ""
        s3 = "{" ++ str ++ "}"
        s4 = "{" ++ (getAttribs atts "" ["label"]) ++ "}"
        items = [(EditableText (s1 ++ s2 ++ s3 ++ s4))]
    in fillLatex out cs (inList ++ items)
        
fillLatex out ((CString _ str):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (Comment str)):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (PI ("mmiss:InsertLaTeX", str))):cs) inList =  fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CElem (Elem name atts contents)):cs) inList = 
  let s1 = "\\begin{" ++ (elemNameToLaTeX name) ++ "}" 
      attrStr = if (name == "package") 
                  then (getAttribs atts "" ["path"])
                  else (getAttribs atts "" [])
      s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "]"
      s3 = "\\end{" ++ (elemNameToLaTeX name) ++ "}"
      items = [(EditableText (s1 ++ s2))] ++ (fillLatex out contents []) 
              ++ [(EditableText s3)]
  in fillLatex out cs (inList ++ items)

fillLatex out (c:cs) inList = fillLatex out cs inList


{-
data Package = Package Options PackageName Versiondate
type DocumentClass = Package

data MMiSSLatexPreamble = Preamble DocumentClass [Package] String
-}

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
  in str1 ++ str2

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
makeImportCmdText (Import ds (EntityFullName entityNames)) =
  let renStr = collectRenames ds ""
      newDs = filter (not.isRename) ds
      dirStr = if ((genericLength newDs) > 0)
                 then drop 2 (concat (map makeDirectiveText newDs))
                 else ""
      tmpStr = if ((genericLength renStr) > 1) && ((genericLength dirStr) > 1)
                 then ", "
                 else ""
      packageName = let namesList = [ str | (EntityName str) <- entityNames]
                    in if (namesList == [])
                         then ""
                         else init (concat (map (++ ".") namesList))
      directivesStr = if (dirStr ++ tmpStr ++ renStr) == "" 
                        then ""
                        else "[" ++ (dirStr ++ tmpStr ++ renStr) ++ "]"
  in "\\Import" ++ directivesStr ++ "{" ++ packageName ++ "}"    

makeImportCmdText (PathAlias (EntityName alias) (EntityFullName entityNames)) =
  let namesList = [ str | (EntityName str) <- entityNames]
      packageName = if (namesList == [])
                      then ""
                      else init (concat (map (++ ".") namesList))
  in "\\Path{" ++ alias ++ "}{" ++ packageName ++ "}"


makeDirectiveText :: Directive -> String
makeDirectiveText Qualified = ", Qualified"
makeDirectiveText Unqualified = ", Unqualified"
makeDirectiveText Global = ", Global"
makeDirectiveText Local = ", Local"
makeDirectiveText (Hide names) = 
  let l = [ n | (EntityName n) <- names]
      str = if l == [] then "" else init (concat (map (++ ",") l))
  in ", Hide{" ++ str ++ "}"
makeDirectiveText  (Reveal names) =  
  let l = [ n | (EntityName n) <- names]
      str = if (l == []) then "" else init (concat (map (++ ",") l))
  in ", Reveal{" ++ str ++ "}"

collectRenames :: [Directive] -> String -> String
collectRenames ((Rename (EntityName newName) (EntityName oldName)):ds) str = 
  collectRenames ds (str ++ ", " ++ newName ++ "=" ++ oldName)
collectRenames (d:ds) str = collectRenames ds str 
collectRenames [] str = 
  if ((genericLength str) > 0) 
    then let str1 = drop 2 str
         in "Rename{" ++ str1 ++ "}"
    else str


isRename :: Directive -> Bool
isRename (Rename _ _) = True
isRename _ = False
             

getParam :: String -> [Attribute] -> String
getParam name atts = let value = lookup name atts
                     in case value of
                          Just(AttValue [(Left str)]) -> str
                          Nothing -> ""


getAttribs :: [Attribute] -> String -> [String] -> String
getAttribs [] str _ = if ((take 1 str) == ",") 
                       then (drop 1 str)
                       else str  
getAttribs ((name, (AttValue [(Left value)])):as) str excludeList = 
   if (name `elem` excludeList)
     then getAttribs as str excludeList
     else getAttribs as (str ++ "," ++ attNameToLatex(name) ++ "={" ++ value ++ "}") excludeList                


attNameToLatex :: String -> String
attNameToLatex "xml:lang" = "Language"
attNameToLatex name = [(toUpper (head name))] ++ (tail name)


attNameToXML :: String -> String
attNameToXML "Language" = "xml:lang"
attNameToXML name = [(toLower (head name))] ++ (tail name)


elemNameToLaTeX :: String -> String
elemNameToLaTeX name = maybe "" fst (find ((name ==) . snd) 
                                          (mmiss2EnvIds ++ embeddedElements ++ includeCommands))


myConcatWithError :: WithError [a] -> WithError [a] -> WithError [a]

myConcatWithError l m = mapWithError (uncurry (++)) (pairWithError l m)


cElemListWithError:: String -> Params -> [Attribute] -> WithError [Content] -> WithError [Content]
cElemListWithError name ps atts c =
  case fromWithError c of
    Right content -> 
      let beginDelimElem = case ps of
                             (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                             otherwise -> []
          endDelimElem =   case ps of
                             (LParams _ _ _ (Just delimStr)) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                             otherwise -> []
      in hasValue([(CElem (Elem name atts (beginDelimElem ++ content)))] ++ endDelimElem)
    Left str -> hasError str
  

-- Maps an Xml tag to its corresponding mini-type if it has one.
-- (The mini-type is just something that identifies what sort of
-- include the String needs.)
classifyLabelledTag :: String -> Maybe Char
classifyLabelledTag str = fromIncludeStrOpt (mapLabelledTag str)

-- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
-- the corresponding includeXXX command.
toIncludeStr :: Char -> String
toIncludeStr 'G' = "Group"
toIncludeStr 'U' = "Unit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "TextFragment"
toIncludeStr 'S' = "Section"
toIncludeStr 'a' = "Abstract"
toIncludeStr 'I' = "Introduction"
toIncludeStr 's' = "Summary"
toIncludeStr 'F' = "FormalUnit"
toIncludeStr 'C' = "ConceptualAtom"
toIncludeStr 'p' = "ProgramFragment"
toIncludeStr 'P' = "Program"
toIncludeStr 'c' = "Clause"
toIncludeStr 't' = "Theory"
toIncludeStr 'x' = "Step"
toIncludeStr 'y' = "Proof"
toIncludeStr 'z' = "Script"
toIncludeStr 'D' = "Development"
toIncludeStr 'E' = "ConceptualUnit"
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"


---
-- fromIncludeStrOpt
-- and also handles the case where the first letter is lower-cased.
fromIncludeStrOpt :: String -> Maybe Char
fromIncludeStrOpt "Group" = Just 'G'
fromIncludeStrOpt "Unit" = Just 'U'
fromIncludeStrOpt "Atom" = Just 'A'
fromIncludeStrOpt "TextFragment" = Just 'T'
fromIncludeStrOpt "Section"         = Just 'S'                
fromIncludeStrOpt "Abstract"        = Just 'a'        
fromIncludeStrOpt "Introduction"    = Just 'I'        
fromIncludeStrOpt "Summary"         = Just 's'        
fromIncludeStrOpt "FormalUnit"      = Just 'F'        
fromIncludeStrOpt "ConceptualAtom"  = Just 'C'        
fromIncludeStrOpt "ProgramFragment" = Just 'p'         
fromIncludeStrOpt "Program"         = Just 'P'        
fromIncludeStrOpt "Clause"          = Just 'c'        
fromIncludeStrOpt "Theory"          = Just 't'        
fromIncludeStrOpt "Step"            = Just 'x'        
fromIncludeStrOpt "Proof"           = Just 'y'        
fromIncludeStrOpt "Script"          = Just 'z'        
fromIncludeStrOpt "Development"     = Just 'D' 
fromIncludeStrOpt "ConceptualUnit"  = Just 'E' 
fromIncludeStrOpt (c : cs) | Char.isLower c 
   = fromIncludeStrOpt (toUpper c : cs)
fromIncludeStrOpt _ = Nothing


fromIncludeStr :: String -> Char
fromIncludeStr str = case fromIncludeStrOpt str of
   Just c -> c
   Nothing -> error 
    ("MMiSSDTDAssumptions.fromIncludeStr - bad include string"++str)

---
-- Map tags to the name of their corresponding include element (minus
-- "include")
mapLabelledTag :: String -> String
mapLabelledTag s = 
   case s of
      "package" -> "Group"
      "section" -> "Section"
      "paragraph" -> "Group"
      "view" -> "Group"
      "example" -> "ConceptualUnit"
      "exercise" -> "ConceptualUnit"
      "definition" -> "ConceptualUnit"
      "theorem" -> "FormalUnit"
      "conjecture" -> "FormalUnit"
      "lemma" -> "FormalUnit"
      "corollary" -> "FormalUnit"
      "assertion" -> "FormalUnit"
      "list" -> "ConceptualAtom"
      "table" -> "ConceptualAtom"
      "figure" -> "ConceptualAtom"
      "glossaryEntry" -> "ConceptualAtom"
      "bibEntry" -> "ConceptualAtom"
      _ -> mapUpper s
   where
      mapUpper [] = []
      mapUpper (c : cs) = toUpper c : cs      


piInsertLaTeX = "mmiss:InsertLaTeX"


parseAndMakeMMiSSLatex :: SourceName -> Bool -> IO ()
parseAndMakeMMiSSLatex name _ = 
  do result <- parseMMiSSLatexFile name
     case (fromWithError result) of
       Left err -> print err
       Right (e, mbPreamble) -> 
         case (fromWithError (makeMMiSSLatex (e, True, []))) of
           Left err -> print err
           Right (EmacsContent l) ->  putStrLn (concat (map getStrOfEmacsDataItem l))


getStrOfEmacsDataItem :: EmacsDataItem ((String, Char), [Attribute]) -> String

getStrOfEmacsDataItem (EditableText str) = str
getStrOfEmacsDataItem (EmacsLink ((str,c), _)) = str ++ [c]                                   


append :: a -> [a] -> [a]
append x xs = xs ++ [x]


appendSourcePos :: SourcePos -> String -> String
appendSourcePos pos str = str ++ "in Line " 
                          ++ (show (sourceLine pos)) ++ " Column " 
                          ++ (show (sourceColumn pos)) ++ "."


---------------------------------------------------------------------------------------------
--
-- MMiSSLatexPreamble is an instance of StringClass
--
---------------------------------------------------------------------------------------------

instance StringClass MMiSSLatexPreamble where
   fromStringWE string = parsePreamble string
   toString preamble = makePreambleText preamble

--instance StringClass ImportCommands where
--   fromStringWE string = parseImportCommands string
--   toString importCmds = makeImportsText importCmds


-- ----------------------------------------------------------------------------------
-- Instances of Typeable & HasCodedValue for Preamble and MMiSSLatexPreamble 
-- (added by George)
-- ----------------------------------------------------------------------------------

package_tyRep = Dynamics.mkTyRep "LaTeXParser" "Package"
instance Dynamics.HasTyRep Package where
   tyRep _ = package_tyRep

instance Monad m => CodedValue.HasBinary Package m where
   writeBin = mapWrite 
      (\ (Package options packageName versionData) -> (options,packageName,versionData))
   readBin = mapRead
      (\ (options,packageName,versionData) -> Package options packageName versionData)

preamble_tyRep = Dynamics.mkTyRep "LaTeXParser" "MMiSSLatexPreamble"
instance Dynamics.HasTyRep MMiSSLatexPreamble where
   tyRep _ = preamble_tyRep

instance Monad m => CodedValue.HasBinary MMiSSLatexPreamble m where
   writeBin = mapWrite
      (\ (MMiSSLatexPreamble latexPreamble importCommands) ->
         (latexPreamble,importCommands))
   readBin = mapRead
      (\ (latexPreamble,importCommands) ->
         (MMiSSLatexPreamble latexPreamble importCommands))

latexPreamble_tyRep = Dynamics.mkTyRep "LaTeXParser" "LaTeXPreamble"
instance Dynamics.HasTyRep LaTeXPreamble where
   tyRep _ = latexPreamble_tyRep

instance Monad m => CodedValue.HasBinary LaTeXPreamble m where
   writeBin = mapWrite
      (\ (Preamble documentClass packages string) -> 
         (documentClass,packages,string))
   readBin = mapRead
      (\ (documentClass,packages,string) ->
         (Preamble documentClass packages string))


mmissOntology_tyRep = Dynamics.mkTyRep "LaTeXParser" "MMiSSOntology"
instance HasTyRep MMiSSOntology where
   tyRep _ = mmissOntology_tyRep

instance Monad m => CodedValue.HasBinary MMiSSOntology m where
   writeBin = mapWrite (
      \ (MMiSSOntology classes objects relations objectLinks) ->
      (classes,objects,relations,objectLinks)
      )
   readBin = mapRead (
      \ (classes,objects,relations,objectLinks) ->
      (MMiSSOntology classes objects relations objectLinks)
      )

classDecl_tyRep = Dynamics.mkTyRep "LaTeXParser" "ClassDecl"
instance HasTyRep ClassDecl where
   tyRep _ = classDecl_tyRep

instance Monad m => HasBinary ClassDecl m where
   writeBin = mapWrite (
      \ (ClassDecl className classText super) ->
      (className,classText,super)
      )
   readBin = mapRead (
      \ (className,classText,super) ->
      (ClassDecl className classText super)
      )

objectDecl_tyRep = Dynamics.mkTyRep "LaTeXParser" "ObjectDecl"
instance HasTyRep ObjectDecl where
   tyRep _ = objectDecl_tyRep

instance Monad m => HasBinary ObjectDecl m where
   writeBin = mapWrite (
      \ (ObjectDecl objName objectText instanceOf) ->
      (objName,objectText,instanceOf)
      )
   readBin = mapRead (
      \ (objName,objectText,instanceOf) ->
      (ObjectDecl objName objectText instanceOf)
      )

relationDecl_tyRep = Dynamics.mkTyRep "LaTeXParser" "RelationDecl"
instance HasTyRep RelationDecl where
   tyRep _ = relationDecl_tyRep

instance Monad m => HasBinary RelationDecl m where
   writeBin = mapWrite (
      \ (RelationDecl multiplicities relName relationText source target) ->
      (multiplicities,relName,relationText,source,target)
      )
   readBin = mapRead (
      \ (multiplicities,relName,relationText,source,target) ->
      (RelationDecl multiplicities relName relationText source target)
      )

objectLink_tyRep = Dynamics.mkTyRep "LaTeXParser" "ObjectLink"
instance HasTyRep ObjectLink where
   tyRep _ = objectLink_tyRep

instance Monad m => HasBinary ObjectLink m where
   writeBin = mapWrite (
      \ (ObjectLink sourceObj targetObj linkRelation) ->
      (sourceObj,targetObj,linkRelation)
      )
   readBin = mapRead (
      \ (sourceObj,targetObj,linkRelation) ->
      (ObjectLink sourceObj targetObj linkRelation)
      )

--}

-- Rubbish: To be deleted

{--
flattenTupel :: WithError((a,b),c) -> WithError (a, b, c)

flattenTupel t = case fromWithError t of
                   Left(str) -> hasError(str)
                   Right((a,b),c) -> hasValue((a,b,c)) 
--}
