module LaTeXParser (
   -- new interface.  For now, just a make-weight bolted on top of the old one.
   FileSystem(..),
   PackageId(..),

   parseMMiSSLatex, 
      -- :: FileSystem -> FilePath 
      -- -> IO (WithError ((Element,PackageId),
      --    [(MMiSSLatexPreamble,PackageId)]))
   makeMMiSSLatexContent,
      -- :: Element -> Bool 
      -- -> (MMiSSLatexPreamble,PackageId,[MMiSSExtraPreambleData])]
      -- -> WithError (EmacsContent ((String,Char),[Attribute])) 
      -- needed for Emacs
   writeMMiSSLatex, 
      -- :: FileSystem -> Element -> Bool
      -- -> [(MMiSSLatexPreamble,PackageId,[MMiSSExtraPreambleData])]
      -- -> IO (WithError ()) -- needed for export
   mergePreambles, -- :: [MMiSSLatexPreamble] -> (MMiSSLatexPreamble,[String]) 


   mkLaTeXString, 
      -- :: EmacsContent ((String,Char),[Attribute]) -> String
      -- Convert the contents of an Emacs buffer representing a particular
      -- LaTeX file into a String.

   -- OLD INTERFACE
   frags,

   parseMMiSSLatex1, 
   -- new :: String -> WithError (Element, Maybe MMiSSLatexPreamble)
   -- Turn MMiSSLaTeX into an Element.   
   parseMMiSSLatex1File, 
   -- new :: String -> WithError (Element, Maybe MMiSSLatexPreamble)
   -- The same, for a file.
   makeMMiSSLatex1,
   -- :: (Element, Bool, [(MMiSSLatexPreamble,[MMiSSExtraPreambleData])]) 
   --   -> WithError (EmacsContent ((String, Char), [Attribute]))
   -- Turns an Element into a MMiSSLaTeX source
   -- If the Bool is set, attaches a preamble.
   -- MMiSSExtraPreambleData contains extra information about the call-site
   -- of the element containing a preamble.

   MMiSSExtraPreambleData(..),
   -- 
   importCommands,
   -- :: MMiSSLatexPreamble -> Maybe ImportCommands

   classifyLabelledTag, -- :: String -> Maybe Char
   -- Maps an Xml tag to its corresponding mini-type if it has one.
   -- (The mini-type is just something that identifies what sort of
   -- include the String needs.)

   fromIncludeStr, --  String -> Char
   fromIncludeStrOpt, -- String -> Maybe Char
   toIncludeStr, -- Char -> String
   -- toIncludeStr and fromIncludeStr convert the mini-type to and from XXX in
   -- the corresponding includeXXX command.
   mapLabelledTag,
   MMiSSLatexPreamble, 
   parsePreamble,
   emptyMMiSSLatexPreamble,
   Frag(..),
   Params(..)
   )
 where

-- module LaTeXParser where

import IO(FilePath)

import IOExts
import List
import Parsec
import Char

import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.Pretty as PP
import Text.PrettyPrint.HughesPJ hiding (char,space)
import Text.XML.HaXml.Combinators hiding (find)

import Dynamics
import Computation hiding (try)
import ExtendedPrelude(unsplitByChar)
import ParsecError
import EmacsContent
import EntityNames
import AtomString
import CodedValue
-- import EmacsEdit(TypedName)


-- ---------------------------------------------------------------------------
-- New Interface
-- ---------------------------------------------------------------------------

-- types
newtype PackageId = PackageId {packageIdStr :: String} deriving (Eq,Ord)

data FileSystem = FileSystem {
   readString :: FilePath -> IO (WithError String),
   writeString :: String -> FilePath -> IO (WithError ())
   } 

-- Functions
parseMMiSSLatex :: FileSystem -> FilePath 
   -> IO (WithError ((Element,PackageId),[(MMiSSLatexPreamble,PackageId)]))
parseMMiSSLatex fileSystem filePath =
   do
      strWE <- readString fileSystem filePath 
      case fromWithError strWE of
         Left err -> return (fail err)
         Right str ->
            do
               let
                  parsedWE = parseMMiSSLatex1 str
               case fromWithError parsedWE of
                  Left err -> return (fail err)
                  Right (el @ (Elem _ atts _),preambleOpt) ->
                     do
                        let
                           packageId = PackageId (getParam "packageId" atts)

                           preambleList = case preambleOpt of
                              Nothing -> []
                              Just preamble -> [(preamble,packageId)]
                        return (hasValue ((el,packageId),preambleList))
                         
makeMMiSSLatexContent :: Element -> Bool ->
   [(MMiSSLatexPreamble,PackageId,[MMiSSExtraPreambleData])]
   -> WithError (EmacsContent ((String,Char),[Attribute]))
makeMMiSSLatexContent el b preambleInfos0 =
   let
      preambleInfos1 = map
         (\ (preamble,packageId,datas) -> (preamble,datas))
         preambleInfos0
   in
      makeMMiSSLatex1 (el,b,preambleInfos1)


writeMMiSSLatex :: FileSystem -> Element -> Bool ->
   [(MMiSSLatexPreamble,PackageId,[MMiSSExtraPreambleData])]
   -> IO (WithError ())
writeMMiSSLatex fileSystem (el @ (Elem _ atts _)) b preambleInfos0 =
   do
      let
         contentWE = makeMMiSSLatexContent el b preambleInfos0
      case fromWithError contentWE of
         Left err -> return (fail err)
         Right content ->
            do
               let
                  result = mkLaTeXString content
                  label = getParam "label" atts
               writeString fileSystem result label  
 

mkLaTeXString :: EmacsContent ((String,Char),[Attribute]) -> String
mkLaTeXString (EmacsContent dataItems) =
   concatMap
      (\ dataItem -> case dataItem of
         EditableText str -> str
         EmacsLink ((included,ch),attributes) -> 
            "\\Include"
            ++ toIncludeStr ch
            ++ "{" ++ included ++ "}"
            ++ toLaTeXAttributes (attributes ++ [statusAttribute])
         )     
      dataItems

toLaTeXAttributes :: [Attribute] -> String
toLaTeXAttributes [] = ""
toLaTeXAttributes attributes =
   "{"
   ++ unsplitByChar ',' (map
      (\ (name,attValue) ->
         case attValue of
            AttValue [Left value] -> name ++ "=" ++ value
         )
      attributes   
      )
   ++
   "}"

statusAttribute :: Attribute
statusAttribute = ("status",AttValue [Left "present"])

mergePreambles :: [MMiSSLatexPreamble] -> (MMiSSLatexPreamble,[String])
mergePreambles [preamble] = (preamble,[])
mergePreambles (preamble:_) = (preamble,[
   "All preambles but the first thrown away; complain to Achim!!!"])
mergePreambles [] = error 
   "mergePreambles given no preambles; complain to George!!!"

-- ---------------------------------------------------------------------------
-- Old Interface
-- ---------------------------------------------------------------------------


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

-- SingleParam nimmt die Daten für einen einfachen (also keine Attributlisten-Parameter)
-- Paramter eines Latex-Kommandos oder einer Umgebung auf. Die Komponente 'Char'
-- nimmt das konkrete Klammerzeichen, mit dem der Parameter links abgegrenzt wird, auf.
-- (Davon ausgehend ist klar, wie das korrespondierende rechte Klammerzeichen aussehen muss: 
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
  superClass :: Maybe String
} deriving(Show)

data ObjectDecl = ObjectDecl {
  objectName :: String,
  text :: String,
  instanceOf :: String
} deriving(Show)

data RelationDecl = RelationDecl {
  multiplicities :: Maybe String,
  relName :: String,
  relationText :: String,
  source:: String,
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

-- Ein Fragment ist ein vom Parser erkanntes Konstrukt. Der abstrakte Syntaxbaum eines
-- Latex-Dokumentes besteht aus diesen Fragmenten:

data Frag = Env EnvId Params [Frag]               -- Environments e.g. \begin{document}..
          | Command Command Params                 -- \name params
          | EscapedChar Char                       -- Sonderzeichen: #$&~_^%{} 
          | Other Other deriving Show

-- Parameter of LateX-Envs and Commands. Der erste der beiden Maybe Delimiter-Komponenten
-- wird benutzt, um bei Commands den String aufzunehmen, der das Command vom nachfolgenden
-- Text trennt, also entweder ein {} oder Leerzeichen (inclusive Tabs und Newlines).
-- Die rechte Delimiter-Komponente 

data Params = LParams [SingleParam] Attributes (Maybe Delimiter) (Maybe Delimiter) 
              deriving Show



-- piInsertLaTeX enthält den String, der zur Kennzeichnung der Processing-Instructions dienst,
-- die in den resultierenden XML-Baum eingefügt werden, um Latex-Anteile zu bewahren, die nicht
-- zur Struktur und nicht zu den Textbestandteilen gehören.

piInsertLaTeX = "mmissInsertLaTeX"

-- The search/replace strings listed in latexToUnicodeTranslations are applied to attribute values when
-- they are stored in XML-attribute instances:

latexToUnicodeTranslations = [("\\\"a", "\x00e4"), ("\\\"u", "\x00fc"), ("\\\"o", "\x00f6")]
                          ++ [("\\\"A", "\x00c4"), ("\\\"U", "\x00dc"), ("\\\"O", "\x00d6")] 
                          ++ [("\\ss{}", "\x00df"), ("\\ss", "\x00df")]

{--
latexToUnicodeTranslations = [("\\\"a", "ä"), ("\\\"u", "ü"), ("\\\"o", "ö")]
                          ++ [("\\\"A", "Ä"), ("\\\"U", "Ü"), ("\\\"O", "Ö")] 
                          ++ [("\\ss{}", "ß"), ("\\ss", "ß")]
--}

unicodeToLatexTranslations = [("ä", "\\\"a"), ("ü", "\\\"u"), ("ö","\\\"o")]
                          ++ [("Ä", "\\\"A"), ("Ü", "\\\"U"), ("Ö", "\\\"O")] 
                          ++ [("ß", "\\ss{}")]
                          ++ [("\x00e4", "\\\"a"), ("\x00fc", "\\\"u"), ("\x00f6","\\\"o")]
                          ++ [("\x00c4", "\\\"A"), ("\x00dc", "\\\"U"), ("\x00d6", "\\\"O")]
                          ++ [("\x00df","\\ss{}")] 

plainTextAtoms = [("Table","table"), ("Glossaryentry", "glossaryEntry"), ("Bibentry", "bibEntry")] ++
                 [("Figure", "figure"), ("ProgramFragment", "programFragment")] ++
                 [("Authorentry", "authorEntry"), ("Rule", "rule"), ("ProofStep", "proofStep")] ++
                 [("DevelopmentStep","developmentStep"), ("Source", "source"), ("Declaration","declaration")] ++
                 [ ("Axiom","axiom")] ++ latexAtomFormulaEnvs


envsWithText = [("Section", "section"), ("Paragraph", "paragraph"), ("Abstract", "abstract")] ++
               [("Introduction", "introduction"),  ("Summary", "summary"), ("Program","program")] ++
	       [("Exercise","exercise"), ("Example","example"),  ("Definition","definition")] ++
               [("Theory","theory"), ("Theorem", "theorem"), ("Development","development")] ++
               [("Proof","proof"), ("Script","script"), ("item", "item"), ("ListItem", "item")] ++
               [("Conjecture", "conjecture"), ("Lemma", "lemma"), ("Corollary", "corollary")] ++
               [("Assertion", "assertion"), ("Text","text"), ("Assignment", "assignment")] ++
               [("Solution", "solution"),("Itemize", "itemize"), ("Enumerate","enumerate")] ++
               [("Description", "description"), ("Proposition", "proposition"), ("FalseConjecture", "falseConjecture")] ++
               [("Item","item"), ("Comment","comment"),("Note","note"),("Warning","warning")] ++
               [("Error","error"), ("Glossary", "glossary")] ++ listEnvs

envsWithoutText = [("Package", "package")]

includeCommands =  [("IncludeUnit", "includeUnit"), ("IncludeSection", "includeSection")] ++
                   [("IncludeAtom", "includeAtom"), ("IncludeText","includeText")] ++
                   [("IncludeProgramComponent","includeProgramComponent")] ++
                   [("IncludeCompositeUnit", "includeCompositeUnit"), ("IncludeTerm","includeTerm")] ++
                   [("IncludeProofStep", "includeProofStep"), ("IncludeProof", "includeProof")] ++
                   [("IncludeDevelopmentStep", "includeDevelopmentStep"), ("IncludeTable","includeTable")] ++
                   [("IncludeFigure","includeFigure")]

linkCommands = [("Link", "link")]
refCommands = [("Reference","reference"), ("Ref","reference")]
linkAndRefCommands = linkCommands ++ refCommands


embeddedElements = [("Emphasis","emphasis"), ("IncludeText","includeText")] ++
		   [("Link","link") , ("Def", "define"), ("Reference", "reference")] ++
                   [("Ref", "reference"), ("Cite", "cite")]


listEnvs = [("Itemize", "itemize"), ("Description", "description"), ("Enumerate", "enumerate")]

itemNames = ["ListItem", "item", "Item"]


-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

mmiss2EnvIds = plainTextAtoms ++ envsWithText ++ envsWithoutText ++ linkAndRefCommands


-- LaTeX-Environments, deren Inhalt nicht geparst werden soll:
latexPlainTextEnvs = ["verbatim", "verbatim*", "code", "xcode", "scode", "math", "displaymath", "equation"] ++
                     ["alltt"]


-- LaTeX-Environments for formulas are translated to the XML-Element 'formula' which has an attribute 'boundsType'
-- which takes an symbol that indicates the concrete LaTeX-Environment the user chose for a particular formula item.
-- The following list matches the various LaTeX formula enviroments to theses symbolic names recorded in the
-- boundsType attribute: 

latexEmbeddedFormulaEnvs = [("math", "math"), ("$", "shortMathDollar"), ("$$", "shortDisplaymathDollar")] ++
                   [("\\(", "shortMathParens")]

latexAtomFormulaEnvs =  [("\\[", "shortDisplaymath"), ("equation", "equation"), ("displaymath", "displaymath")] 


-- specialTreatmentInPreamble contains all Commands which are specially treated in the process
-- of generation a MMiSSLaTeX-Preamble out of the Fragments collected before the \begin{document}:

specialTreatmentInPreamble = ["documentclass", "usepackage", "Path", "Import"]


-- glAttribsExclude is a list with attribute names which are omitted in the process of generating MMiSSLaTeX from
-- XML.

glAttribsExclude = ["files"]


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


{- attributesOrNot checks if a '[' is the next input character. If so, it expects an
   attribute list (found on MMiSS-Environments) which it returns. If the next character 
   is something else, than it succeeds but returns an empty list. In the latter case 
   it succeeds because it is valid to have no attribute list at all at a MMiSS-Environment.
 -}

attributesOrNot :: GenParser Char st Attributes
attributesOrNot =  (do pos <- getPosition
                       try(char '[')
                       result <- (try attParser)
                                 <?> (appendSourcePos pos "[attribute-list] for MMiSS environment ")
                       try(char ']')
                       return result
                   )
                   <|> return []


{- attParser parses the list of attributes belonging to an MMiSS-Environment -}

attParser :: GenParser Char st Attributes
attParser = commaSep attribute
            <|> return([])

attribute :: GenParser Char st (String, String)
attribute = do spaces
               key <- try(many1(noneOf " ,=}]\n\t\f\r\v")) <?> "attribute name"
               spaces
               char '=' <?> "value for attribute '" ++ key ++ "'"
               spaces
               v <- choice ((try(string "{}")):((parenthesed False '{' '}'):((oldvalue ",]"):[]))) 
                    <?> "value for attribute '" ++ key ++ "'"
               spaces
               pos <- getPosition
               case key of
                 "Label" -> let elEntityName = parse entityNameParser1 "" v
                            in case elEntityName of
                                 Left err -> fail (appendSourcePos pos ("Label '" ++ v ++ "' contains illegal characters ")) 
                                 Right _ -> done
                 _ -> done
               new_v <- if (v == "{}") then return "" else return v
               return (key, new_v)

delimitedValue :: String -> GenParser Char st String
delimitedValue key = try(between (char '{') (char '}') (oldvalue key))


{- The value parser accepts any String enclosed by {}. Furthermore it accepts
   Strings containing arbitrarily nested Substrings enclosed by {}. 
   It stops at characters, specified by 'rightClosure'. 
-}


oldvalue :: String -> GenParser Char st String
oldvalue rightClosure = 
  try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
         s2 <- try(between (char '{') (char '}') (try(oldvalue rightClosure)))
         s3 <- option "" (oldvalue rightClosure)
         return (s1 ++ "{" ++ s2 ++ "}" ++ s3))
  <|> try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
             s2 <- try(string "{}")
             s3 <- option "" (oldvalue rightClosure)
             return (s1 ++ "{}" ++ s3))
  <|> try(do s1 <- try(many (noneOf ("{}\\" ++ rightClosure)))
             s2 <- char '\\'
             s3 <- anyChar
             s4 <- option "" (oldvalue rightClosure)
             return (s1 ++ [s2] ++ [s3] ++ s4))
  <|> try(do s1 <- try(many1 (noneOf ("{}\\" ++ rightClosure)))
             return s1)


anyWithoutThisParens :: String -> String -> GenParser Char st String
anyWithoutThisParens parSymbols inStr = 
  do s <- try (escapedBracket)
     anyWithoutThisParens parSymbols (inStr ++ s) 
  <|> do char '\\'
         anyWithoutThisParens parSymbols (inStr ++ "\\")
  <|> do s <- many1 (noneOf ("\\" ++ parSymbols))
         anyWithoutThisParens parSymbols (inStr ++ s)
  <|> return inStr


parenthesed :: Bool -> Char -> Char -> GenParser Char st String
parenthesed printParens opening closing = 
   do char opening
      s1 <- anyWithoutThisParens parSymbols ""
      l <- many ( do str <- parenthesed True opening closing 
		     str2 <- (anyWithoutThisParens parSymbols "")
		     return (str ++ str2))
      s2 <- anyWithoutThisParens parSymbols ""
      char closing
      p1 <- if printParens then return [opening] else return ""
      p2 <- if printParens then return [closing] else return ""
      return (p1 ++ s1 ++ (concat l) ++ s2 ++ p2)
   where
     parSymbols = [opening] ++ [closing]


escapedBracket :: GenParser Char st String
escapedBracket = do try (char '\\')
                    c <- try (oneOf "([{}])")
                    return ("\\" ++ [c]) 


-- other konsumiert solange Text, bis ein Backslash oder Kommentarzeichen auftaucht und
-- generiert ein Other-Fragment mit diesem Text.

other :: GenParser Char st Frag
other = fmap Other (many1 (noneOf "\\%[]{}$"))


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
                     <|> do str <- plainText "\\"
                            continuePlain (l ++ str) id


mySpaces ::  String -> GenParser Char st String
mySpaces str = do s <- space
                  mySpaces (str ++ [s])
               <|> return(str)

-- beginBlock erkennt den Start einer Umgebung (\begin{id}) und parst mittels
-- 'continue' den Inhalt der Umgebung. 'continue' achtet darauf, dass es zu der 
-- id aus dem begin-Block auch ein \end{id}-Block gibt.  
               
beginBlock :: GenParser Char st Frag
beginBlock = do id <- begin
                spaceStr <- option "" (try(mySpaces ""))
                frags <- if (spaceStr == "") then return([]) else return([(Other spaceStr)])
--                spaces
                p <- envParams id
                l <- if (id `elem` latexPlainTextEnvs)
                       then continuePlain "" id
                       else continue [] id
                return (Env id p (frags ++ (reverse l)))


-- envParams unterscheidet MMiSSLatex-Umgebungen von Latex-Umgebungen und stoesst
-- die passende Erkennungsfunktion fuer die Parameter an.

envParams :: String -> GenParser Char st Params
envParams id =  if (id `elem` (map fst mmiss2EnvIds)) 
		    then mEnvParams id 
                    else lParams id [] 
                         <|> unexpected ("Parameters for LaTeX-Environment <" ++ id ++ ">")


-- mListParams erkennt die Parameter, die zu einer MMiSSLatex-Umgebung gehoeren 
-- [Attributes]

mEnvParams :: String -> GenParser Char st Params

mEnvParams id = 
  do pos <- getPosition
     attributes <- attributesOrNot
     return(LParams [] attributes Nothing Nothing)


-- lParams erkennt Parameter eines Latex-Commands. Diese koennen in normalen, geschweiften
-- oder eckigen Klammern eingeschlossen sein. Es wird nicht geprueft, ob die Parameter
-- und ihre Reihenfolge den Latex-Definitionen entsprechen.
	    
lParams :: String -> [SingleParam] -> GenParser Char st Params
lParams id l
  | id == "Emphasis" = do spaces
                          str <- try (parenthesed False '{' '}')
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

  | id `elem` (map fst refCommands) =
      do pos <- getPosition
	 phrase <- option [] (parenthesed False '[' ']')
         spaces
	 labelId <-  try(between (char '{') (char '}') idParser)
	             <?> (appendSourcePos pos ("[phrase]{referenced LabelID} for Command <" ++ id ++ ">"))
         linkTextAttrs <- if (phrase == []) then return([]) else return([("LinkText", phrase)])
         return (LParams [(SingleParam [(Other labelId)] '{')] 
                         ([("status","absent")] ++ linkTextAttrs) Nothing Nothing)

  | id `elem` (map fst linkCommands) =
      do pos <- getPosition
	 phrase <- option [] (try(parenthesed False '[' ']'))
         spaces
	 param1 <- try(between (char '{') (char '}') idParser)
	           <?> (appendSourcePos pos ("[phrase]{type}{label} or [phrase]{label} for Command <" ++ id ++ ">"))
         spaces
	 param2 <- try(do id <- between (char '{') (char '}') idParser
                          return (Just id)
                      )
                   <|> return Nothing
--	            <?> (appendSourcePos pos ("[phrase]{element type}{referenced LabelID} for Command <" ++ id ++ ">"))
         linkTextAttrs <- if (phrase == []) then return([]) else return([("LinkText", phrase)])
         (elType, labelId) <- case param2 of
                                (Just id) -> return (param1, id)
                                _ -> return ("", param1) 
         return (LParams [(SingleParam [(Other labelId)] '{')] 
                         ([("status","absent"), ("Type", elType)] ++ linkTextAttrs) Nothing Nothing)
 
 | (id == "Def") = 
      do pos <- getPosition
	 phrase <- do try (string "[]")
                      return ("")
                   <|> (try(parenthesed False '[' ']'))
                   <|> return ""
         spaces
         labelId <- try(between (char '{') (char '}') idParser)
	            <?> (appendSourcePos pos ("[phrase]{defined LabelID} for Command <" ++ id ++ ">"))
         optTextAttrs <- if (phrase == "") then return([]) else return([("OptText", phrase)])
         return (LParams [(SingleParam [(Other labelId)] '{')] 
                         ([("status","absent")] ++ optTextAttrs) Nothing Nothing)

 | (id `elem` itemNames) =
      do pos <- getPosition
         descItem <- option [] (try(parenthesed False '[' ']'))
         attributes <- if (descItem == [])
                         then return []
                         else return ([("descItem", descItem)])
         return (LParams [] attributes Nothing Nothing)

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

  | otherwise = do optionStr <- option "" (choice ((try (string "[]")):(try(parenthesed False '[' ']')):[]))
                   options <- case optionStr of
                                "" -> return([])
                                "[]" -> return([])
                                otherwise -> return([(SingleParam [(Other optionStr)] '[')])
                   restParams <- genericParams []
                   return(LParams (options ++ restParams) [] Nothing Nothing)



genericParams :: [SingleParam] -> GenParser Char st [SingleParam]
genericParams l =
   do str <- try (try (parenthesed False '{' '}'))
      p <- return [(Other str)]
      genericParams ((SingleParam p '{'):l)  
   <|>  do str <- try (try (parenthesed False '(' ')'))
           p <- return [(Other str)]
           genericParams ((SingleParam p '('):l)
   <|>  return (reverse l)


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


mathEnv :: GenParser Char st Frag
mathEnv = try( do c <- oneOf "([" 
                  rightDelim <- if (c == '(') then (return ")") else return("]")
                  fs <- continuePlainFormula "" ("\\" ++ rightDelim) "\\"
                  return (Env ("\\" ++ [c]) (LParams [] [] Nothing Nothing) fs))

simpleMathEnv :: GenParser Char st Frag
simpleMathEnv = 
  try (do string "$$"
          fs <- continuePlainFormula "" "$$" "$"
          return (Env ("$$") (LParams [] [] Nothing Nothing) fs)
  )
  <|>
    try( do char '$'
            str <- plainText "$"
            char '$'
            return (Env ("$") (LParams [] [] Nothing Nothing) [(Other str)])
    )


continuePlainFormula ::  String -> String -> [Char] -> GenParser Char st [Frag]
-- 1. String : The accumulation of characters, parsed so far
-- 2. String : The delimiter to stop at
-- 3. String : The list of characters which could be the start of the delimiter or the delimiter itself

continuePlainFormula l delimiter stopChars = 
  (try (do string delimiter
           return ([(Other l)])))
  <|> do sc <- oneOf stopChars
         continuePlainFormula (l ++ [sc]) delimiter stopChars
  <|> do str <- plainText stopChars
         continuePlainFormula (l ++ str) delimiter stopChars


plainText stopChars = many1 (noneOf stopChars)


-- frag erkennt Latex-Fragmente: Kommentare, Environments, Commands und Escaped Chars. 
-- Alle anderen Zeichenfolgen werden in das Fragment 'other' verpackt.

frag :: GenParser Char st Frag
frag = comment
	 <|> do backslash
		mathEnv <|> beginBlock <|> escapedChar <|> command  <|> return (Other "\\")
         <|> adhocEnvironment
         <|> simpleMathEnv
	 <|> other
		

-- frags ist der Haupt-Parser. Er sammelt die Fragmente der Root-Ebene ein.

frags :: [Frag] -> GenParser Char st [Frag]
frags l =  do f <-  frag <?> "Fragment"
              frags (f:l)
	   <|> return(reverse l)



-- **********************************************************************************************
--
-- (EntityName, EntityFullName, EntitySearchName parsers now in EntityNames.hs, and the
-- old code that used to be here has been deleted.  Note that we are now much stricter,
-- allowing only letters and digits in names; however we can extend that later if necessary.)
-- (GER, 18/9/03)
--
-- Instead we include versions of the parsers which check that all the input is read.
-- **********************************************************************************************

entityNameParser1 :: GenParser Char st EntityName
entityNameParser1 = isAll entityNameParser


entityFullNameParser1 :: GenParser Char st EntityFullName
entityFullNameParser1 = isAll entityFullNameParser


entitySearchNameParser1 :: GenParser Char st EntitySearchName
entitySearchNameParser1 = isAll entitySearchNameParser

isAll :: GenParser Char st a -> GenParser Char st a
isAll parser0 =
   do
      a <- parser0
      eof
      return a

-- ***********************************************************************************************
--
-- Die nachfolgenden Parser werden zum parsen der Import-Statements in der Präambel benutzt:
--
-- simpleDirectiveParser erkennt die einfachen Import-Direktiven: Global, Local, Qualified, Unqualified:
-- ***********************************************************************************************

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


{-- Main function: Parses the given MMiSSLatex-string and returns an Element which holds the
    XML-structure.  --}

parseMMiSSLatex1 :: String -> WithError (Element, Maybe MMiSSLatexPreamble)

parseMMiSSLatex1 s = 
  let result = parse (frags []) "" s
  in case result of
--     Right ast  -> trace s (makeXML peamble ast)
       Right fs  -> makeXML (Env "Root" (LParams [] [] Nothing Nothing) fs)
       Left err -> hasError (show err)


parseMMiSSLatex1File :: SourceName -> IO (WithError (Element, Maybe MMiSSLatexPreamble))
parseMMiSSLatex1File s = do result <- parseFromFile (frags []) s
 		            case result of
			       Right fs  -> return(makeXML (Env "Root" (LParams [] [] Nothing Nothing) fs))
 		               Left err -> return(hasError (concat (map messageString (errorMessages(err)))))


{--
   parsePreamble is used as fromStringWE-method in the instanciation for
   MMiSSLatexPreamble as StringClass. 
--}

parsePreamble :: String -> WithError MMiSSLatexPreamble
parsePreamble s = 
  let result = parse (frags []) "" s
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

emptyMMiSSLatexPreamble :: MMiSSLatexPreamble
emptyMMiSSLatexPreamble = 
  MMiSSLatexPreamble (Preamble (Package [] "mmiss" "today") [] "") Nothing

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

{--
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
--}


makeXML :: Frag -> WithError (Element, Maybe MMiSSLatexPreamble)
makeXML frag = 
  case fromWithError (findFirstEnv [frag] [] False) of
    Left err -> hasError(err)
    Right (e, preamble) ->
      let clist = (foldXml insertFileAttributes) (CElem e)
      in case (last clist) of
           (CElem e) -> hasValue(e, preamble)
           otherwise -> hasError("Internal Error in function 'makeXML': XML filter delivered a non-element content.")


{----------------------------------------------------------------------------------------------------------------

  The functions in this section are used for extracting the information, which external files are addressed by the
  latex file from which the XML tree was built. The repository expects Attributes named 'files' at the elements
  containing references to external files (via the latex commands \includegraphics or \externalFile, the
  latter one is mmisslatex.
  This extraction is done by filtering the XML tree with help of the HaXml-Combinator library.
  
--}


{-- getFileCommand examines a XML content portion. If it is a processing instruction, containing a latex command
    \includegraphics or \externalFile, then it gives back the referenced filename, otherwise it
      returns nothing.
--}

getFileCommand :: Content -> [String]

getFileCommand c = 
  case c of 
    (CString True text) -> getFileCmdInternal text
    (CMisc (PI (target, str))) -> if (target == piInsertLaTeX)
                                    then getFileCmdInternal str
                                    else []
    _ -> []
  where 
     getFileCmdInternal str =
       let elFrag = parse (frags []) "" str
       in case elFrag of
            Right fs -> (foldl extractFilenames [] fs)       
            Left err -> []

     extractFilenames l c@(Command name (LParams singlePs _ _ _)) =
       if ((isPrefixOf "includegraphics" name) 
            || (isSuffixOf "includegraphics" name)
            || (name == "includeExternalFile")
            && ((length singlePs) > 0))
         then let param = singleParamToString(last singlePs)
                  fileStr = genericTake ((length param) - 2) (genericDrop 1 param)
              in (l ++ [fileStr])
         else let fs = concat (map getFragsFromSingleParam singlePs)
                  subList = foldl extractFilenames [] fs
              in (l ++ subList)   
     extractFilenames l f = l
     getFragsFromSingleParam (SingleParam fs _) = concat (map parseString fs)


parseString :: Frag -> [Frag]

parseString (Other str) =
  let result = parse (frags []) "" str
  in case result of
       Right fs  -> fs
       Left err -> []

parseString _ = []


{-- insertFileAttributes is a cfilter which collects the filenames from the _direct_ children of
    the given Content element and adds a 'files'-Attribute with a list of filenames referenced.
--}

insertFileAttributes :: CFilter

insertFileAttributes c@(CElem(Elem name attribs clist)) =
  let rlist = concat (map getFileCommand clist) 
      filesStr = concatFilenames rlist
      fileAttr = if (filesStr == "") 
                   then []
                   else [("files", AttValue [(Left filesStr)])]
  in [(CElem(Elem name (attribs ++ fileAttr) clist))]

insertFileAttributes c = [c]
 

concatFilenames :: [String] -> String

concatFilenames [] = ""
concatFilenames (filename:[]) = filename
concatFilenames (filename:rest) = filename ++ "," ++ concatFilenames rest


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
findFirstEnv ((Env "Package" ps@(LParams _ packAtts _ _) fs):_) preambleFs beforeDocument = 
  if beforeDocument 
    then
      let (newPreambleFs, atts1) = addPropertiesFrag preambleFs packAtts
          xmlAtts = map convertAttrib atts1
          content = makeContent fs NoText "package"
      in case (fromWithError content) of
            Right c -> hasValue((Elem "package" xmlAtts c), Nothing)
            Left err -> hasError(err)
    else
      let (newPreambleFs, atts1) = addPropertiesFrag preambleFs packAtts
          latexPre = makePreamble (filterGeneratedPreambleParts newPreambleFs)
          importCmds = makeImportCmds newPreambleFs []
          xmlAtts = map convertAttrib atts1
          content = makeContent fs NoText "package"
      in case (fromWithError content) of
            Right c -> pairWithError elem mmissPreamble 
                       where 
                         elem = hasValue(Elem "package" xmlAtts c)
                         mmissPreamble = 
                           case fromWithError latexPre of
                             Left str -> hasError(str)
                             Right(lp) -> case lp of 
                                            Just(p) -> wE_MMiSSLatexPreamble p
                                            Nothing -> hasError("MMiSSLatexPreamble is empty!")
                         wE_MMiSSLatexPreamble p = 
                           case fromWithError(importCmds) of
                             Right(impCmds) -> hasValue (Just(MMiSSLatexPreamble {
                                                             latexPreamble = p,
                                                             importCommands = impCmds
                                                         }))
                             Left str -> hasError(str)
            Left err -> let preEl = fromWithError(pairWithError latexPre importCmds)
                            mmissPreamble = case preEl of 
                                              Right _ -> hasValue(Nothing)
                                              Left str -> hasError(str)
                        in pairWithError (hasError(err)) mmissPreamble

findFirstEnv ((Env name ps fs):rest) preambleFs beforeDocument = 
  if (name `elem` (map fst (plainTextAtoms ++ envsWithText ++ envsWithoutText))) then
    let content = makeContent [(Env name ps fs)] (detectTextMode name) "Root"
    in case (fromWithError content) of
         (Left str) -> hasError(str)
         (Right cs) -> 
            if ((genericLength cs) == 0) 
              then hasError("Internal Error: no XML content could be genereated for topmost Env. '" ++ name ++ "'")
              else let ce = head cs
                   in case ce of 
			(CElem e) -> hasValue(e, Nothing)
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
     else inStr ++ (makeTextElem [f] "")

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


makeContent :: [Frag] -> Textmode -> String -> WithError [Content]

makeContent [] _ _ = hasValue([])
makeContent (f:frags) NoText parentEnv = 
   case f of
     (EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
                        in  mapWithError ([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))] ++)
                                         (makeContent frags NoText parentEnv)
     (Other str) -> if ((length (filter (not . (`elem` "\n ")) str) == 0) ||
			((head str) == '%'))
                      -- String besteht nur aus Leerzeichen oder Zeilenenden
                      then mapWithError ([(CMisc (PI (piInsertLaTeX ,str)))] ++) (makeContent frags NoText parentEnv)
		      else hasError("No text allowed inside a " ++ parentEnv ++ "!\nString found: " ++ str)
		      -- TODO: Text, der nur aus Linefeeds besteht, muss erhalten bleiben, da er Einfluss
                      --       auf das von Latex erzeugte Layout haben kann.
     (Env name ps fs) -> 
       if (name `elem` ((map fst plainTextAtoms) ++ (map fst latexEmbeddedFormulaEnvs)))
         then hasError("Environment '" ++ name ++ 
                       "' with label '" ++ (getLabelFromParams ps) ++ 
                       "' not allowed inside a " ++ parentEnv ++ "!")
         else
           if (name == "Text")
	     then hasError("No Environment 'Text'" ++ 
                           "' with label '" ++ (getLabelFromParams ps) ++ 
                           "' allowed inside a " ++ parentEnv ++ "!")
             else
               if (name `elem` (map fst mmiss2EnvIds))
	         then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
                      in myConcatWithError 
                             (cElemListWithError ename ps (makeAttribs ps name)
	                                         (makeContent fs (detectTextMode name) name))
                             (makeContent frags NoText parentEnv)
                 else  -- No MMiSS-Env. -> Put the starting and closing Kommands in a processing instruction.
                       -- Look into the environment because MMiSS-Envs could be in there: 
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
     (Command name ps) -> 
        if (name `elem` (map fst includeCommands))
	  then let ename = maybe "" snd (find ((name ==) . fst) includeCommands)
                   delimElem = case ps of
                                (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                                otherwise -> []
	       in myConcatWithError (hasValue([(CElem (Elem ename (makeIncludeAttribs ps) []))] ++ delimElem))
                                    (makeContent frags NoText parentEnv)
          else let delimStr = case ps of
                                (LParams _ _ (Just str) _) -> str
                                otherwise -> ""
              in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
                                                                   ++ (lparamsToString ps) ++ delimStr))]))
                                            (makeContent frags NoText parentEnv)


makeContent (f:frags) TextAllowed parentEnv = 
  if (parentEnv `elem` (map fst listEnvs)) 
    then
      case f of
	(EscapedChar c) -> let cstr = if (c == '\\') then "\\" else [c]
			   in  mapWithError ([(CMisc (PI (piInsertLaTeX , "\\" ++ cstr)))] ++)
					    (makeContent frags TextAllowed parentEnv)
	(Other str) -> myConcatWithError (hasValue([(CMisc (PI (piInsertLaTeX, str)))])) (makeContent frags TextAllowed parentEnv)
	(Env name ps fs) -> 
	   if (name `elem` (map fst mmiss2EnvIds))
	     then hasError("Environment '" ++ name ++ "' is not allowed in lists. Wrap it up with a \\item.")
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
	(Command name ps) ->
	  if (name `elem` itemNames)  
	    then let (content, restFrags) = makeListItem ps frags []
		 in myConcatWithError (hasValue([content])) (makeContent restFrags TextAllowed parentEnv)
	    else let delimStr = case ps of
				   (LParams _ _ (Just str) _) -> str
				   otherwise -> ""
		 in  myConcatWithError (hasValue([CMisc (PI (piInsertLaTeX ,"\\" ++ name 
								      ++ (lparamsToString ps) ++ delimStr))]))
					       (makeContent frags TextAllowed parentEnv)

    else   
    ------------------------------------------
    -- Parent is no List environment
    ------------------------------------------
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
	      let ename = if (name `elem` (map fst latexAtomFormulaEnvs))
                            then "formula"
                            else maybe "" snd (find ((name ==) . fst) plainTextAtoms)
 		  text = makeTextElem fs ""
		  content = (hasValue([CString True text]))
		  attribs = (makeAttribs ps name)
	      in myConcatWithError
		    (cElemListWithError ename ps attribs content)
		    (makeContent frags TextAllowed parentEnv)
	    else
	      if (name == "Text")
		then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
		     in  myConcatWithError (hasValue([(makeTextFragment parentEnv ename (Just(ps)) fs [])])) 
					   (makeContent frags TextAllowed parentEnv)
		else
		  if (name `elem` (map fst mmiss2EnvIds))
		    then let ename = maybe "" snd (find ((name ==) . fst) mmiss2EnvIds)
			 in myConcatWithError (cElemListWithError ename ps (makeAttribs ps name)
								  (makeContent fs (detectTextMode name) name))
					      (makeContent frags TextAllowed parentEnv)
		    else
		      if (name `elem` (map fst latexEmbeddedFormulaEnvs))
			  -- a formula environment -> we make a formlua XML element:
			then let (content, restFrags) = makeNamelessTextFragment parentEnv (f:frags) []
			     in  myConcatWithError (hasValue([content])) 
						   (makeContent restFrags TextAllowed parentEnv)
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


-- makeTextElem converts the Frags back into strings and concatenates them into the first
-- tuple component. The second string is a comma separated list of filename referenced by
-- includeGraphics or externalFile-Commands.
--

makeTextElem :: [Frag] -> String -> String

makeTextElem [] inStr = inStr
makeTextElem (f:fs) inStr = 
    case f of
      (EscapedChar c) -> let str1 = "\\" 
                             str2 = if (c == '\\') then "\\" else [c] 
                         in (makeTextElem fs (inStr ++ str1 ++ str2))
      (Other str) -> makeTextElem fs (inStr ++ str)
      (Command name ps@(LParams singlePs _ d _)) -> 
         let delimStr = case d of
                          (Just delimStr) -> delimStr
                          otherwise -> "" 
         in makeTextElem fs (inStr ++ "\\" ++ name ++ (lparamsToString ps) ++ delimStr)
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
                        "$" -> "$"
                        "$$" -> "$$"
                        "\\(" -> "\\("
                        "\\[" -> "\\["
                        otherwise -> "\\begin{" ++ name ++ "}" ++ (lparamsToString ps)
              end = case name of
                        "[]" -> "]"
                        "{}" -> "}"
                        "$" -> "$"
                        "$$" -> "$$"
                        "\\(" -> "\\)"
                        "\\[" -> "\\]"
                        otherwise -> "\\end{" ++ name ++ "}"
              newStr = makeTextElem content ""
         in makeTextElem fs (inStr ++ begin ++ beginDelimStr ++ newStr ++ end ++ endDelimStr)



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
singleParamToString (SingleParam f '{') = "{" ++ (makeTextElem f "") ++ "}"
singleParamToString (SingleParam f '[') = "[" ++ (makeTextElem f "") ++ "]" 
singleParamToString (SingleParam f '(') = "(" ++ (makeTextElem f "") ++ ")"



{-- makeNamelessTextFragment:
String: Name des Vater-Environments
1. [Frag] : Eingangsliste: Liste der noch abzuarbeitenden Fragmente auf dieser Ebene
2. [Frag] : Textfragmente: Liste der Fragmente, die in dasselbe Textfragment eingehen
3. (Content, [Frag]): Das zusammengesetzt Textfragment-XML-Element sowie die restlichen Fragmente,
                      die nicht aufgenommen wurden.

Die Funktion bekommt eine Liste mit Fragmenten, von denen das erste ein Fragment sein sollte,
dass in ein Text-Element ohne Label eingepackt werden muss. Die Funktion geht die Liste
der Eingangsfragmente durch und sammelt in der Textfragmentliste alle nachfolgenden Fragmente
zusammen, die ebenfalls in das Text-Element übernommen werden können. Dies können sein:

- Other str  -> Strings
- Escaped Chars
- Embedded-Elemente (link, reference, define etc.)
- Formel-Environments

Findet die Funktion ein Fragment, dass nicht mehr in ein Textelement gehört, dann baut es 
mittels 'makeTextFragment' ein Element vom Typ 'text' zusammen und gibt dieses zusammen
mit der Liste der übriggebliebenen Fragmente zurück.
--}

makeNamelessTextFragment :: String -> [Frag] -> [Frag] -> (Content, [Frag])
makeNamelessTextFragment parentEnv [] textFrags = 
  ((makeTextFragment parentEnv "text" Nothing textFrags []), [])
makeNamelessTextFragment parentEnv (f:frags) textFrags = 
  case f of
    (Env name _ fs) -> 
       if (name `elem` ((map fst embeddedElements) ++ (map fst latexEmbeddedFormulaEnvs)))               
         then makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
         else 
           if (name `elem` (map fst mmiss2EnvIds))
             then let e1 = (makeTextFragment parentEnv "text" Nothing textFrags [])  
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
    (Command "IncludeText" _) -> makeNamelessTextFragment parentEnv frags (textFrags ++ [f])
    (Command name _) -> 
      if (name `elem` itemNames)
        then  ((makeTextFragment parentEnv "text" Nothing textFrags []), (f:frags))
        else if (name `elem` (map fst includeCommands))
               then let e1 = (makeTextFragment parentEnv "text" Nothing textFrags [])  
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
    (Command "IncludeText" ps) -> 
         let newElem = CElem (Elem "includeText" (makeIncludeAttribs ps) [])
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
    (Command "Reference" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Ref" ps) ->
         let newElem = CElem (Elem "reference" (makeRefAttribs ps) (getLinkText ps))
             delimElem = case ps of
                           (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                           otherwise -> []
	 in  makeTextFragment parentEnv name params frags (content ++ [newElem] ++ delimElem)
    (Command "Def" ps) ->
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
      if (ename `elem` (map fst latexEmbeddedFormulaEnvs))
        then
              -- Fieser Trick: Um die Fragmente innerhalb der Formel-Umgebung in XML umzuwandeln,
              -- machen wir daraus einfach ein Textfragment mit eben dieser Funktion, in der wir uns gerade befinden
              -- und nehmen uns aus dem resultieren Element einfach den Content und stopfen ihn in das
              -- Formel-Element:
          let (CElem (Elem _ _ c)) = makeTextFragment "Text" name params fs []
              newElem = CElem (Elem "formula" (makeFormulaAttribs ename) c)
	  in  makeTextFragment parentEnv name params frags (content ++ [newElem])

        else
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
              (CElem (Elem _ _ c)) = makeTextFragment "Text" name params fs []
          in makeTextFragment parentEnv name params frags (content ++ begin ++ c ++ end)



makeListItem :: Params -> [Frag] -> [Content] -> (Content, [Frag])

makeListItem params [] contentList = 
  let delimElem = case params of
                     (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                     otherwise -> []
  in ((CElem (Elem "item" (makeListItemAttribs params) (delimElem ++ contentList))), [])

makeListItem params (f:frags) contentList =
   case f of
     (EscapedChar c) -> let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
                        in makeListItem params restFrags (contentList ++ [content])
     (Other str) -> 
        if (str /= "")
	  then let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
               in makeListItem params restFrags (contentList ++ [content]) 
	  else makeListItem params frags contentList
     (Env name ps fs) -> 
        if (name `elem` (map fst plainTextAtoms))
          then
            let ename = if (name `elem` (map fst latexAtomFormulaEnvs))
                          then "formula"
                          else maybe "" snd (find ((name ==) . fst) plainTextAtoms)
                text = makeTextElem fs "" 
                attribs = (makeAttribs ps name)
            in  makeListItem params frags 
                             (contentList ++ [(CElem (Elem ename attribs
                                                          [CString True text]))])
          else
            if (name == "Text")
	      then makeListItem params frags 
                                (contentList ++ [(makeTextFragment "Item" "text" (Just(ps)) fs [])])
              else if (name `elem` (map fst listEnvs))
                     then makeListItem params frags 
                                       (contentList ++ coerceWithError(makeContent [f] TextAllowed "Item"))
                     else 
                       if (name `elem` (map fst latexEmbeddedFormulaEnvs))
                         -- als erstes Env. innerhalb eines ListItems kommt eine Formel-Umgebung -> Textfragment erzeugen
            	         then let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
                              in makeListItem params restFrags (contentList ++ [content]) 
                         else
                           if (not (name `elem` (map fst mmiss2EnvIds)))
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
     (Command "IncludeText" ps) -> 
        let newElem = CElem (Elem "includeText" (makeIncludeAttribs ps) [])
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

     (Command name ps) -> 
       if (name `elem` itemNames)
         then 
           let delimElem = case params of
                             (LParams _ _ (Just delimStr) _) -> [(CMisc (PI (piInsertLaTeX, delimStr)))]
                             _ -> []
           in ((CElem (Elem "item" (makeListItemAttribs params) (delimElem ++ contentList))), (f:frags))
         else 
          let (content, restFrags) = makeNamelessTextFragment "Item" (f:frags) []
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
concatTextElems (c:cs) = concatTextElems cs

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
    else if (name == "Text")
           then makeTextFragmentAttribs (Just(ps))
	   else if (name `elem` ["Link"])
	          then makeLinkAttribs ps
		  else if (name `elem` ["Reference", "Ref"]) 
		         then makeRefAttribs ps
			 else if (name == "Def")
			        then makeDefineAttribs ps
				else if (name `elem` (map fst (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs)))
                                       then let (LParams _ atts _ _) = id ps
				                normalAttrs = map convertAttrib atts
                                                formulaAttrs = makeFormulaAttribs name
                                            in normalAttrs ++ formulaAttrs
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
   [("defined", (AttValue [Left labelId]))] ++ map convertAttrib (filter ((not . (== "OptText")) . fst) atts)
makeDefineAttribs (LParams [] atts _ _) = map convertAttrib (filter ((not . (== "OptText")) . fst) atts)


{-- makeFormulaAttribs weicht vom Schema der anderen makeXXXAttribs-Funktionen ab, da bei Formel-Umgebungen
    die nötigen Infos zum Befüllen der XML-Attribute nicht in den Parametern stecken, sondern im Namen
    des Environments, das als String übergeben wird:
--}
makeFormulaAttribs :: String -> [Attribute]
makeFormulaAttribs name =
  let latexEnv = maybe "" snd (find ((name ==) . fst) (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs))
  in  [("latexEnv", (AttValue [Left latexEnv]))]


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
getDefineText (LParams _ atts _ _) =
  case (find ((== "OptText") . fst) atts) of
    Just((_, linkText)) -> [(CString True linkText)]
    Nothing -> []

getLabelFromParams :: Params -> String
getLabelFromParams (LParams _ atts _ _) =
  case (find ((== "Label") . fst) atts) of
    Just((_, label)) -> label
    Nothing -> ""


convertAttrib :: (String, String) -> Attribute
convertAttrib (l, r) = ((attNameToXML l), AttValue [Left (latexToUnicode r)])


getEmphasisText :: Params -> String
getEmphasisText (LParams [] _ _ _) = ""
getEmphasisText (LParams ((SingleParam ((Other s):[]) _):ps) _ _ _) = s


unicodeToLatex :: String -> String
unicodeToLatex inStr = foldl (applyTranslation "") inStr unicodeToLatexTranslations


latexToUnicode :: String -> String
latexToUnicode inStr = foldl (applyTranslation "") inStr latexToUnicodeTranslations

applyTranslation :: String -> String -> (String, String) -> String
applyTranslation outStr inStr (search, replaceStr) =
   if lenInStr < lenSearch 
     then outStr ++ inStr
     else if (isPrefixOf search inStr)
            then applyTranslation (outStr ++ replaceStr) (drop lenSearch inStr)  (search, replaceStr)
            else applyTranslation (outStr ++ (take 1 inStr)) (drop 1 inStr)  (search, replaceStr)
   where
   lenInStr = genericLength inStr
   lenSearch = genericLength search   


{-- makeMMiSSLatex1 erzeugt aus einem XML-Element die zugehoerige MMiSSLatex-Repraesentation.
    Element ist das Root-Element des auszugebenden Dokumentbaumes, der Bool-Wert legt fest,
    ob das erzeugte LaTeX-Fragment ein komplettes File sein soll, dass ohne Änderungen
    geteXt werden kann (True), oder nicht (False). Wenn es komplett sein soll, dann wird 
    eine Praeambel erzeugt und 'includeXXX'-Elemente werden in MMiSSLaTeX-Include-Kommandos
    umgesetzt. Ist der Bool-Wert 'False', dann wird LaTeX für den XEmacs-Buffer erzeugt. In
    diesem Fall wird keine Preamble generiert und die Includes werden in spezielle EmacsContent-Objekte
    umgesetzt, die im MMiSS-XEmacs-Mode speziell behandelt werden.
--}

newtype MMiSSExtraPreambleData = MMiSSExtraPreambleData {
   callSite :: Maybe EntitySearchName
      -- How the element to which this preamble belongs was referred to
      -- originally by the user.
      -- Nothing means that this is the head element.
   }

makeMMiSSLatex1 :: 
   (Element, Bool, [(MMiSSLatexPreamble,[MMiSSExtraPreambleData])]) 
   -> WithError (EmacsContent ((String, Char), [Attribute]))
   -- Each distinct preamble occurs once in the list, paired with a list
   -- for each of its call-sites.
makeMMiSSLatex1 (element,preOut,preambles') =
   -- stub function that doesn't use MMiSSExtraPreambleData for now.
   makeMMiSSLatex11 (element,preOut,map fst preambles')


makeMMiSSLatex11 :: (Element, Bool, [MMiSSLatexPreamble]) -> WithError (EmacsContent ((String, Char), [Attribute]))

makeMMiSSLatex11 ((Elem name atts content), preOut, preambles) = 
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

fillLatex out ((CElem (Elem "text" atts contents)):cs) inList = 
  let (b_insert, e_insert) = if (out && ((getParam "priority" atts) == "0"))
                               then ("\\begin{Included}\n", "\n\\end{Included}")
                               else ("","")
      s1 = "\\begin{Text}" 
      s2 = "[" ++ (getAttribs atts "" []) ++ "]"
      s3 = "\\end{Text}"
      items = if (s2 == "[]") 
                then (fillLatex out contents [])
                else [(EditableText (b_insert ++ s1 ++ s2))] 
                     ++ (fillLatex out contents []) 
                     ++ [(EditableText (s3 ++ e_insert))]
  in fillLatex out cs (inList ++ items)

fillLatex out ((CElem (Elem "item" atts contents)):cs) inList = 
   let s1 = "\\item" 
       attrStr = (getAttribs atts "" [])
       s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "] "
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

-- Translates Link- and Reference-Embedded-Ops:
--
fillLatex out ((CElem (Elem name atts contents)):cs) inList
  | (name `elem` (map snd linkCommands)) =  
    let s1 = "\\" ++ (elemNameToLaTeX name) 
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                          in case c of
                              (CString _ body) -> "[" ++ body ++ "]"
                              _ -> ""
        s2 = "{" ++ (getParam "type" atts) ++ "}"
        s3 = "{" ++ (getParam "linked" atts) ++ "}"
        items = [(EditableText (s1 ++ phrase ++ s2 ++ s3))]
    in fillLatex out cs (inList ++ items)
  | (name `elem` (map snd refCommands)) =  
    let s1 = "\\" ++ (elemNameToLaTeX name) 
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                          in case c of
                              (CString _ body) -> "[" ++ body ++ "]"
                              _ -> ""
        s2 = "{" ++  (getParam "referenced" atts) ++ "}"
        items = [(EditableText (s1 ++ phrase ++ s2))]
    in fillLatex out cs (inList ++ items)
  | (name == "define") =
    let s1 = "\\" ++ (elemNameToLaTeX name)
        s2 = "{" ++ (getParam "defined" atts) ++ "}"
        phrase = if (length(contents) == 0) 
                   then "" 
                   else let c = head contents
                      in case c of
                           (CString _ body) -> "[" ++ body ++ "]"
                           _ -> ""
        items = [(EditableText (s1 ++ phrase ++ s2))]
    in fillLatex out cs (inList ++ items)
  | (name == "formula") =  
    let envType = getParam "latexEnv" atts
        latexEnv = maybe "" fst (find ((envType ==) . snd) (latexEmbeddedFormulaEnvs ++ latexAtomFormulaEnvs))
        (s1, s2) = case latexEnv of
                      "$" -> ("$", "$")
                      "$$" -> ("$$", "$$")
                      "\\(" -> ("\\(", "\\)")
                      "\\[" -> ("\\[", "\\]")
                      otherwise -> ("\\begin{" ++ latexEnv ++ "}", "\\end{" ++ latexEnv ++ "}")
    in fillLatex out cs (inList ++ [(EditableText s1)] ++ (fillLatex out contents []) ++ [(EditableText s2)])     
        
fillLatex out ((CString _ str):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (Comment str)):cs) inList = fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CMisc (PI (piInsertLaTeX, str))):cs) inList =  fillLatex out cs (inList ++ [(EditableText str)])

fillLatex out ((CElem (Elem name atts contents)):cs) inList = 
  let (b_insert, e_insert) = if (out && ((getParam "priority" atts) == "0"))
                               then ("\\begin{Included}\n", "\n\\end{Included}")
                               else ("","")
      s1 = "\\begin{" ++ (elemNameToLaTeX name) ++ "}" 
      attrStr = getAttribs atts "" []
      s2 = if (attrStr == "") then "" else "[" ++ attrStr ++ "]"
      s3 = "\\end{" ++ (elemNameToLaTeX name) ++ "}"
      items = [(EditableText ( b_insert ++ s1 ++ s2))] ++ (fillLatex out contents []) 
              ++ [(EditableText (s3 ++ e_insert))]
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
   if (name `elem` (excludeList ++ glAttribsExclude))
     then getAttribs as str excludeList
     else getAttribs as (str ++ "," ++ attNameToLatex(name) ++ "={" ++ (unicodeToLatex value) ++ "}") excludeList                

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
                             (LParams _ _ (Just delimStr) _) -> [(CString True delimStr)]
                             otherwise -> []
          endDelimElem =   case ps of
                             (LParams _ _ _ (Just delimStr)) -> [(CString True delimStr)]
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
toIncludeStr 'G' = "Package"
toIncludeStr 'U' = "Unit"
toIncludeStr 'E' = "CompositeUnit"
toIncludeStr 'A' = "Atom"
toIncludeStr 'T' = "Text"
toIncludeStr 'S' = "Section"
toIncludeStr 'c' = "ProgramComponent"
toIncludeStr 'm' = "Term"
toIncludeStr 'D' = "DevelopmentStep"
toIncludeStr 'P' = "Proof"
toIncludeStr 'o' = "ProofStep"
toIncludeStr _ = error "MMiSSDTDAssumptions.toIncludeStr - bad mini-type"


---
-- fromIncludeStrOpt
-- and also handles the case where the first letter is lower-cased.
fromIncludeStrOpt :: String -> Maybe Char
fromIncludeStrOpt "Package" = Just 'G'
fromIncludeStrOpt "Unit" = Just 'U'
fromIncludeStrOpt "Atom" = Just 'A'
fromIncludeStrOpt "Text" = Just 'T'
fromIncludeStrOpt "Section"          = Just 'S'                
fromIncludeStrOpt "CompositeUnit"    = Just 'E'        
fromIncludeStrOpt "ProgramComponent" = Just 'c'         
fromIncludeStrOpt "Proof"            = Just 'P'        
fromIncludeStrOpt "ProofStep"        = Just 'o'        
fromIncludeStrOpt "DevelopmentStep"  = Just 'D' 
fromIncludeStrOpt "Term"             = Just 'm' 
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
      "package" -> "Package"
      "paragraph" -> "Unit"
      "abstract" -> "Unit"
      "introduction" -> "Unit"
      "summary" -> "Unit"
      "theory" -> "Unit"
      "program" -> "Unit"
      "view" -> "Unit"
      "list" -> "CompositeUnit"
      "itemize" -> "CompositeUnit"
      "enumerate" -> "CompositeUnit"
      "description" -> "CompositeUnit"
      "example" -> "CompositeUnit"
      "exercise" -> "CompositeUnit"
      "assignment" -> "CompositeUnit"
      "solution" -> "CompositeUnit"
      "illustration" -> "CompositeUnit"
      "proposition" -> "CompositeUnit"
      "definition" -> "ConceptualUnit"
      "theorem" -> "CompositeUnit"
      "conjecture" -> "CompositeUnit"
      "falseConjecture" -> "CompositeUnit"
      "lemma" -> "CompositeUnit"
      "corollary" -> "CompositeUnit"
      "assertion" -> "CompositeUnit"
      "proof" -> "CompositeUnit"
      "development" -> "CompositeUnit"
      "comment" -> "Annotation"
      "note" -> "Annotation"
      "message" -> "Annotation"
      "error" -> "Annotation"
      "glossary" -> "Annotation"
      "bibEntry" -> "Atom"
      _ -> mapUpper s
   where
      mapUpper [] = []
      mapUpper (c : cs) = toUpper c : cs      



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
