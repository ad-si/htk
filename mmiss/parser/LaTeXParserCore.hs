module LaTeXParserCore (
   Frag(..),
   Params(..),
   SingleParam(..),
   SpecialFragType(..),
   Attributes,

   parseFrags,  -- :: String -> Either ParseError [Frag]
   findInString,  -- :: String -> String -> Bool
                  -- Searches for the second string in the first string.
                  -- Returns True if found, otherwise False.

   piInsertLaTeX,   -- :: String
   piSpecial,      -- :: String
   latexToUnicodeTranslations,  -- :: String

   parseString,  -- :: Frag -> [Frag]
   lparamsToString, -- :: Params -> String
   singleParamToString, -- :: SingleParam -> String
   makeTextElem, -- :: [Frag] -> String -> String
   attNameToLatex, -- :: String -> String
   unicodeToLatex, -- :: String -> String
   latexToUnicode,  -- :: String -> String
   elemNameToLaTeX, -- :: String -> String
   attNameToXML, -- :: String -> String

   entityNameParser1,   -- :: GenParser Char st EntityName
   entityFullNameParser1,   -- :: GenParser Char st EntityName
   entitySearchNameParser1,   -- :: GenParser Char st EntityName
   commaSep,

   plainTextAtoms,       -- :: [(String, String)]
   mmissPlainTextAtoms,  -- :: [String]
   envsWithText,         -- :: [(String, String)]
   envsWithoutText,      -- :: [(String, String)]
   mmiss2EnvIds,         -- :: [(String, String)]
   latexEmbeddedFormulaEnvs,  -- :: [(String, String)]
   latexAtomFormulaEnvs,      -- :: [(String, String)]
   linkCommands,              -- :: [(String, String)]
   refCommands,               -- :: [(String, String)]
   includeCommands,           -- :: [(String, String)]
   listEnvs,                  -- :: [(String, String)]
   itemNames,                 -- :: [String]
   embeddedElements,          -- :: [(String, String)]
   glAttribsExclude,          -- :: [String]
   mmissSystemAttribs,        -- :: [String]
   mmissVariantAttribs,       -- :: [String]
   mmissIncludeAttribs,       -- :: [String]
   mmissEmbeddedEnvs          -- :: [String]
)
 where

-- module LaTeXParserCore where

import Data.List
import Data.Char
import Control.Monad

import Text.ParserCombinators.Parsec

import Computation hiding (try)
-- import ExtendedPrelude(unsplitByChar,mapEq)
-- import EmacsContent
import EntityNames
-- import AtomString
-- import CodedValue
-- import QuickReadShow


type EnvId = String
type Command = String
type Attributes = [(String, String)]
type Other = String
type Delimiter = String

-- SingleParam nimmt die Daten f�r einen einfachen (also keine Attributlisten-Parameter)
-- Paramter eines Latex-Kommandos oder einer Umgebung auf. Die Komponente 'Char'
-- nimmt das konkrete Klammerzeichen, mit dem der Parameter links abgegrenzt wird, auf.
-- (Davon ausgehend ist klar, wie das korrespondierende rechte Klammerzeichen aussehen muss:
data SingleParam = SingleParam [Frag] Char    deriving Show

data SpecialFragType = InputStart | InputEnd deriving (Eq, Show, Read)


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
          | Other Other
          | Special SpecialFragType String    deriving Show

-- Parameter of LateX-Envs and Commands. Der erste der beiden Maybe Delimiter-Komponenten
-- wird benutzt, um bei Commands den String aufzunehmen, der das Command vom nachfolgenden
-- Text trennt, also entweder ein {} oder Leerzeichen (inclusive Tabs und Newlines).
-- Die rechte Delimiter-Komponente

data Params = LParams [SingleParam] Attributes (Maybe Delimiter) (Maybe Delimiter)
              deriving Show


-- ------------------------------------------------------------------------------------------
--
-- Hauptfunktion:
-- parseFrags parsiert einen MMiSSLatex-String.
--
-- ------------------------------------------------------------------------------------------
parseFrags :: String -> Either ParseError [Frag]
parseFrags str = parse (frags []) "" str



-- piInsertLaTeX enth�lt den String, der zur Kennzeichnung der Processing-Instructions dienst,
-- die in den resultierenden XML-Baum eingef�gt werden, um Latex-Anteile zu bewahren, die nicht
-- zur Struktur und nicht zu den Textbestandteilen geh�ren.

piInsertLaTeX :: String
piInsertLaTeX = "mmissInsertLaTeX"

piSpecial :: String
piSpecial = "mmissSpecial"

{- The search/replace strings listed in latexToUnicodeTranslations are
applied to attribute values when they are stored in XML-attribute
instances: -}

latexToUnicodeTranslations :: [(String, String)]
latexToUnicodeTranslations = ("\\ss", "\223") :
    map ( \ (a, b) -> (b, a)) unicodeToLatexTranslations

unicodeToLatexTranslations :: [(String, String)]
unicodeToLatexTranslations =
    [("\196", "\\\"A"), ("\214", "\\\"O"), ("\220", "\\\"U")]
    ++ ("\223", "\\ss{}")
    : [("\228", "\\\"a"), ("\246","\\\"o"), ("\252", "\\\"u")]

mmissPlainTextAtoms :: [(String, String)]
mmissPlainTextAtoms = [("Table","table"), ("Glossaryentry", "glossaryEntry"), ("Bibentry", "bibEntry")] ++
                 [("Figure", "figure"), ("ProgramFragment", "programFragment")] ++
                 [("Authorentry", "authorEntry"), ("Rule", "rule"), ("ProofStep", "proofStep")] ++
                 [("DevelopmentStep","developmentStep"), ("Source", "source"), ("Declaration","declaration")] ++
                 [ ("Axiom","axiom")]

plainTextAtoms :: [(String, String)]
plainTextAtoms =  mmissPlainTextAtoms ++ latexAtomFormulaEnvs

envsWithText :: [(String, String)]
envsWithText = [("Section", "section"), ("Paragraph", "paragraph"), ("Abstract", "abstract")] ++
               [("Introduction", "introduction"),  ("Summary", "summary"), ("Program","program")] ++
               [("Exercise","exercise"), ("Example","example"),  ("Definition","definition")] ++
               [("Theory","theory"), ("Theorem", "theorem"), ("Development","development")] ++
               [("Proof","proof"), ("Script","script"), ("item", "item"), ("ListItem", "item")] ++
               [("Conjecture", "conjecture"), ("Lemma", "lemma"), ("Corollary", "corollary")] ++
               [("Assertion", "assertion"), ("Text","text"), ("Assignment", "assignment")] ++
               [("Solution", "solution")] ++
               [("Proposition", "proposition"), ("FalseConjecture", "falseConjecture")] ++
               [("Item","item"), ("Comment","comment"),("Note","note"),("Warning","warning")] ++
               [("Error","error"), ("Glossary", "glossary")] ++ listEnvs

envsWithoutText :: [(String, String)]
envsWithoutText = [("Package", "package")]


includeCommands :: [String]
includeCommands = map (("Include" ++) . fst) envsWithText
{--
includeCommands =  [("IncludeUnit", "includeUnit"), ("IncludeSection", "includeSection")] ++
                   [("IncludeAtom", "includeAtom"), ("IncludeText","includeText")] ++
                   [("IncludeProgramComponent","includeProgramComponent")] ++
                   [("IncludeCompositeUnit", "includeCompositeUnit"), ("IncludeTerm","includeTerm")] ++
                   [("IncludeProofStep", "includeProofStep"), ("IncludeProof", "includeProof")] ++
                   [("IncludeDevelopmentStep", "includeDevelopmentStep"), ("IncludeTable","includeTable")] ++
                   [("IncludeFigure","includeFigure"), ("IncludeAnnotation","includeAnnotation")]
--}

linkCommands :: [(String, String)]
linkCommands = [("Link", "link")]

refCommands :: [(String, String)]
refCommands = [("Reference","reference"), ("Ref","reference")]

linkAndRefCommands :: [(String, String)]
linkAndRefCommands = linkCommands ++ refCommands

embeddedElements :: [(String, String)]
embeddedElements = [("Emphasis","emphasis"), ("IncludeText","includeText")] ++
                   [("Link","link") , ("Def", "define"), ("Reference", "reference")] ++
                   [("Ref", "reference"), ("Cite", "cite")]


listEnvs :: [(String, String)]
listEnvs = [("Itemize", "itemize"), ("Description", "description"), ("Enumerate", "enumerate"), ("List","list")] ++
           [("description", "description"),("itemize", "itemize"),("enumerate", "enumerate")]

itemNames :: [String]
itemNames = ["ListItem", "item", "Item"]


-- mmiss2EnvIds enthaelt alle gueltigen Environment-Ids.

mmiss2EnvIds :: [(String, String)]
mmiss2EnvIds = plainTextAtoms ++ envsWithText ++ envsWithoutText ++ linkAndRefCommands


-- TODO: Abstracts, Introduction und Summary k�nnen sowohl Sections, also Groups, als
-- auch Paragraphs, also Units sein Sie sind hier erstmal als Unit klassifiziert, dass
-- muss aber noch erg�nzt werden.

mmissEmbeddedEnvs :: [String]
mmissEmbeddedEnvs = ["emphasis", "cite", "link", "reference", "define","refRelation", "relate"]

-- LaTeX-Environments, deren Inhalt nicht geparst werden soll:
latexPlainTextEnvs :: [String]
latexPlainTextEnvs = ["verbatim", "verbatim*", "code", "xcode", "scode", "math", "displaymath", "equation"] ++
                     ["alltt", "lstlisting", "array", "tabularx"] ++ (map fst plainTextAtoms)


-- LaTeX-Environments for formulas are translated to the XML-Element 'formula' which has an attribute 'boundsType'
-- which takes an symbol that indicates the concrete LaTeX-Environment the user chose for a particular formula item.
-- The following list matches the various LaTeX formula enviroments to theses symbolic names recorded in the
-- boundsType attribute:

latexEmbeddedFormulaEnvs :: [(String, String)]
latexEmbeddedFormulaEnvs = [("math", "math"), ("$", "shortMathDollar"), ("$$", "shortDisplaymathDollar")] ++
                   [("\\(", "shortMathParens")]

latexAtomFormulaEnvs :: [(String, String)]
latexAtomFormulaEnvs =  [("\\[", "shortDisplaymath"), ("equation", "equation"), ("displaymath", "displaymath")]
                     ++ [("eqnarray", "eqnarray"), ("eqnarray*", "eqnarrayStar"), ("equation*", "equationStar")]


-- glAttribsExclude is a list with attribute names which are omitted in the process of generating MMiSSLaTeX from
-- XML.

glAttribsExclude :: [String]
glAttribsExclude = ["files","object-class","version"]


mmissSystemAttribs :: [String]
mmissSystemAttribs = [ "object-class","label","priority","packageId","packagePath","files"]

mmissVariantAttribs :: [String]
mmissVariantAttribs = ["xml:lang","format","formalism","levelOfDetail","interactionLevel","version"]

mmissIncludeAttribs :: [String]
mmissIncludeAttribs = ["included","status","priority","object-class"]

---------------------------------------------------------------------------------------------
--
-- Hier beginnen die Parser
--
---------------------------------------------------------------------------------------------

backslash :: GenParser Char st Char
backslash = char '\\'

idParser :: GenParser Char st String
idParser = many (noneOf "}]")

commaSep :: GenParser Char st a -> GenParser Char st [a]
commaSep p = p `sepBy` (oneOf ",")

-- equalSep p = p `sepBy` (oneOf "=")



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
attParser = do attlist <- commaSep attribute
               return(normalizeAttlist attlist)
            <|> return([])
  where
    normalizeAttlist [] = []
    normalizeAttlist ((Just(key,value)):as) = (key,value):(normalizeAttlist as)
    normalizeAttlist ((Nothing):as) = normalizeAttlist as


attribute :: GenParser Char st (Maybe (String, String))
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
                 "Label" ->
                    case v of
                      "{}" -> done
                      otherwise ->
                        let elEntityName = parse entityFullNameParser "" v
                        in case elEntityName of
                              Left err -> fail (appendSourcePos pos
                                               ("Label '" ++ v ++ "' contains illegal characters "))
                              Right _ -> done
                 _ -> done
               if (v == "{}")
                 then return Nothing
                 else return (Just(key,v))

{--
delimitedValue :: String -> GenParser Char st String
delimitedValue key = try(between (char '{') (char '}') (oldvalue key))
--}

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

singleOptParser :: GenParser Char st String
singleOptParser = many (noneOf "],")


-- -------------------------------------------------------------------------------------
--
-- Hier beginnen die Parser f�r die Fragmente (Frag)
--
-- -------------------------------------------------------------------------------------

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

  | id `elem` includeCommands =
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
         refTypeAttrs <- case id of
                           "Reference" -> return([("type","long")])
                           _ -> return([("type","short")])
         return (LParams [(SingleParam [(Other labelId)] '{')]
                         ([("status","absent")] ++ linkTextAttrs ++ refTypeAttrs) Nothing Nothing)

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
--                  <?> (appendSourcePos pos ("[phrase]{element type}{referenced LabelID} for Command <" ++ id ++ ">"))
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


continueAdhocEnv :: Char -> [Frag] -> GenParser Char st [Frag]
continueAdhocEnv closingChar l =
  do try (char closingChar)
     return l
  <|> do f <- frag
         continueAdhocEnv closingChar (f:l)


-- adhocEnvironment erkennt Umgebungen ohne Namen: ...{text}...
adhocEnvironment :: GenParser Char st Frag
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
                  <|> do blankDelim <- try (string "\n")
                         return (LParams [] [] (Just(blankDelim)) Nothing)
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


-- mmissSpecialComment erkennt spezielle Kommentare, die durch das MMiSS repository
-- bzw. den UnParser eingef�gt wurden, um z.B. den Start und das Ende von expandierten
-- File-Includes zu kennzeichnen. Diese m�ssen beim Parsen wiedererkannt werden,
-- damit sie in die XML-Repr�sentation einfliessen und nicht als normale Kommentare
-- behandelt werden.

mmissSpecialComment :: GenParser Char st Frag
mmissSpecialComment =
  try(
    do string "%%MMiSS:>"
       specialTypeStr <- many1 (noneOf ">")
       if ((specialTypeStr == "InputStart") ||
           (specialTypeStr == "InputEnd"))
         then do char '>'
                 s <- manyTill anyChar (try newline)
                 return(Special (read specialTypeStr) s)
         else return(Other ("%%MMiSS:>Error:Unrecognized specialFragType:" ++ specialTypeStr ++ "\n"))
  )
-- comment erkennt Kommentarzeilen. Kommentare werden als Other-Fragment behandelt
--

comment :: GenParser Char st Frag
comment = try(do char '%'
                 s <- manyTill anyChar (try newline)
                 return (Other ("%" ++ s ++ "\n")))


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
            str <- plainTextWithoutDollar ""
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

plainText :: String -> GenParser Char st String
plainText stopChars = many1 (noneOf stopChars)

plainTextWithoutDollar :: String -> GenParser Char st String
plainTextWithoutDollar str =
  try (do char '$'
          return str)
  <|> try (do backslash
              char '$'
              plainTextWithoutDollar (str ++ "\\$"))
  <|> try (do backslash
              c <- anyChar
              plainTextWithoutDollar (str ++ "\\" ++ [c]))
  <|> try (do newstr <- many1 (noneOf ("\\$"))
              plainTextWithoutDollar (str ++ newstr))


-- frag erkennt Latex-Fragmente: Kommentare, Environments, Commands und Escaped Chars.
-- Alle anderen Zeichenfolgen werden in das Fragment 'other' verpackt.

frag :: GenParser Char st Frag
frag =
    mmissSpecialComment
    <|> comment
    <|> do backslash
           mathEnv <|> beginBlock <|> escapedChar <|> command <|> return (Other "\\")
    <|> adhocEnvironment
    <|> simpleMathEnv
    <|> other


-- frags ist der Haupt-Parser. Er sammelt die Fragmente der Root-Ebene ein.

frags :: [Frag] -> GenParser Char st [Frag]
frags l =
  do f <-  frag <?> "Fragment"
     frags (f:l)
  <|> return(reverse l)


-- ----------------------------------------------------------------------------------------
--
--  Hilfsfunktionen f�r das Auffinden von Pragmas
--
-- ----------------------------------------------------------------------------------------

findInString :: String -> String -> Bool

findInString [] _ = False
findInString str search
  | isPrefixOf search str = True
  | otherwise = findInString (drop 1 str) search


-- ----------------------------------------------------------------------------------------
--
--  Einige Hilfsfunktionen f�r die Module, die mit Params und Frags umgehen m�ssen
--
-- ----------------------------------------------------------------------------------------

-- makeTextElem converts the Frags back into strings.

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
      otherwise -> makeTextElem fs inStr


{-- lparamsToString formatiert Params (Command-Parameter) in die urspr�ngliche Latex-Form --}

lparamsToString :: Params -> String
lparamsToString (LParams singleParams atts _ _) =
  let pStr = (concat (map singleParamToString singleParams))
      attStr = attribsToString atts ""
      resultStr = if (attStr == "")
                    then pStr
                    else pStr ++ "[" ++ attStr ++ "]"
  in resultStr


singleParamToString :: SingleParam -> String
singleParamToString (SingleParam f '{') = "{" ++ (makeTextElem f "") ++ "}"
singleParamToString (SingleParam f '[') = "[" ++ (makeTextElem f "") ++ "]"
singleParamToString (SingleParam f '(') = "(" ++ (makeTextElem f "") ++ ")"


attribsToString :: Attributes -> String -> String
attribsToString [] str = if ((take 1 str) == ",")
                           then (drop 1 str)
                           else str
attribsToString ((name, value):as) str =
   if (name `elem` glAttribsExclude)
     then attribsToString as str
     else attribsToString as (str ++ "," ++ attNameToLatex(name) ++ "={" ++ (unicodeToLatex value) ++ "}")


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


attNameToLatex :: String -> String
attNameToLatex "xml:lang" = "Language"
attNameToLatex name = [(toUpper (head name))] ++ (tail name)

attNameToXML :: String -> String
attNameToXML "Language" = "xml:lang"
attNameToXML name = [(toLower (head name))] ++ (tail name)


elemNameToLaTeX :: String -> String
elemNameToLaTeX name = maybe "" fst (find ((name ==) . snd)
                                          (mmiss2EnvIds ++ embeddedElements))




-- ---------------------------------------------------------------------------------------
--
-- (EntityName, EntityFullName, EntitySearchName parsers now in EntityNames.hs, and the
-- old code that used to be here has been deleted.  Note that we are now much stricter,
-- allowing only letters and digits in names; however we can extend that later if necessary.)
-- (GER, 18/9/03)
--
-- Instead we include versions of the parsers which check that all the input is read.
--
-- ---------------------------------------------------------------------------------------

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

-- ------------------------------------------------------------------------------------
parseString :: Frag -> [Frag]

parseString (Other str) =
  let result = parse (frags []) "" str
  in case result of
       Right fs  -> fs
       Left err -> []

parseString _ = []


appendSourcePos :: SourcePos -> String -> String
appendSourcePos pos str = str ++ "in Line "
                          ++ (show (sourceLine pos)) ++ " Column "
                          ++ (show (sourceColumn pos)) ++ "."



