{-- OntoParser stellt eine Funktion zum Parsen von mmisslatex-Ontologiebefehlen 
    in LaTeX-Files (oder anderen Textfiles) zur Verfügung. 
--}

module OntoParser (
  OFrag(..),
  ClassDecl(..),
  ObjectDecl(..),
--  RelationDecl(..),
  BaseRelationDecl(..),
  RelationTypeDecl(..),
  ObjectLink(..),
  parseOntology
)

where

import Computation hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Maybe

type Other = String

data ClassDecl = ClassDecl {
  className :: String,
  classText :: String,
  super :: Maybe String
} deriving(Read, Show)

data ObjectDecl = ObjectDecl {
  objName :: String,
  objectText :: String,
  instanceOf :: String
} deriving(Read, Show)
{--
data RelationDecl = RelationDecl {
  multiplicities :: Maybe String,
  relName :: String,
  relationText :: String,
  source :: String,
  target :: String
} deriving(Read, Show)
--}
data BaseRelationDecl = BaseRelationDecl {
  baseRelName :: String,
  baseRelationText :: String
} deriving(Read, Show)

data RelationTypeDecl = RelationTypeDecl {
  multiplicities :: Maybe String,
  nameOfRel :: String,
  nameOfSource :: String,
  nameOfTarget :: String,
  superRel :: Maybe String
} deriving(Read, Show)

data ObjectLink = ObjectLink {
  sourceObj :: String,
  targetObj :: String,
  linkRelation :: String
} deriving(Read, Show)

data OFrag = 
   ClassDeclFrag ClassDecl 
 | ObjectDeclFrag ObjectDecl
-- | RelationDeclFrag RelationDecl
 | BaseRelationDeclFrag BaseRelationDecl
 | RelationTypeDeclFrag RelationTypeDecl
 | ObjectLinkFrag ObjectLink
 | OtherFrag Other deriving(Read,Show)

{--
parseMMiSSOntologyFile :: SourceName -> IO(WithError MMiSSOntology)
parseMMiSSOntologyFile s = 
 do peFs <- parseFromFile (ontoDoc [] []) s
    case peFs of
      Right fs -> return (generateOntology (emptyMMiSSOntology "Test" AutoInsert) fs)
      Left err -> return (hasError (show err))
--}
{-- ontoDoc ist die Hauptparser-Funktion. Jedes erkannte Fragment wird direkt verarbeitet und in 
    Ontologie-Datenstruktur aufgenommen. Relationen werden so beim Parsen aufgesammelt und dem
    Parser für Objekt-Links zur Verfügung gestellt.
--}

parseOntology :: String -> Either ParseError [OFrag]
parseOntology str = parse (ontoDoc [] []) "" str 


ontoDoc :: [String] -> [OFrag] -> GenParser Char st [OFrag]
ontoDoc rels fs =  
  do eof
     return(fs)
  <|> do f <- (frag rels) <?> "Fragment"
         ontoDoc rels (fs ++ [f])
 



-- frag ist der eigentliche Parser für Fragmente. Als ersten Parameter bekommt er eine Liste der
-- bisher beim Parsen aufgesammelten Relationsnamen übergeben, damit der Link-Parser (aufgerufen vom
-- ontologyElement-Parser) die Makros, die einem Relationsnamen entsprechen als Links erkennen kann.

frag :: [String] -> GenParser Char st OFrag
frag rels = 
       comment
       <|> do backslash
              (try (ontologyElement rels)) <|> escapedChar <|> return(OtherFrag "\\")
       <|> do eof
              return(OtherFrag "")
       <|> other

backslash :: GenParser Char st Char
backslash = char '\\'

escapedChar :: GenParser Char st OFrag
escapedChar = do c <- anyChar
                 return (OtherFrag [c])

other :: GenParser Char st OFrag
other = -- do str <- manyTill anyChar (try(oneOf "\\%\n"))
         do  str <- (many1 (noneOf "\\%"))
             return(OtherFrag str)

comment :: GenParser Char st OFrag
comment = do char '%'
             s <- manyTill anyChar (try newline)
             return (OtherFrag "")

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


ontologyElement :: [String] -> GenParser Char st OFrag
ontologyElement rels = declClassP <|> declObjectP  -- <|> declRelationP 
                       <|> declBaseRelationP <|> declRelTypeP 
                       <|> objRelationP


declClassP :: GenParser Char st OFrag

declClassP = 
  do try (string "DeclClass") <|> (try (string "Class"))
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultText <- (try (between (char '{') (char '}') (value "")))
     spaces
     superClass <- try(between (char '{') (char '}') idParser)
     superClassValue <- if (superClass == "")
                          then return(Nothing)
                          else return(Just(superClass))     
     return(ClassDeclFrag (ClassDecl name defaultText superClassValue))

declObjectP = 
  do try (string "DeclObject") <|> (try (string "Object"))
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultText <- (try (between (char '{') (char '}') (value "")))
     spaces
     instanceOf <- try(between (char '{') (char '}') idParser)
     return(ObjectDeclFrag (ObjectDecl name defaultText instanceOf))
{--
declRelationP = 
  do try (string "DeclRelation")
     card <- option "" (try(between (char '{') (char '}') idParser)) 
     cardValue <- if (card == "") 
                    then return(Nothing)
                    else return(Just(card))
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultText <- try(between (char '{') (char '}') idParser)
     spaces
     sourceClass <- try(between (char '{') (char '}') idParser)
     spaces
     targetClass <- try(between (char '{') (char '}') idParser)
     return(RelationDeclFrag (RelationDecl cardValue name defaultText sourceClass targetClass))
--}

declBaseRelationP = 
  do try (string "DeclRel") <|> (try (string "RelationName")) 
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultText <- try(between (char '{') (char '}') idParser)
--     spaces
--     super <- choice ((try(string "{}")):(try(between (char '{') (char '}') idParser)):[]) 
--     superRel <- if (super == "{}") 
--                    then return(Nothing)
--                    else return(Just(super))
     return(BaseRelationDeclFrag (BaseRelationDecl name defaultText))

declRelTypeP = 
  do try (string "Relation")
     card <- option [] (parenthesed False '[' ']')
     cardValue <- if (card == []) 
                    then return(Nothing)
                    else return(Just(card))
     spaces
     name <- try(between (char '{') (char '}') idParser)
     spaces
     source <- try(between (char '{') (char '}') idParser)
     spaces     
     target <- try(between (char '{') (char '}') idParser)
     spaces
     super <- choice ((try(string "{}")):(try(between (char '{') (char '}') idParser)):[]) 
     superRelation <- if (super == "{}") 
                        then return(Nothing)
                        else return(Just(super))
     return(RelationTypeDeclFrag (RelationTypeDecl cardValue name source target superRelation))

objRelationP = 
  do try (string "Relate")
     name <- try(between (char '{') (char '}') idParser)
     spaces
     source <- try(between (char '{') (char '}') idParser)
     spaces
     target <- try(between (char '{') (char '}') idParser)
     return(ObjectLinkFrag (ObjectLink source target name))

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

anyWithoutThisParens :: String -> String -> GenParser Char st String
anyWithoutThisParens parSymbols inStr = 
  do s <- try (escapedBracket)
     anyWithoutThisParens parSymbols (inStr ++ s) 
  <|> do char '\\'
         anyWithoutThisParens parSymbols (inStr ++ "\\")
  <|> do s <- many1 (noneOf ("\\" ++ parSymbols))
         anyWithoutThisParens parSymbols (inStr ++ s)
  <|> return inStr

escapedBracket :: GenParser Char st String
escapedBracket = do try (char '\\')
                    c <- try (oneOf "([{}])")
                    return ("\\" ++ [c]) 


{--
objLinkP :: [String] -> GenParser Char st OFrag
objLinkP rels = 
  try(
   do possibleRel <- try (many1 (noneOf "{\\\n"))
      if (possibleRel `elem` rels)
        then do sourceClass <- try(between (char '{') (char '}') idParser)
                spaces
                targetClass <- try(between (char '{') (char '}') idParser)
                return(ObjectLinkFrag (ObjectLink sourceClass targetClass possibleRel))
        else fail ""
  )
--}

idParser :: GenParser Char st String
idParser = many (noneOf "{}[]")


