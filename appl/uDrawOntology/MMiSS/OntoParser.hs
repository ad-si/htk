{-- OntoParser stellt eine Funktion zum Parsen von mmisslatex-Ontologiebefehlen
    in LaTeX-Files (oder anderen Textfiles) zur Verfgung.
--}

module MMiSS.OntoParser (

  parseMMiSSOntologyFile  -- SourceName -> IO(WithError (MMiSSOntology, [String]))
  {-- Main function that takes a filename as argument and parses that file
      for mmisslatex ontology commands. If it succeeds gives back the whole ontology structure
      along with a list of warnings (if class, object or relation declarations are missing).
  --}

)

where

import qualified MMiSS.MMiSSOntology as MOnto
import Computation hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Maybe

type Other = String

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
} deriving(Show)

data Frag =
   ClassDeclFrag ClassDecl
 | ObjectDeclFrag ObjectDecl
 | BaseRelationDeclFrag BaseRelationDecl
 | RelationTypeDeclFrag RelationTypeDecl
 | ObjectLinkFrag ObjectLink
 | OtherFrag Other deriving(Show)


parseMMiSSOntologyFile :: SourceName -> IO(WithError MOnto.MMiSSOntology)
parseMMiSSOntologyFile s =
 do peFs <- parseFromFile (ontoDoc [] []) s
    case peFs of
      Right fs -> return (generateOntology (MOnto.emptyMMiSSOntology "Test" MOnto.AutoInsert) fs)
      Left err -> return (hasError (show err))

{-- ontoDoc ist die Hauptparser-Funktion. Jedes erkannte Fragment wird direkt verarbeitet und in
    Ontologie-Datenstruktur aufgenommen. Relationen werden so beim Parsen aufgesammelt und dem
    Parser fr Objekt-Links zur Verfgung gestellt.
--}

ontoDoc :: [String] -> [Frag] -> GenParser Char st [Frag]
ontoDoc rels fs =
  do eof
     return(fs)
  <|> do f <- (frag rels) <?> "Fragment"
         ontoDoc rels (fs ++ [f])


generateOntology :: MOnto.MMiSSOntology -> [Frag] -> WithError (MOnto.MMiSSOntology)

generateOntology onto [] = hasValue(onto)

generateOntology onto (f:fs) =
  let weOnto = case f of
                  {-- todo: handle packages and importMode --}
                ClassDeclFrag (ClassDecl name defaultText super) ->
                  MOnto.insertClass onto name defaultText (maybeToList super) Nothing Nothing Nothing
                  {-- todo: handle packages and importMode --}
                ObjectDeclFrag (ObjectDecl name defaultText instanceOf) ->
                  MOnto.insertObject onto name defaultText instanceOf Nothing Nothing

                BaseRelationDeclFrag (BaseRelationDecl name defaultText) ->
                  MOnto.insertBaseRelation onto name defaultText

                RelationTypeDeclFrag (RelationTypeDecl card name source target superRel) ->
                  MOnto.insertRelationType onto name source target superRel card

                ObjectLinkFrag (ObjectLink source target name) ->
                  MOnto.insertLink onto source target name

                otherwise -> hasValue(onto)

  in case fromWithError weOnto of
       Left err -> weOnto
       Right o -> generateOntology o fs


-- frag ist der eigentliche Parser fr Fragmente. Als ersten Parameter bekommt er eine Liste der
-- bisher beim Parsen aufgesammelten Relationsnamen bergeben, damit der Link-Parser (aufgerufen vom
-- ontologyElement-Parser) die Makros, die einem Relationsnamen entsprechen als Links erkennen kann.

frag :: [String] -> GenParser Char st Frag
frag rels =
       comment
       <|> do backslash
              (try (ontologyElement rels)) <|> escapedChar <|> return(OtherFrag "\\")
       <|> do eof
              return(OtherFrag "")
       <|> other

backslash :: GenParser Char st Char
backslash = char '\\'

escapedChar :: GenParser Char st Frag
escapedChar = do c <- anyChar
                 return (OtherFrag [c])

other :: GenParser Char st Frag
other = -- do str <- manyTill anyChar (try(oneOf "\\%\n"))
         do  str <- (many1 (noneOf "\\%"))
             return(OtherFrag str)

comment :: GenParser Char st Frag
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


ontologyElement :: [String] -> GenParser Char st Frag
ontologyElement rels = declClassP <|> declObjectP -- <|> declRelationP
                       <|> declBaseRelationP <|> declRelTypeP
                       <|> objRelationP


declClassP :: GenParser Char st Frag

declClassP =
  do try (string "DeclClass") <|> (try (string "Class"))
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultTextOpt <- choice ((try(string "{}")):(try(between (char '{') (char '}') (value ""))):[])
     defaultText <- case defaultTextOpt of
                      "{}" -> return("")
                      str -> return(str)
--     defaultText <- (try (between (char '{') (char '}') (value "")))
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

declBaseRelationP =
  do try (string "DeclRel") <|> (try (string "RelationName"))
     name <- try(between (char '{') (char '}') idParser)
     spaces
     defaultText <- try(between (char '{') (char '}') idParser)
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


idParser :: GenParser Char st String
idParser = many (noneOf "{}[]")


