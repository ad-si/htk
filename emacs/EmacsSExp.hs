{- Module for parsing XEmacs sexps. -}
module EmacsSExp (
   SExp(..),
   doParse,
   ) where

import Parsec
import qualified ParsecToken as P


-- datatype representing those bits of an sexp we are interested in.
data SExp =
      String String
   |  Integer Integer
   |  Id String
   |  List [SExp]
   |  DotList [SExp] SExp
   deriving (Show)

doParse :: String -> SExp
doParse input =
   case parse parseSExp "XEmacs output" input of
      Left errors ->
         error ("Parse errors:\n "++show errors++"\n in "++input)
      Right sexp -> sexp

parseSExp :: Parser SExp
parseSExp =
   do
      P.whiteSpace lexer
      parseSExp0

parseSExp0 :: Parser SExp
parseSExp0 = parseString <|> parseInteger <|> parseId <|> parseList

parseString :: Parser SExp 
parseString =
   do
      str <- P.stringLiteral lexer
      return (String str)


parseInteger :: Parser SExp 
parseInteger =
   do
      int <- P.integer lexer
      return (Integer int)

parseId :: Parser SExp 
parseId =
   do
      id <- P.identifier lexer
      return (Id id)


parseList :: Parser SExp 
parseList =
   P.parens lexer
      (do
          sexps <- sepBy parseSExp (P.whiteSpace lexer)
          (do
              P.dot lexer
              sexp <- parseSExp
              return (DotList sexps sexp)
            )
            <|> return (List sexps)
      )


-- -------------------------------------------------------------
-- Tokens
-- We use the ParsecToken module (imported as P).
-- -------------------------------------------------------------

lexer = P.makeTokenParser lispLanguageDef

lispLanguageDef =
   P.LanguageDef {
      P.commentStart = "",
      P.commentEnd = "",
      P.commentLine = ";",
      P.nestedComments = False,
      P.identStart = letter,
      P.identLetter = alphaNum <|> oneOf "-_",
      P.opStart = oneOf "'?",
      P.opLetter = alphaNum <|> oneOf "-_",
      P.reservedNames = [],
      P.reservedOpNames = [],
      P.caseSensitive = True
      }