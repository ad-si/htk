{- Module for parsing XEmacs sexps. -}
module EmacsSExp (
   SExp(..),
   doParse,
   doParseBool,
   sexpToBool,
   doParseInt,
   ) where

import Char
import Maybe

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
   case parse parseSExp "XEmacs output" (doControlChars input) of
      Left errors ->
         error ("Parse errors:\n "++show errors++"\n in "++input)
      Right sexp -> sexp

doParseBool :: String -> Bool
doParseBool input =
   let
      sexp = doParse input
   in
      fromMaybe
         (error ("EmacsSExp: expecting Bool but found "++input))
         (sexpToBool sexp)

sexpToBool :: SExp -> Maybe Bool
sexpToBool sexp =
   case sexp of
      List [] -> Just False
      Id "t" -> Just True
      _ -> Nothing

doParseInt :: String -> Int
doParseInt input =
   let
      sexp = doParse input
   in
      case sexp of
         Integer i -> fromIntegral i
         _ -> error ("EmacsSExp: expecting Integer but found "++input) 

---
-- Because the ParsecToken module seems to have a problem with
-- control characters (with charcode < 32) in strings, we replace them
-- by their Haskell representation.  However since they don't occur
-- all that often, we first check if they actually occur.
doControlChars :: String -> String
doControlChars str =
   if any isControl str
      then
         foldr
            (\ ch str -> if isControl ch then showLitChar ch str else ch:str)
            ""
            str
      else
         str   

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

---
-- We special-case nil.
parseId :: Parser SExp 
parseId =
   do
      id <- P.identifier lexer
      return (if id == "nil" then List [] else Id id)


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