{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

-- #include "config.h"

import IO

import Computation
--import AtomString

--import CodedValue

--import LaTeXParser
import LaTeXParserCore
-- import EmacsContent

--import Text.XML.HaXml.Types
--import Text.XML.HaXml.Parse
--import Text.XML.HaXml.Pretty
-- import MMiSSContent
-- import MMiSSDTD
-- import MMiSSObjects
--import MMiSSEditXml
import Text.ParserCombinators.Parsec


main =
   do
      doc <- getContents
      let result = parseFrags doc
      str <- case result of
--               Left err -> ioError (userError (concat (map messageString (errorMessages(err)))))
               Left err -> return(show err)
               Right fs -> return(show (Env "Root" (LParams [] [] Nothing Nothing) fs))
      putStrLn str


{-
      let verified = validateElement "package" el
      case verified of
         [] -> done
         errors -> error (unlines errors)
      let
         unparsedWE = makeMMiSSLatex (el,False)
         unparsed = coerceWithError unparsedWE

      putStrLn (mkLaTeXString unparsed)         
      -}
