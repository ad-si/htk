{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

#include "config.h"

import IO

import Pretty

import Computation
import AtomString

import CodedValue

import LaTeXParser

#if HAXMLINT
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
#else
import XmlTypes
import XmlParse
import XmlPP
#endif

import MMiSSContent
import MMiSSDTD
import MMiSSEditXml

main =
   do
      doc <- getContents
      let
         elEither = parseMMiSSLatex doc
      el <- case  fromWithError elEither of
         Left str -> ioError (userError str)
         Right (el,_,_) -> return el
      putStr (toExportableXml el)
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