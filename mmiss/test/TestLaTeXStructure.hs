{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

import IO
import System

import Pretty

import Computation
import AtomString

import CodedValue

import LaTeXParser

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty

import MMiSSDTD
import MMiSSEditXml
import MMiSSFileSystemExamples

main =
   do
      args <- getArgs
      docType <- case args of
         [] -> return "package"
         [arg] -> return arg
         _ -> error "Must have at most one argument, the document type"

      doc <- getContents
      let
         fileName = "stdin"
         fileSystem = oneFileFileSystem fileName doc

      elEither <- parseMMiSSLatex fileSystem fileName
      el <- case  fromWithError elEither of
         Left str -> ioError (userError str)
         Right (el,_) -> return el
      putStr (toExportableXml el)

      let verified = validateElement docType el
      case verified of
         [] -> done
         errors -> error (unlines errors)
