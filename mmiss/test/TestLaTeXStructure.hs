{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where

import IO
import System

import Util.Computation

import MMiSS.LaTeX.Parser

import MMiSS.DTD
import MMiSS.EditXml
import MMiSS.FileSystemExamples

main :: IO ()
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

      elEither <- parseMMiSSLatex fileSystem fileName True
      el <- case  fromWithError elEither of
         Left str -> ioError (userError str)
         Right (el,_) -> return el
      putStr (toExportableXml el)

      let verified = validateElement docType el
      case verified of
         [] -> done
         errors -> error (unlines errors)
