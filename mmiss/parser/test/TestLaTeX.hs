{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where


import IO
import System

-- import Pretty

import Computation
import AtomString

import CodedValue

import LaTeXParser
import EmacsContent
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty

import MMiSSContent
import MMiSSDTD

-- import MMiSSEditXml

main =
   do
      hSetBuffering stdout NoBuffering
      args <- System.getArgs
      let
         expected = case args of
            [] -> "package"
            [arg] -> arg

      doc <- getContents
      --
      -- Parse LaTeX input
      --
      let elEither = parseMMiSSLatex doc
      (el, preamble) <- case fromWithError elEither of
                          Left str -> ioError (userError str)
                          Right a -> return a
      --
      -- validate the XML-Element 
      --
      let verified = validateElement expected el
      case verified of
         [] -> done
         errors -> do putStr(render (element el))
                      error (unlines errors)
      --
      -- Reconstruct LaTeX from XML-Element and Preamble 
      --
      let emptyPreambleData = MMiSSExtraPreambleData {callSite = Nothing}
          (emacsCont, preambleStr) = 
            case preamble of
              Nothing -> (makeMMiSSLatex (el, True, []), "")
              (Just(a)) -> (makeMMiSSLatex (el, True, [(a,[emptyPreambleData])]), (toString a))               
          (EmacsContent l) = coerceWithError emacsCont
      putStr ("\n**************** XML:\n" ++
--                     (toExportableXml el) ++
                     (render (element el)) ++ 
                     "\n**************** Reconstructed LaTeX:\n" ++
                     (concat (map getStrOfEmacsDataItem l)) ++ 
                     "\n\n************** Preamble:\n" ++ preambleStr ++ 
                     "\nMMiSSLaTeX source parsing and validation was successfull!\n")


getStrOfEmacsDataItem :: EmacsDataItem ((String, Char), [Attribute]) -> String

getStrOfEmacsDataItem (EditableText str) = str
getStrOfEmacsDataItem (EmacsLink ((str,c), _)) = str ++ [c]                                   

