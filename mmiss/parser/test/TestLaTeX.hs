{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where


import IO
import System
import List

-- import Pretty

import Computation
import AtomString

-- import CodedValue

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
         mbRoot = find (isPrefixOf "-root=") args 
         expected = case mbRoot of
                      (Just rootOpt) -> drop 6 rootOpt 
                      Nothing -> "package"
         xmlOutput   = if ((elemIndices "-xml" args) == []) then False else True
         latexOutput = if ((elemIndices "-latex" args) == []) then False else True
         latexWithPreOutput = if ((elemIndices "-preamble" args) == []) then False else True

      doc <- getContents
      --
      -- Parse LaTeX input
      --
      let elEither = parseMMiSSLatex doc
      (el, preamble) <- case fromWithError elEither of
                          Left message -> let str = "Parse: The following errors occured:\n\n" 
                                          in ioError (userError (message ++ str))
                          Right a -> return a
      --
      -- validate the XML-Element 
      --
      let verified = validateElement expected el
      case verified of
         [] -> done
         errors -> do putStr(render (element el))
                      let  
                         str1 = "\nParse: Successfull\n"
                         str2 = "Validate XML: The following errors occured:\n\n"
                      error (unlines ([str1] ++ [str2] ++ errors))
      --
      -- Reconstruct LaTeX from XML-Element and Preamble 
      --
      let emptyPreambleData = MMiSSExtraPreambleData {callSite = Nothing}
          (emacsCont, preambleStr) = 
            case preamble of
              Nothing -> (makeMMiSSLatex (el, True, []), "")
              (Just(a)) -> (makeMMiSSLatex (el, True, [(a,[emptyPreambleData])]), (toString a))               
          (EmacsContent l) = coerceWithError emacsCont
      if (xmlOutput == True) 
        then putStr (render (element el)) 
        else if (latexOutput)
               then putStr (concat (map getStrOfEmacsDataItem l))
               else if (latexWithPreOutput) 
                      then putStr ((concat (map getStrOfEmacsDataItem l)) 
                           ++ "\n\n************** Preamble:\n" ++ preambleStr)
                      else done 
      hPutStr stderr "\nParse: Successfull\nValidate XML: Successfull\n"


getStrOfEmacsDataItem :: EmacsDataItem ((String, Char), [Attribute]) -> String

getStrOfEmacsDataItem (EditableText str) = str
getStrOfEmacsDataItem (EmacsLink ((str,c), _)) = str ++ [c]                                   

