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
-- import WBFiles

-- import CodedValue

import LaTeXParser
import LaTeXPreamble
import MMiSSFileSystemExamples
import EmacsContent
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Combinators hiding (find)

-- import MMiSSContent
import MMiSSDTD

-- import MMiSSEditXml

main =
   do hSetBuffering stdout NoBuffering
      args <- System.getArgs
      if ((length args) == 0)
        then do putStr "You must specify an input file. See testLaTeX --help for usage info\n"
		exitWith ExitSuccess
        else done
      fileName <- return(last args)
      if ((length (elemIndices "--help" args)) > 0)
        then do putStr "Structue checking tool for MMiSSLaTeX v0.1\n"
		putStr "usage:\n  testLaTeX [OPTIONS] INPUTFILE [> OUTFILE]\n"
		putStr "Options are:\n"
	        putStr "  -root=<element type>  (default: package)   Checker expects this element type as root of the MMiSS document.\n"
		putStr "                        This would be 'section' if you just have a section and no whole package in your input\n"
		putStr "  -xml                  Prints only the resulting XML tree\n"
		putStr "  -latex                Prints only the regenerated MMiSSLaTeX (parse-validate-regenerate cycle)\n"
		putStr "  -preamble             Prints the regenerated MMiSSLaTeX with document preamble -> ready for latex\n"
                putStr "  --uni-MMiSSDTD=<MMiSSDTD-File>  Sets location of the DTD file.\n"
		exitWith ExitSuccess
        else done
      let
         mbRoot = find (isPrefixOf "-root=") args 
         expected = case mbRoot of
                      (Just rootOpt) -> drop 6 rootOpt 
                      Nothing -> "package"
         xmlOutput   = if ((elemIndices "-xml" args) == []) then False else True
         latexOutput = if ((elemIndices "-latex" args) == []) then False else True
         latexWithPreOutput = if ((elemIndices "-preamble" args) == []) then False else True

--      doc <- getContents
      --
      -- Parse LaTeX input
      --
      elEither <- parseMMiSSLatex standardFileSystem fileName True 
      (el, preambleList) <- case fromWithError elEither of
                          Left message -> let str = "The following errors occured during parsing:\n" 
                                          in error (str ++ message)
                          Right a -> return a
      --
      -- validate the XML-Element 
      --
      let verified = validateElement expected el
      case verified of
         [] -> case (duplicateLabels el) of
                 [] -> done
                 l -> do if (xmlOutput == True) 
                           then putStr( "<?xml version='1.0' encoding='UTF-8'?>" ++ (toExportableXml el)) 
                           else done
                         let  
                           str1 = "\nParse: Successfull\nValidating: Successfull"
                           str2 = "The following Labels are duplicated:\n"
                           str3 = concat (map (++ " ") l)
                         error (unlines ([str1] ++ [str2] ++ [str3]))
         errors -> do if (xmlOutput == True) 
                        then putStr( "<?xml version='1.0' encoding='ISO-8859-1'?>\n<!DOCTYPE package SYSTEM 'file:///home/amahnke/uni/mmiss/MMiSS.dtd'>" ++ (toExportableXml el)) 
--                        then putStr( "<?xml version='1.0' encoding='ISO-8859-1'?>" ++ (render (element el))) 
                        else done
                      let  
                         str1 = "Parse: Successfull\n"
                         str2 = "The following errors occured during validation against the DTD:\n\n"
                      error (unlines ([str1] ++ [str2] ++ errors))
      --
      -- Reconstruct LaTeX from XML-Element and Preamble 
      --
      let packageIdContent = deep (iffind "packageId" returnPackageID none) (CElem el)
          packageId = case packageIdContent of
                        [] -> PackageId ""
                        [CString True str] -> PackageId str
                        otherwise -> PackageId ""
          -- emptyPreambleData = MMiSSExtraPreambleData {callSite = Nothing}
          (emacsCont, preambleStr) = 
            case preambleList of
              [] -> ((makeMMiSSLatex el True []), "")
              (p:ps) -> ((makeMMiSSLatex el True [((fst p),packageId)]), (toString (fst p)))               
          (EmacsContent l) = coerceWithError emacsCont
      if (xmlOutput == True) 
--        then putStr( "<?xml version='1.0' encoding='UTF-8'?>\n<!DOCTYPE package SYSTEM 'file:///home/amahnke/uni/mmiss/MMiSS.dtd'>" ++ (toExportableXml el)) 
        then putStr( "<?xml version='1.0' encoding='UTF-8'?>" ++ (toExportableXml el)) 
-- (render (element el))) 
        else if (latexOutput)
               then putStr (concat (map getStrOfEmacsDataItem l))
               else if (latexWithPreOutput) 
                      then putStr ((concat (map getStrOfEmacsDataItem l)) 
                           ++ "\n\n************** Preamble:\n" ++ preambleStr)
                      else done 
      hPutStr stderr "Parse: Successfull\nValidate XML: Successfull\n"
  where
    returnPackageID value _ = [CString True value]


duplicateLabels :: Element -> [String]

duplicateLabels e = 
  let contents = multi (attr "label") (CElem e)
      labels = filter (/= "") (map extractLabel contents)
  in nub(labels \\ (nub labels))
  where
    extractLabel (CElem(Elem _ attrs _)) = 
      let labelAtt = find ((== "label").fst) attrs
      in case labelAtt of
           Just((_, (AttValue []))) -> ""
           Just((_, (AttValue (v:[])))) -> case v of
                                             Left str -> str
                                             Right _ -> ""
           _ -> ""   

getStrOfEmacsDataItem :: EmacsDataItem ((String, Char), [Attribute]) -> String

getStrOfEmacsDataItem (EditableText str) = str
getStrOfEmacsDataItem (EmacsLink ((str,c), _)) = str ++ [c]                                   

