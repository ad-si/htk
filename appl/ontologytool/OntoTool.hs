{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where


import IO
import System
import List

import Computation
import AtomString
import OntoParser
import MMiSSOntology

-- import WBFiles

-- import CodedValue

main =
   do args <- System.getArgs
      if ((length (elemIndices "--help" args)) > 0)
        then do putStr "Tool for checking and converting MMiSS ontologies"
		putStr "usage:\n  ontotool [OPTIONS] INPUTFILE [> OUTFILE]\n"
		putStr "Options are:\n"
		exitWith ExitSucess
        else done
      if ((length args) == 0 
        then fail "You must specify a ontology file!"
        else let fileName = last args
      case fromWithError weOntology of
         Left message -> let str = "The following errors occured during parsing:\n" 
                         in error (str ++ message)
         Right o -> let messages = isComplete o
                    in if (messages == [])
                         then hPutStr stderr "Parse: Successfull\nChecking Ontology: Successfull\n" 
                         else hPutStr stderr (unlines messages)
      exit

