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
import MMiSSOntologyGraph
import Data.Graph.Inductive.Graphviz

import Events
-- import Destructible
-- import InfoBus

import HTk
import SimpleForm
import DialogWin


main =
   do args <- System.getArgs
      if ((length (elemIndices "--help" args)) > 0)
        then do putStr "Tool for checking and converting MMiSS ontologies"
		putStr "usage:\n  ontotool [OPTIONS] INPUTFILE [> OUTFILE]\n"
		putStr "Options are:\n"
		exitWith ExitSuccess
        else done
      filename <- if ((length args) == 0) 
                    then do putStr "Tool for checking and converting MMiSS ontologies"
		            putStr "usage:\n  ontotool [OPTIONS] INPUTFILE [> OUTFILE]\n"
		            putStr "Options are:\n"
                            exitWith (ExitFailure 1)
                    else return (last args)

      weOntology <- parseMMiSSOntologyFile filename

      onto <- case fromWithError weOntology of
                Left message -> let str = "The following errors occured during parsing:\n" 
                                in error (str ++ message)
                Right o -> let messages = isComplete o
                           in if (messages == [])
                                then do hPutStr stderr "Parse: Successfull\nChecking Ontology: Successfull\n" 
                                        return o
                                else do hPutStr stderr (unlines messages)
					return o
--                                        exitWith (ExitFailure 2)

      if ((length (elemIndices "-owl" args)) > 0)
        then let str = (exportOWL onto)
             in do putStr str 
                   done
        else if ((length (elemIndices "-dot" args)) > 0)
               then let str = (graphviz' (getClassGraph onto))
                    in do putStr str 
                          done
        else done
      if ((length (elemIndices "-daVinci" args)) > 0)
        then do displayClassGraph onto
                getLine
                done
        else done