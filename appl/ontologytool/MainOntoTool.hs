{- Module which tests parsing an MMiSS document file (given as input),
   verifying it, structuring it, converting it to CodedValue and converting it
   back again. -}
module Main(main) where


import IO
import System
import List

-- import Computation
-- import AtomString
import OntoParser
import MMiSSOntology
import MMiSSOntologyGraph
import Data.Graph.Inductive.Graph

-- import Events
-- import Destructible
-- import InfoBus

import HTk
-- import SimpleForm
-- import DialogWin

main :: IO()
main =
   do args <- System.getArgs
      if ((length (elemIndices "--help" args)) > 0)
        then do putStr "Tool for checking and converting MMiSS ontologies"
		putStr "usage:\n  ontotool [OPTIONS] [STARTNODENAME] INPUTFILE\n"
		putStr "Options are:\n"
		putStr " -owl     : print out OWL representation\n"
		putStr " -daVinci : start daVinci and show ontology as graph\n"
		exitWith ExitSuccess
        else done
      filename <- if ((length args) == 0) 
                    then do putStr "Tool for checking and converting MMiSS ontologies"
		            putStr "usage:\n  ontotool [OPTIONS] [STARTNODENAME] INPUTFILE\n"
		            putStr "Options are:\n"
		            putStr " -owl     : print out OWL representation\n"
		            putStr " -daVinci : start daVinci and show ontology as graph\n"
                            exitWith (ExitFailure 1)
                    else return (last args)
      startNodeName <- if ((length args) > 2)
                         then return(Just(head (drop 1 (reverse args))))
                         else return(Nothing)
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
               then let g = addObjectsForGraph onto (getPureClassGraph (getClassGraph onto))
                        str = (graphviz  (revdir g)
                                         (getOntologyName onto) 
                                         (8.5,11.0) 
                                         (1,1) 
                                         Landscape 
                                         (graphvizNodeAtts onto)
                                         (graphvizEdgeAtts onto))
                    in do putStr str 
                          done
        else done
      if ((length (elemIndices "-daVinci" args)) > 0)
        then do displayClassGraph onto startNodeName
                getLine
                done
        else done


revdir :: DynGraph gr => gr a b -> gr a b
revdir = gmap (\(p,v,l,s)-> (s,v,l,p))
