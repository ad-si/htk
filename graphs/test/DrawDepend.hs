{- DrawDepend.hs parses a .depend file and displays its contents! -}
module DrawDepend(
  drawDepend
  ) where

import List
import Maybe
import IO
import System

import Set
import FiniteMap

import Debug
import Computation
import IOExtras

import RegularExpression
import InfoBus
import SIM(Destructible(..),sync)

import GraphDisp

import Hasse

--------------------------------------------------------------------------
-- Parsing the .depend file.
-- We ignore lines 
-- (a) which don't look like .depend entries (so the comment lines
--     at the beginning and end;
-- (b) the imported element of which does not begin with a /;
-- (c) entries where the file depended on does not appear on the
--     left hand side of a rule.
--------------------------------------------------------------------------

data ParsedDepend = ParsedDepend (FiniteMap String [String])
-- Map from file to its dependencies.

data SemiParsedDepend = SemiParsedDepend [(String,String)]
-- Each pair is a dependency.

parseDepend :: IO ParsedDepend
parseDepend =
   do
      args <- getArgs
   
      depend <- openFile ".depend" ReadMode
      semiParsedDepend <- semiParseDepend depend
      let
         doFilter = isJust(find (== "h") args)
         filteredDepend =
            if doFilter
               then
                  filterDepend semiParsedDepend
               else
                  semiParsedDepend
         parsedDepend = finishParseDepend filteredDepend
      hClose depend
      return parsedDepend


semiParseDepend :: Handle -> IO SemiParsedDepend
semiParseDepend handle = semiParseDependAcc handle []

dependRegEx :: RegularExpression
dependRegEx = compile "\\`(.*)\\.o : (.*)\\.hi\\'"

semiParseDependAcc :: Handle -> [(String,String)] -> IO SemiParsedDepend
semiParseDependAcc handle soFar =
   do
      nextLineOpt <- catchEOF (hGetLine handle)
      case nextLineOpt of
         Nothing -> return (SemiParsedDepend soFar)
         Just line ->
            let
               continue = semiParseDependAcc handle soFar
            in
               case matchString dependRegEx line of
                  Nothing -> continue
                  Just matchResult -> 
                     case getSubStrings matchResult of
                        importer:imported1:_ ->
                           let
                              imp imported =
                                 semiParseDependAcc handle 
                                    ((importer,imported):soFar)
                           in
                              case imported1 of
                                 '.':'/':imported -> imp imported
                                 '/':imported -> continue
                                 imported -> imp imported

filterDepend :: SemiParsedDepend -> SemiParsedDepend
filterDepend (SemiParsedDepend dependencies) = 
   SemiParsedDepend (hasse dependencies)

finishParseDepend :: SemiParsedDepend -> ParsedDepend
finishParseDepend (SemiParsedDepend dependencies) =
   let
      finiteMap =
         foldr
            (\ (importer,imported) map ->
               let
                  importedSoFar =
                     lookupWithDefaultFM map [] importer
                  map2 = addToFM map importer (imported:importedSoFar)
                  -- add imported to map if not there.
                  map3 =
                     case lookupFM map2 imported of
                        Nothing ->
                            addToFM map2 imported []
                        Just _ -> map2
               in
                  map3
               )
            emptyFM
            dependencies
   in
      ParsedDepend finiteMap

--------------------------------------------------------------------------
-- Drawing the graph
--------------------------------------------------------------------------

drawDepend :: 
   (GraphAll graph graphParms node nodeType nodeTypeParms 
      arc arcType arcTypeParms,
    GraphConfigParms GraphTitle graphParms,
    NodeTypeConfigParms ValueTitle nodeTypeParms,
    NodeTypeConfigParms LocalMenu nodeTypeParms
    ) 
   => (graph,graphParms,
      node Int,nodeType Int,nodeTypeParms Int,
      arc String Int Int,arcType String,arcTypeParms String) 
   -> IO ()
drawDepend (_::
   (graph,graphParms,
      node Int,nodeType Int,nodeTypeParms Int,
      arc String Int Int,arcType String,arcTypeParms String) 
      ) =
   do
      (ParsedDepend dependMap) <- parseDepend
      let
         (dependencies :: [(String,[String])]) = fmToList dependMap

      let
         nullGraphParms = emptyGraphParms :: graphParms
         graphParms =
            (graphConfig (GraphTitle "Haskell Dependencies")) $
               nullGraphParms

         (nullNodeParms :: nodeTypeParms String) = emptyNodeTypeParms
         nodeTypeParms =
            (nodeTypeConfig (ValueTitle (\ title -> return title ))) .
            (nodeTypeConfig (LocalMenu (Button "Type1" 
                  (\ title -> putStrLn title))))
               $ nullNodeParms
   
         (nullArcParms :: arcTypeParms ()) = emptyArcTypeParms

      (graph::graph) <- newGraph graphParms
      (nodeType :: nodeType String) <- newNodeType graph nodeTypeParms
      (arcType :: arcType ()) <- newArcType graph nullArcParms

      (nodes :: [(String,node String)]) <-
         mapM
            (\ (importer,_) ->
               do
                  node <- newNode nodeType graph importer
                  return (importer,node)
               )
            dependencies
      let
         nodeMap = listToFM nodes
         stringToNode nodeString =
            case lookupFM nodeMap nodeString of
               Just node -> node

      sequence_ (map
         (\ (importer,allImported) ->
            let
               importerNode = stringToNode importer
            in
               sequence_
                  (map
                     (\ imported ->
                        (newArc arcType graph () 
                           importerNode (stringToNode imported)) :: 
                           IO (arc () String String)
                        ) 
                     allImported
                     )
            )
         dependencies
         )

      redraw graph

      sync (destroyed graph)

