-- | DrawDepend.hs parses a .depend file and displays its contents!
module DrawDepend(
  drawDepend
  ) where

import Data.List
import Data.Maybe
import IO
import System

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util.Debug
import Util.Computation
import Util.IOExtras
import Util.Dynamics(fromDynamic)

import Util.RegularExpression
import Reactor.InfoBus
import Events.Events
import Events.Destructible
import Util.WBFiles

import Graphs.GraphDisp
import Graphs.GraphConfigure

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

data ParsedDepend = ParsedDepend (Map.Map String [String])
-- Map from file to its dependencies.

data SemiParsedDepend = SemiParsedDepend [(String,String)]
-- Each pair is a dependency.

parseDepend :: IO ParsedDepend
parseDepend =
   do
      let
         programArguments =
            ProgramArgument {
               optionName = "prune",
               optionHelp = "flag to prune non-essential dependencies",
               defaultVal = Just (BoolValue False),
               argType = BOOL
               } : usualProgramArguments

      parseTheseArguments programArguments

      (Just doPrune) <- getArgBool "prune"

      depend <- openFile ".depend" ReadMode
      semiParsedDepend <- semiParseDepend depend
      let
         filteredDepend =
            if doPrune
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
                     Map.findWithDefault map [] importer
                  map2 = Map.insert map importer (imported:importedSoFar)
                  -- add imported to map if not there.
                  map3 =
                     case Map.lookup map2 imported of
                        Nothing ->
                            Map.insert map2 imported []
                        Just _ -> map2
               in
                  map3
               )
            Map.empty
            dependencies
   in
      ParsedDepend finiteMap

--------------------------------------------------------------------------
-- Drawing the graph
--------------------------------------------------------------------------

drawDepend ::
   (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
    HasConfig GraphTitle graphParms,
    HasConfig GraphGesture graphParms,
    HasConfig AllowDragging graphParms,
    HasConfig SurveyView graphParms,
    HasConfigValue ValueTitle nodeTypeParms,
    HasConfigValue LocalMenu nodeTypeParms,
    HasConfigValue NodeGesture nodeTypeParms,
    HasConfigValue NodeDragAndDrop nodeTypeParms
    )
   => (Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> IO ()
drawDepend (displaySort :: Graph graph graphParms node nodeType nodeTypeParms
   arc arcType arcTypeParms) =
   do
      (ParsedDepend dependMap) <- parseDepend
      let
         (dependencies :: [(String,[String])]) = Map.toList dependMap

      let
         graphParms =
            GraphGesture (putStrLn "New Node Gesture") $$
            AllowDragging True $$
            SurveyView True $$
            GraphTitle "Haskell Dependencies" $$
               emptyGraphParms

         nodeTypeParms =
            NodeGesture (\ title ->
               putStrLn ("New Node-And-Edge gesture on "++title)) $$$
            NodeDragAndDrop (\ otherDyn this ->
               do
                  let Just other = fromDynamic otherDyn
                  putStrLn ("Dragged "++other++" to "++this)
               ) $$$
            ValueTitle (\ title -> return title ) $$$
            LocalMenu (Button "Type1"
                  (\ title -> putStrLn title)) $$$
               emptyNodeTypeParms

      graph <- newGraph displaySort graphParms
      nodeType  <- newNodeType graph nodeTypeParms
      arcType <- newArcType graph emptyArcTypeParms

      (nodes :: [(String,node String)]) <-
         mapM
            (\ (importer,_) ->
               do
                  node <- newNode graph nodeType importer
                  return (importer,node)
               )
            dependencies
      let
         nodeMap = Map.fromList nodes
         stringToNode nodeString =
            case Map.lookup nodeMap nodeString of
               Just node -> node

      sequence_ (fmap
         (\ (importer,allImported) ->
            let
               importerNode = stringToNode importer
            in
               sequence_
                  (fmap
                     (\ imported ->
                        newArc graph arcType ()
                           importerNode (stringToNode imported)
                        )
                     allImported
                     )
            )
         dependencies
         )

      redraw graph

      sync (destroyed graph)

