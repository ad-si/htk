module AbstractGraphView where

{- Interface for graph viewing and abstraction.
   It is possible to hide sets of nodes and edges.
   Using a composition table for edge types,
   paths through hidden nodes can be displayed.
   Graphs, nodes, and edges are handled via
   descriptors (here: integers), while node and
   edge types are handled by user-supplied strings.
   
   todo:
   AbstractionGraph erweitern
   Funktionen anpassen bzgl. erweiteren edges + nodes
   neue Funktionen implementieren
   Testen mit LispInterface, Daten aus test.data

   Evtl. noch Aktionen "Benutzer klickt auf leere
   Fläche bzw. fügt neue Kante ein" vorsehen?
-}

import DaVinciGraph

import GraphDisp
import GraphConfigure

import Destructible


-- Which graph display tool to be used, perhaps make it more tool independent?
graphtool = daVinciSort

type OurGraph = 
     Graph   DaVinciGraph
             DaVinciGraphParms
             DaVinciNode
             DaVinciNodeType
             DaVinciNodeTypeParms
             DaVinciArc
             DaVinciArcType
             DaVinciArcTypeParms

-- Main datastructure for carrying around the graph,
-- both internally (nodes as integers), and at the daVinci level

type CompTable = [(String,String,String)]
data AbstractionGraph = AbstractionGraph {
       theGraph :: OurGraph, -- (DaVinciNode,String) () (DaVinciEdge,String) (),
       nodeTypes :: [(String,DaVinciNodeType (String,Int))],
       edgeTypes :: [(String,DaVinciArcType (String,Int))],
       nodes :: [(Int,(String,DaVinciNode (String,Int)))], -- id, daVinci
       edges :: [(Int,(Int,Int,String,DaVinciArc (String,Int)))], -- id, src, tar, type, daVinci
       -- probably, also the abstracted graph needs to be stored,
       -- and a list of hide/abstract events with the hidden nodes/edges (for each event),
       -- which is used to restore things when show_it is called
       edgeComp :: CompTable }

type Descr = Int
type GraphInfo = ([(Descr,AbstractionGraph)],Descr) -- for each graph the descriptor and the graph,
                                                    -- plus a global counter for new descriptors
data Result = Result GraphInfo                      -- the new graph list
                     Descr                          -- graph, node or edge descriptor
                     (Maybe String)                 -- a possible error message


-- lookup tables and failure handling

remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove x l = filter (\(y,_) -> not (x==y)) l

return_fail graphs msg =
  return (Result graphs 0 (Just msg))

-- lookup a graph descriptor and execute a command on the graph
-- the delete flag specifies if the graph should be removed from the graph list afterwards
-- fetch_graph :: Descr -> GraphInfo -> Bool -> a ?
fetch_graph gid (gs,ev_cnt) delete cmd =
  case lookup gid gs of
    Just g -> do (g',descr,err) <- cmd g
                 let gs'' = if delete then gs' else (gid,g'):gs'
                 return (Result (gs'',ev_cnt+1) descr err)
                 where gs' = remove gid gs
    Nothing -> return (Result (gs,ev_cnt) 0 (Just ("Graph id "++show gid++" not found")))


-- These are the operations of the interface

makegraph :: String -> [GlobalMenu] -> 
             [(String,DaVinciNodeTypeParms (String,Descr))] -> 
             [(String,DaVinciArcTypeParms (String,Descr))] ->
             CompTable -> GraphInfo -> IO Result 
makegraph title menus nodetypeparams edgetypeparams comptable (gs,ev_cnt) = do
  let graphParms  = 
       foldr ($$) (GraphTitle title $$
                   OptimiseLayout True $$ 
	           emptyGraphParms)
                   menus 
      (nodetypenames,nodetypeparams1) = unzip nodetypeparams
      (edgetypenames,edgetypeparams1) = unzip edgetypeparams
  graph <- GraphDisp.newGraph graphtool graphParms
  nodetypes <- sequence (map (newNodeType graph) nodetypeparams1)
  edgetypes <- sequence (map (newArcType graph) edgetypeparams1)
  let g = AbstractionGraph {
            theGraph = graph,
            nodeTypes = zip nodetypenames nodetypes,
            edgeTypes = zip edgetypenames edgetypes,
            nodes = [], 
            edges = [], 
            edgeComp = comptable }
  return (Result ((ev_cnt,g):gs,ev_cnt+1) ev_cnt Nothing)

delgraph :: Descr -> GraphInfo -> IO Result
delgraph gid (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) True
   (\g -> do destroy (theGraph g)
             return (g,0,Nothing))

addnode :: Descr -> String -> String -> GraphInfo -> IO Result
addnode gid nodetype name (gs,ev_cnt) = 
  fetch_graph gid (gs,ev_cnt) False (\g -> 
   do case lookup nodetype (nodeTypes g) of
       Nothing -> return (g,0,Just ("addnode: illegal node type: "++nodetype))
       Just nt -> do
        node <- newNode (theGraph g) nt (name,ev_cnt)
        return (g{nodes = (ev_cnt,node):nodes g},ev_cnt,Nothing)
   )

delnode :: Descr -> Descr -> GraphInfo -> IO Result
delnode gid node (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
      case lookup node (nodes g) of
        Just n -> do deleteNode (theGraph g) n
                     return (g{nodes = remove node (nodes g)},0,Nothing)
        Nothing ->  return (g,0,Just ("delnode: illegal node: "++show node))
    )
{-
changenodetype
unclear how to implement, ask George
-}

addlink :: Descr -> String -> String -> Descr -> Descr -> GraphInfo -> IO Result
addlink gid edgetype name src tar (gs,ev_cnt) = 
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case (lookup edgetype (edgeTypes g),
          lookup src (nodes g),
          lookup tar (nodes g)) of
    (Just et, Just src_node, Just tar_node) -> do
       edge <- newArc (theGraph g) et (name,ev_cnt) src_node tar_node
       return (g{edges = (ev_cnt,(src,tar,edge)):edges g},ev_cnt,Nothing)
    (Nothing,_,_) -> return (g,0,Just ("addlink: illegal edge type: "++edgetype))
    (_,Nothing,_) -> return (g,0,Just ("addlink: illegal source node id: "++show src))
    (_,_,Nothing) -> return (g,0,Just ("addlink: illegal target node id: "++show tar))
   )


dellink :: Descr -> Descr -> GraphInfo -> IO Result
dellink gid edge (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case lookup edge (edges g) of
        Just (_,_,e) -> 
           do deleteArc (theGraph g) e
              return (g{edges = remove edge (edges g)},0,Nothing)
        Nothing -> return (g,0,Just ("dellink: illegal edge: "++show edge))
   )

redisplay :: Descr -> GraphInfo -> IO Result
redisplay gid (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    do redraw (theGraph g)
       return (g,0,Nothing)
    )
hidenodetype :: Descr -> String -> GraphInfo -> IO Result

hidenodes :: Descr -> [Descr] -> GraphInfo -> IO Result
hidenodes gid node_list (gs,ev_cnt) =
{- Baustelle1:
fuer jeden Knoten:
- reinfuehrende edges finden, rausfuehrende edges finden
- jede der ersteren mit jeder der letzteren zu einer edge verschmelzen
  (mit source der reinfuehrenden, target der rausfuehrenden, typ wie in edgeComp angegeben)
  wenn nichts in der edgeComp-Tabelle drin ist, dann keine Kante erzeugen
- doppelte edges vom gleichen Typ entfernen. (evtl. erst spaeter implementieren)


- mit fetch_graph den Graphen raussuchen
- mit map auf die Liste der zu versteckenden Knoten folgendes ausfuehren (geht das so mit map?):
- den Knoten aus dem Graphen entfernen, aber in nodes im labbeled record lassen
- ein Listentupel aus den reingehenden und rausgehenden Edges machen

-> wie kriege ich deren Typen?

- Pfade bilden


fetch_graph gid (gs,ev_cnt) False (\g ->
   case sequence (map (\node -> lookup node (nodes g)) node_list) of
     Nothing -> fehler!!!
     Just nl ->  ...
   do l <- sequence (map (\node -> 
     do case lookup node (nodes g) of
       Just n -> do deleteNode (theGraph g) n
                    makePaths gid (fetch_edges_of_node gid (gs,ev_cnt) node) (gs,ev_cnt)
		    return (g,0,Nothing)
        Nothing -> return (g,0,Just ("hidenodes: illegal node: "++ show node))
     
       )

    node_list)
    return (g, ... l ....
   )


- Pfade bilden:
- eine reinfuehrende edge nehmen, mit allen rausgeheneden verschmelzen
- dasselbe mit den anderen reinfuehrenden edges wiederholen

-> warum ist der Rueckgabewert von lookup bei edges ein Tripel?


makePaths :: Descr -> ([Descr],[Descr]) -> GraphInfo -> IO Result
makePaths gid (inEdges, outEdges) (gs,ev_cnt)

-- wenn es keine rein- und/oder rausfuehrenden edges (mehr) gibt, sind wir fertig
  | inEdges == [] || outEdges == [] = do fetch_graph gid (gs,ev_cnt) False
                                      return (g,0,Nothing)
-- sonst "eigentliche" Arbeit				      
  | otherwise = do fetch_graph gid (gs,ev_cnt) False
                   let edge <- head inEdges
                   do case lookup edge (edges g) of
                       Just (_,_,e) -> makePathsAux gid edge outEdges (gs,ev_cnt)
		                       makePaths gid (tail inEdges, outEdges) (gs,ev_cnt)		       
			
                        Nothing -> return (g,0,Just ("dellink: illegal edge: "++show edge))
     

makePathsAux :: Descr -> Descr -> [Descr] -> GraphInfo -> IO Result
makePathsAux gid edge [] (gs,ev_cnt) = do fetch_graph gid (gs,ev_cnt) False
                                          return (g,0,Nothing)
makePathsAux gid edge edge_list (gs,ev_cnt) = do fetch_graph gid (gs,ev_cnt) False
                                                 do let oe <- head edge_list
						    case lookup oe (edges g) of
						     Just (_,_,oe) -> do addlink gid (determineEdgeType e oe) ##name## ##(lookup scr)## ##(lookup tar)## (gs,ev_cnt)
					                                 makePathsAux gid edge (tail edge_list) (gs,ev_cnt)
						     Nothing -> return (g,0,Just("makePaths: illegal edge: " ++show edge))

						      
  


-}
  undefined


-- like hidenodes, but replaces the hidden nodes by a new node
--  with a menu to unhide the nodes
abstractnodes :: Descr -> [Descr] -> GraphInfo -> IO Result
abstractnodes gid node_list (gs,ev_cnt) =
{- Baustelle:
Ueberlegung : ist es nicht sinnvoller, die zu versteckenden Knotentypen zu uebergeben?
Graph: fetch_graph gid (gs,ev_cnt) False do ...



(\g -> 
   do case lookup nodetype (nodeTypes g) of
       Nothing -> return (g,0,Just ("abstractnodes: illegal node type: "++nodetype))
       Just nt -> do
        node <- newNode (theGraph g) nt (name,ev_cnt)
        return (g{nodes = (ev_cnt,node):nodes g},ev_cnt,Nothing)
   )



makePaths :: Graph ->  [(Int,DaVinciNode (String,Int))] -> ...... -- [String] -> [(EdgeType,NodeType)]
makePaths gr [] et = []
makePaths gr tgt et = if (notElem tgt1 node_list) then
		         newTgt
		   else (makePaths gr (snd(findNode gr tgt1)) ((getEdgeType(fst(head tgt))):et) )++newTgt

  	where	tgt1 = snd(head tgt)
		newTgt = (determineEdgeType ((fst(head tgt)):(map read et)),snd(head tgt)):(makePaths gr (tail tgt) et)

-}
  undefined
hideedgetype :: Descr -> String -> GraphInfo -> IO Result

hideedges :: Descr -> [Descr] -> GraphInfo -> IO Result
hideedges gid edge_list (gs,ev_cnt) =
{- Baustelle3:
- edge aus ourGraph entfernen, aber in edges lassen
-}
  undefined

show_it :: Descr -> Descr -> GraphInfo -> IO Result
show_it gid hide_event (gs,ev_cnt) =
{- Baustelle4:
wiederherstellen: die angegebenen edges, nodes aus dem labbeled record wieder in den Graphen einfuegen
-}

  undefined




