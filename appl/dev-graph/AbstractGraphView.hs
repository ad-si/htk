module AbstractGraphView where

{- Interface for graph viewing and abstraction.
   It is possible to hide sets of nodes and edges.
   Using a composition table for edge types,
   paths through hidden nodes can be displayed.
   Graphs, nodes, and edges are handled via
   descriptors (here: integers), while node and
   edge types are handled by user-supplied strings.
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
       nodes :: [(Int,DaVinciNode (String,Int))], -- id, daVinci
       edges :: [(Int,(Int,Int,DaVinciArc (String,Int)))], -- id, src, tar, daVinci
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

hidenodes :: Descr -> [Descr] -> GraphInfo -> IO Result
hidenodes gid node_list (gs,ev_cnt) =
  undefined

abstractnodes :: Descr -> [Descr] -> GraphInfo -> IO Result
abstractnodes gid node_list (gs,ev_cnt) =
  undefined

hideedges :: Descr -> [Descr] -> GraphInfo -> IO Result
hideedges gid edge_list (gs,ev_cnt) =
  undefined

show_it :: Descr -> Descr -> GraphInfo -> IO Result
show_it gid hide_event (gs,ev_cnt) =
  undefined




