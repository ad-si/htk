module AbstractGraphView where


import DaVinciGraph

import GraphDisp
import GraphConfigure

import Destructible


-- Which graph display tool to be used
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

data AbstractionGraph = AbstractionGraph {
       theGraph :: OurGraph, -- (DaVinciNode,String) () (DaVinciEdge,String) (),
       nodeTypes :: [(String,DaVinciNodeType (String,Int))],
       edgeTypes :: [(String,DaVinciArcType (String,Int))],
       nodes :: [(Int,DaVinciNode (String,Int))], -- id, daVinci
       edges :: [(Int,(Int,Int,DaVinciArc (String,Int)))], -- id, src, tar, daVinci
       -- probably, also the abstracted graph needs to be stored,
       -- and a list of hide/abstract events with the hidden nodes/edges (for each event),
       -- which is used to restore things when show_it is called
       edgeComp :: [(String,String,String)] }


data Result = Result ([(Int,AbstractionGraph)],Int) -- the new graph list
                     Int                            -- graph, node or edge descriptor
                     (Maybe String)                 -- a possible error message


-- lookup tables and failure handling

remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove x l = filter (\(y,_) -> not (x==y)) l

return_fail graphs msg =
  return (Result graphs 0 (Just msg))

fetch_graph gid (gs,ev_cnt) delete cmd =
  case lookup gid gs of
    Just g -> do (g',descr,err) <- cmd g
                 let gs'' = if delete then gs' else (gid,g'):gs'
                 return (Result (gs'',ev_cnt+1) descr err)
                 where gs' = remove gid gs
    Nothing -> return (Result (gs,ev_cnt) 0 (Just ("Graph id "++show gid++" not found")))

makegraph title menus nodetypeparams edgetypeparams comptable (gs,ev_cnt) = do
  let graphParms  = 
       foldr ($$) (GraphTitle title $$
                   OptimiseLayout True $$ 
	           emptyGraphParms)
                   (map GlobalMenu menus) 
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

delgraph gid (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) True
   (\g -> do destroy (theGraph g)
             return (g,0,Nothing))

addnode gid nodetype name (gs,ev_cnt) = 
  fetch_graph gid (gs,ev_cnt) False (\g -> 
   do case lookup nodetype (nodeTypes g) of
       Nothing -> return (g,0,Just ("addnode: illegal node type: "++nodetype))
       Just nt -> do
        node <- newNode (theGraph g) nt (name,ev_cnt)
        return (g{nodes = (ev_cnt,node):nodes g},ev_cnt,Nothing)
   )
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


dellink gid edge (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case lookup edge (edges g) of
        Just (_,_,e) -> 
           do deleteArc (theGraph g) e
              return (g{edges = remove edge (edges g)},0,Nothing)
        Nothing -> return (g,0,Just ("dellink: illegal edge: "++show edge))
   )

redisplay gid (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    do redraw (theGraph g)
       return (g,0,Nothing)
    )

hidenodes gid node_list (gs,ev_cnt) =
  undefined


abstractnodes gid node_list (gs,ev_cnt) =
  undefined

hideedges gid edge_list (gs,ev_cnt) =
  undefined


show_it gid hide_event (gs,ev_cnt) =
  undefined




