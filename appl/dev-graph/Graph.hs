module Graph (Graph, newGraph, fromGraph,
	EdgeType, NodeType, LocType, ThmType,
	mixedThmType, mixedLocType, mixedProofObl, 
	proofObligation, 
	addNode, isNode, removeNode, findNode, addNodeStr,
	addEdge, removeEdge, addEdgeStr,
        getNodeType, getEdgeType, isDefined,
	getNodeName,
	fromGraph, toGraph)
	where     

import Dynamics

locType_tyCon = mkTyCon "Graph" "LocType"
thmType_tyCon = mkTyCon "Graph" "ThmType"
nodeType_tyCon = mkTyCon "Graph" "NodeType"

instance HasTyCon LocType where
   tyCon _ = locType_tyCon

instance HasTyCon ThmType where
   tyCon _ = thmType_tyCon

instance HasTyCon NodeType where
   tyCon _ = nodeType_tyCon

data NodeType = Internal String | Defined String
                deriving(Show, Read, Eq)

data LocType = GlobalLink | LocalLink | MixedLocType
               deriving (Show, Read, Eq)

data ThmType = Axiom | Theorem | ProofObligation | MixedThmType | MixedProofObl
               deriving (Show, Read, Eq)

type EdgeType = (ThmType, LocType)

mixedLocType = MixedLocType
mixedThmType = MixedThmType
mixedProofObl = MixedProofObl
proofObligation = ProofObligation

newtype Graph = Graph [(NodeType, [(EdgeType, NodeType)])]
	deriving (Show,Eq)

newGraph = Graph []

-- creates a new node from two strings
newNode :: String -> String -> NodeType
newNode s2 s1
  | s1 == "INTERN"  = Internal s2
  | s1 == "DEFINED" = Defined s2
  | otherwise  = error "newNode: Unknown type of node"

-- adds a node to a graph,
-- but returns the old graph, if this node already exists
addNode :: Graph -> NodeType -> Graph
addNode (Graph gr) node = if (isNode (Graph gr) node) then (Graph gr) else (Graph ((node,[]):gr))

-- same as addNode, just with a different signature
addNodeStr :: Graph -> String -> String -> Graph
addNodeStr gr s1 s2 = addNode gr (newNode s1 s2)

-- finds a node in a graph and returns it together with the list of its edges
findNode :: Graph -> NodeType -> (NodeType,[(EdgeType,NodeType)])
findNode (Graph []) nd = error ("findNode: no such node"++(show nd))
findNode (Graph gr) nd = if (fst (head gr) == nd) then (head gr)
			   else findNode (Graph (tail gr)) nd

-- removes a node from a graph
removeNode :: Graph -> String -> String -> Graph
removeNode (Graph gr) s1 s2 = Graph (zip (map fst rem) (map (removeFromList node) (map snd rem)))
		where
		node = newNode s1 s2
		rem = [part| part <- gr, fst part /= node]
		removeFromList :: NodeType -> [(EdgeType,NodeType)] -> [(EdgeType,NodeType)]
		removeFromList node ls = [(a,b)|(a,b) <- ls, b /= node]

-- checks if a node is element of a graph
isNode ::  Graph -> NodeType-> Bool
isNode (Graph gr) node = elem node [fst n| n <- gr]
-- ||(elem node (map snd (concat[snd n|n<-gr] ) ))

-- creates a new edge from two strings
newEdge :: String -> String -> EdgeType
newEdge s1 s2 = (thm s1, lk s2)
		where
		thm :: String -> ThmType
		thm s
		  | s == "AXIOM"	= Axiom 
		  | s == "THEOREM"	= Theorem
		  | s == "PROOFOBLIGATION" = ProofObligation
		  | otherwise		= error ("newEdge: Unknown type of predicate ("++show(s)++")")
		lk :: String -> LocType
		lk l
		  | l == "GLOBAL"	= GlobalLink
		  | l == "LOCAL"	= LocalLink
		  | otherwise		= error ("newEdge: Unknown type of scope ("++show(l)++")")

-- adds an edge to a graph,
-- but returns the old graph, if this edge already exists
addEdge :: Graph -> NodeType -> NodeType -> EdgeType -> Graph
addEdge (Graph gr) source target edge =
        if ((isNode (Graph gr) source) && (isNode (Graph gr) target)) then
	   if (isEdge (Graph gr) source target edge) then (Graph gr)
	   else Graph ((source,concat[(edge, target):(snd n)| n <- gr, fst n == source]):[n| n <- gr, fst n /= source])
	else 
	  if(isNode (Graph gr) source) then
	    error ("addEdge applied to non-existent node ("++show(target)++")")
	  else
	    error ("addEdge applied to non-existent node ("++show(source)++")")

-- same as addEdge, just with a different signature
addEdgeStr :: Graph -> String -> String -> String -> String -> String -> String -> Graph
addEdgeStr gr src1 src2 tgt1 tgt2 th loc = addEdge gr (newNode src1 src2) (newNode tgt1 tgt2) (newEdge th loc)
	
-- removes an edge from a graph
removeEdge (Graph gr) src tgt edge =
	Graph ([(fst n, removeFromList edge tgt (snd n))|n <- gr, fst n == src]++[n| n <- gr, fst n /= src])
		where
--		src = newNode src1 src2
--		tgt = newNode tgt1 tgt2
--		edge = newEdge th loc
		removeFromList :: EdgeType -> NodeType -> [(EdgeType,NodeType)] -> [(EdgeType,NodeType)]
		removeFromList edge node ls = [(a,b)|(a,b) <- ls, (a,b) /= (edge,node)]

-- same as removeEdge, just with a different signature
removeEdgeStr :: Graph -> String -> String -> String -> String -> String -> String -> Graph
removeEdgeStr gr src1 src2 tgt1 tgt2 th loc = removeEdge gr src tgt edge
  where
       src = newNode src1 src2
       tgt = newNode tgt1 tgt2
       edge = newEdge th loc

-- checks, if an edge is part of a graph
isEdge :: Graph -> NodeType -> NodeType -> EdgeType -> Bool
isEdge gr src tgt ed = elem (ed,tgt) (snd(findNode gr src))


-- returns the type of a node as a string
getNodeType :: NodeType -> String
getNodeType node = case node of
   Internal _ -> "Internal"
   Defined _  -> "Defined"

-- checks, if a node is of type Defined
isDefined :: NodeType -> Bool
isDefined node = (getNodeType node == "Defined")
 

-- returns the type of an edge as a string
getEdgeType :: EdgeType -> String
getEdgeType ed = show ed

fromGraph (Graph gr) = gr

toGraph gr = Graph gr

-- returns the name of the node
getNodeName :: NodeType -> String
getNodeName nm = (words (show nm))!!1

