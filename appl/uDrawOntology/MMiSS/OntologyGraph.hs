{------------------------------------
MODULE        : MMiSS.OntologyGraph
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2006
VERSION       : 1.0
DESCRIPTION   : Provides functions to handle graph-commands for uDraw (Graph).
-------------------------------------}
module MMiSS.OntologyGraph (
  createGraph,
  updateGraphAttributes,
  updateGraph,
  filterObjects,
  filterRelations,
  filterRelationsNotX,
  getParents,
  getSiblings,
  getInRelationTo,
  getNodeName
)
where

import Data.List

import qualified UDraw.Util                 as UUtil
import qualified Data.Graph.Inductive       as Graph
import qualified MMiSS.MMiSSOntology        as MOnto
import qualified UDraw.Communication        as UDraw
import qualified Debug.Trace                as Debug
import qualified DaVinciTypes               as DVTypes
import qualified UDraw.OntoSideConfig       as OntoConf

--------------------------------------------
-- Build a complete graph from an ontology.
-- Return result as uDraw-Command.
--------------------------------------------
createGraph :: Graph.Gr (String, String, MOnto.OntoObjectType) String -> OntoConf.Config -> String
createGraph classGraph config =
  "graph(new_placed([" ++ (generateNodes classGraph (Graph.labNodes classGraph)) ++ "]))\n"
  where
    generateNodes _ []                                        = ""
    generateNodes classGraph ((nId,(className,_,nType)):rest) =
      (UDraw.createNode False nId className "" (retrieveOutgoingEdges classGraph nId) nType config) ++ (generateNodes classGraph rest)
    retrieveOutgoingEdges classGraph nId =
      map prepareEdges (Graph.out classGraph nId)
       where
       prepareEdges :: (Int, Int, String) -> (String, Int, Int)
       prepareEdges (startId, targetId, eType) = (eType, startId, targetId)

--------------------------------------------
-- Update a graph. Including adding and
-- removing nodes.
-- ToDo: funktion not finished
--------------------------------------------
updateGraph :: Graph.Gr (String, String, MOnto.OntoObjectType) String -> MOnto.MMiSSOntology ->
                Graph.Gr (String, String, MOnto.OntoObjectType) String -> OntoConf.Config -> String
updateGraph oldGraph onto newGraph config =
  let completeGraph  = MOnto.getClassGraph onto
      nodeChanges = getNodeChanges (Graph.nodes oldGraph) (zip (repeat False) (Graph.nodes newGraph))
      edgeChanges = getEdgeChanges (Graph.labEdges oldGraph)  (zipEdges (repeat False) (Graph.labEdges newGraph))
  in "graph(update([" ++ (nodeUpdates completeGraph onto nodeChanges config) ++ "],"++
      "[" ++ (edgeUpdates completeGraph onto edgeChanges config) ++ "]))\n"
  where
    zipEdges xs ys =
      zipWith pair xs ys
      where
      pair x (a,b,c) = (x,a,b,c)
  -- compare old and new graph and create a list of changes to made
    getNodeChanges :: [Int] -> [(Bool,Int)] -> [(Bool,Int)]
    getNodeChanges [] newGraph = newGraph
    getNodeChanges (node:rest) newGraph =
      if elem (False,node) newGraph
        then getNodeChanges rest (filter (removeNode (False,node)) newGraph)
        else getNodeChanges rest ((True,node):newGraph)
      where
      removeNode node nodes = if node == nodes then False else True
  --
    getEdgeChanges :: [(Int,Int,String)] -> [(Bool,Int,Int,String)] -> [(Bool,Int,Int,String)]
    getEdgeChanges [] newGraph = newGraph
    getEdgeChanges ((node1,node2,rel):rest) newGraph =
      if elem (False,node1,node2,rel) newGraph
        then getEdgeChanges rest (filter (removeEdge (False,node1,node2,rel)) newGraph)
        else getEdgeChanges rest ((True,node1,node2,rel):newGraph)
      where
      removeEdge edge edges = if edge == edges then False else True
  --
    nodeUpdates :: Graph.Gr (String, String, MOnto.OntoObjectType) String ->
                    MOnto.MMiSSOntology -> [(Bool,Int)] -> OntoConf.Config -> String
    nodeUpdates _ _ [] _ = ""
    nodeUpdates completeGraph onto ((delete,nId):rest) config =
      if delete
        then (UDraw.deleteNode nId) ++ "," ++ (nodeUpdates completeGraph onto rest config)
        else case Graph.lab completeGraph nId of
             Nothing -> ""
             Just (className,_,nType) ->
                (UDraw.createNode True nId className "" [] nType config) ++ "," ++ (nodeUpdates completeGraph onto rest config)
  --
    edgeUpdates :: Graph.Gr (String, String, MOnto.OntoObjectType) String ->
                    MOnto.MMiSSOntology -> [(Bool,Int,Int,String)] -> OntoConf.Config -> String
    edgeUpdates _ _ [] _ = ""
    edgeUpdates completeGraph onto ((delete,nId1,nId2,rel):rest) config =
      if delete
        then (UDraw.deleteEdge rel nId1 nId2) ++ "," ++ (edgeUpdates completeGraph onto rest config)
        else (UDraw.createEdges True [(rel,nId1,nId2)] config) ++ "," ++ (edgeUpdates completeGraph onto rest config)

--------------------------------------------
-- Update all attributes from graph.
--------------------------------------------
updateGraphAttributes :: MOnto.MMiSSOntology -> OntoConf.Config -> String
updateGraphAttributes onto config =
  let classGraph  = MOnto.getClassGraph onto
  in ("graph(change_attr([" ++ (generateNodes classGraph (Graph.labNodes classGraph)) ++"]))\n")
  where
    generateNodes _ []                                        = ""
    generateNodes classGraph ((nId,(className,_,nType)):rest) =
      (UDraw.updateNodeAttributes nId className "" (retrieveOutgoingEdges classGraph nId) nType config) ++
          (generateNodes classGraph rest)
    retrieveOutgoingEdges classGraph nId =
      map prepareEdges (Graph.out classGraph nId)
       where
       prepareEdges :: (Int, Int, String) -> (String, Int, Int)
       prepareEdges (startId, targetId, eType) = (eType, startId, targetId)

--------------------------------------------
-- Get a list of parent-nodes.
--------------------------------------------
getParents :: DVTypes.NodeId -> Graph.Gr (String, String, MOnto.OntoObjectType) String -> [Int]
getParents nId graph =
  Graph.suc graph (makeNodeIDInt nId)

--------------------------------------------
-- Get a list of sibling-nodes.
--------------------------------------------
getSiblings :: DVTypes.NodeId -> Graph.Gr (String, String, MOnto.OntoObjectType) String -> [Int]
getSiblings nId graph =
  Graph.pre graph (makeNodeIDInt nId)

--------------------------------------------
-- Get a list of nodes which are in relation
-- to this node.
--------------------------------------------
getInRelationTo :: DVTypes.NodeId -> Graph.Gr (String, String, MOnto.OntoObjectType) String -> [Int]
getInRelationTo nId graph =
  let suc = Graph.suc graph (makeNodeIDInt nId)
      pre = Graph.pre graph (makeNodeIDInt nId)
  in Debug.trace (show graph) (suc ++ pre)

--------------------------------------------
-- Search for a node bei node-id and return
-- the name
--------------------------------------------
getNodeName :: DVTypes.NodeId -> MOnto.MMiSSOntology -> String
getNodeName nId onto =
  let lNodes = Graph.labNodes ((MOnto.getClassGraph onto))
      name   = getName lNodes (makeNodeIDInt nId)
  in name
  where
  getName [] _                                = ""
  getName ((nodeId,(className,_,_)):rest) nId
    | nodeId == nId = className
    | otherwise     = getName rest nId


--------------------------------------------
-- Get's an DaVinceNodeID and returns a real
-- Int.
--
-- this funktion is shit, I'm sure there is
-- a better way, but I don't know
--------------------------------------------
makeNodeIDInt :: DVTypes.NodeId -> Int
makeNodeIDInt (DVTypes.NodeId (nId)) =
  read nId

--------------------------------------------
-- remove objects from graph
-------------------------------------------
filterObjects :: Graph.Gr (String, String, MOnto.OntoObjectType) String
                    -> Graph.Gr (String, String, MOnto.OntoObjectType) String
filterObjects oldGraph = getPureClassGraph oldGraph

--------------------------------------------
-- remove relation from graph which will
-- not be displayed.
-- Because the fgl does not handle labled
-- edges (it only carries a label, but
-- doesn't do anything with it), I have to
-- make another evil work around in which
-- a delete every edge an reload all other
-- edge which I don't want to delete.
-------------------------------------------
filterRelations :: UUtil.Relations -> Graph.Gr (String, String, MOnto.OntoObjectType) String
                        -> Graph.Gr (String, String, MOnto.OntoObjectType) String
filterRelations rels oldGraph =
  let unselectedRels = UUtil.getUnSelectedRelationNames rels
  in deleteRels unselectedRels oldGraph
  where
    deleteRels :: [String] -> Graph.Gr (String, String, MOnto.OntoObjectType) String
                  -> Graph.Gr (String, String, MOnto.OntoObjectType) String
    deleteRels [] graph = graph
    deleteRels (rel:rest) graph = deleteRels rest (Graph.elfilter (notThisRel rel) graph)

    notThisRel :: String -> String -> Bool
    notThisRel relToDelete rels =
      (not (relToDelete == rels))

-------------------------------------------
-- this will return the graph only with
-- isa-relation
-------------------------------------------
filterRelationsNotX :: String -> Graph.Gr (String, String, MOnto.OntoObjectType) String
                        -> Graph.Gr (String, String, MOnto.OntoObjectType) String
filterRelationsNotX rel graph =
  Graph.elfilter notISARel graph
  where
    notISARel :: String -> Bool
    notISARel rels =
      (rel == rels)

--------------------------------------------
--
--------------------------------------------
getPureClassGraph :: Graph.Gr (String,String,MOnto.OntoObjectType) String -> Graph.Gr (String,String,MOnto.OntoObjectType) String
getPureClassGraph g =
  let classNodeList = map (\(nid,_) -> nid) (getTypedNodes g MOnto.OntoClass)
  in nfilter (`elem` classNodeList) g

--------------------------------------------
--
--------------------------------------------
nfilter :: Graph.DynGraph gr => (Graph.Node -> Bool) -> gr a b -> gr a b
nfilter f =
  Graph.ufold cfilter Graph.empty
  where cfilter (p,v,l,s) g = if (f v)
                                then (p',v,l,s') Graph.& g
                                else g
          where p' = filter (\(b,u)->f u) p
                s' = filter (\(b,w)->f w) s

--------------------------------------------
--
--------------------------------------------
getTypedNodes :: Graph.Gr (String,String,MOnto.OntoObjectType) String -> MOnto.OntoObjectType -> [Graph.LNode (String, String, MOnto.OntoObjectType)]
getTypedNodes g t =
  map Graph.labNode' (Graph.gsel (\(_,_,(_,_,objType),_) -> objType == t) g)
