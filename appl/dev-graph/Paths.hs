module Paths (hideInternalNodes, allPaths, restoreEdges, compatibleTypes) where

import Graph
import List

-- hides all internal nodes and their edges,
-- replacing paths between nodes of type Defined by corresponding edges
hideInternalNodes :: Graph -> Graph
hideInternalNodes = defs.allPaths

-- just changes the type of the argument from Graph to list for the function addPaths
allPaths :: Graph -> Graph
allPaths gr = toGraph (addPaths (fromGraph gr) (fromGraph gr))

-- adds all paths of a graph as edges
-- without removing any of the previous ones
addPaths :: [(NodeType,[(EdgeType,NodeType)])] -> [(NodeType,[(EdgeType,NodeType)])] -> [(NodeType,[(EdgeType,NodeType)])] 
addPaths [] gr = []
addPaths remGr gr= ( (fst (head remGr), (nub(makePaths (toGraph gr) (snd (head remGr)) [] ))):
			(addPaths (tail remGr) gr))

-- gets the list of edges of a certain node
-- and determines all paths starting from it by recursively going through the target nodes
makePaths :: Graph -> [(EdgeType,NodeType)] -> [String] -> [(EdgeType,NodeType)]
makePaths gr [] et = []
makePaths gr tgt et = if (isDefined tgt1) then
		         newTgt
		   else (makePaths gr (snd(findNode gr tgt1)) ((getEdgeType(fst(head tgt))):et) )++newTgt

  	where	tgt1 = snd(head tgt)
		newTgt = (determineEdgeType ((fst(head tgt)):(map read et)),snd(head tgt)):(makePaths gr (tail tgt) et)

--		newTgt = if (all (== getEdgeType (fst (head tgt)) ) et)
--			    then ((head tgt):(makePaths gr (tail tgt) et))
--			    else ((MixedThmType,MixedLocType),snd(head tgt)):(makePaths gr (tail tgt) et)

-- drops all nodes that are not of type Defined
-- and calls function toDef
defs :: Graph -> Graph
defs gr = toGraph (map toDef [part| part <- (fromGraph gr), isDefined (fst part)])

-- drops all edges that do not lead to a node of type Defined
toDef :: (NodeType,[(EdgeType,NodeType)]) -> (NodeType,[(EdgeType,NodeType)])
toDef (node,list) = (node,[(ed,nd)| (ed,nd) <- list, isDefined nd])


-- function to determine the edgetype of the path from the types of the edges contained
determineEdgeType :: [EdgeType] -> EdgeType
determineEdgeType et = (determineThmType (map fst et), determineLocType (map snd et))

determineThmType :: [ThmType] -> ThmType
determineThmType tt
  | all (== head tt) (tail tt)	= head tt
  | elem proofObligation tt	= mixedProofObl  
  | otherwise			= mixedThmType

determineLocType :: [LocType] -> LocType
determineLocType lt
  | all (== head lt) (tail lt)	= head lt
  | otherwise                   = mixedLocType
  



restoreEdges :: Graph -> Graph -> (NodeType,(EdgeType,NodeType)) -> Graph
restoreEdges oldGr actGr (scr,(ed,tgt)) = if(not (isDefined scr) || not (isDefined tgt)) then actGr
                                           else replacePath oldGr (removeEdge actGr scr tgt ed) (findNode oldGr scr) ed tgt []

replacePath :: Graph -> Graph -> (NodeType,[(EdgeType,NodeType)]) -> EdgeType -> NodeType -> [EdgeType] -> Graph
replacePath oldGr actGr (scr,list) ed tgt et = if (list /= []) then
						  if (isDefined (snd(head list))) then
						      if ((snd(head list) == tgt) && (determineEdgeType ((fst(head list)):et) == ed)) then
						         replacePath oldGr (addEdge (addNode actGr scr) scr tgt (fst(head list))) (scr, tail list) ed tgt et
						      else replacePath oldGr actGr (scr, tail list) ed tgt et
						  else
						      if ((not(compatibleTypes ed ((fst(head list)):et))) || ((furtherPath == actGr)&&(not (isNode actGr (snd(head list)))) ) ) then
						      -- || (furtherPath == actGr)) then
						           replacePath oldGr actGr (scr, tail list) ed tgt et
						      else
						      --  if((furtherPath == actGr)&&(not (isNode actGr (snd(head list))))) then
							--  actGr
						        --else
						           replacePath oldGr (addEdge (addNode furtherPath scr) scr (snd(head list)) (fst(head list))) (scr, tail list) ed tgt et
					       else actGr

        where
	   furtherPath = replacePath oldGr actGr (findNode oldGr (snd(head list))) ed tgt ((fst(head list)):et)
	  

-- used by Paths.hs to find out whether the types of a list of edges are
-- compatible with the type of the given edge
compatibleTypes :: EdgeType -> [EdgeType] -> Bool
compatibleTypes ed et = and [elem y (compLocType (snd ed))| y <- aux]

   where
	 aux = [snd x | x <- et, elem (fst x) (compThmType (fst ed)) ]
   
-- part of compatibleTypes, checks the compatiblity of the scope types
compLocType :: LocType -> [LocType]
compLocType loc = case (show loc) of
  "MixedLocType" -> read "[LocalLink,GlobalLink]"
  _ -> [loc]

-- part of compatibleTypes, checks the compability of the proof types
compThmType :: ThmType -> [ThmType]
compThmType thm = case (show thm) of
  "MixedThmType" -> read "[Axiom,Theorem]"
  "MixedProofObl" -> read "[Axiom,Theorem,ProofObligation]"
  _ -> [thm]
