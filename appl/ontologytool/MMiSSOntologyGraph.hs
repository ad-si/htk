module MMiSSOntologyGraph (
  displayClassGraph 
  -- MMiSSOntology -> IO ()
)
where


import Data.FiniteMap
import Data.List
import Monad
import IORef
import Char

import DaVinciGraph
import GraphDisp
import GraphConfigure
import MMiSSOntology

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.TransClos

import qualified AbstractGraphView as A

type NodeMapping = FiniteMap Node A.Descr



displayClassGraph :: MMiSSOntology -> IO ()

displayClassGraph onto =
  do ginfo <- A.initgraphs
     classGraph <- return (getClassGraph onto)
     A.Result gid err <-
       A.makegraph (getOntologyName onto)
           [GlobalMenu (Button "Show all classes" (putStrLn "Knopf2 wurde gedrückt")),
            GlobalMenu (Button "Knopf2" (putStrLn "Knopf2 wurde gedrückt"))]
           [("class", Box $$$ Color "azure2" $$$
                   createLocalMenu onto ginfo 
                   $$$ ValueTitle ( \ (name,descr,gid) -> return name) $$$
                   emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int,Int)
            ),
            ("object", Box  $$$ Color "#ffffA0" $$$
                   createLocalMenu onto ginfo
                   $$$ ValueTitle ( \ (name,descr,gid) -> return name) $$$
                   emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int,Int)
            )]
           (createEdgeTypes classGraph)
           []
           ginfo
     nodeMap <- return (emptyNodeMap)
     nodeMap1 <- createDaVinciGraph nodeMap classGraph "class" gid ginfo
     A.Result gid _ <- A.redisplay gid ginfo
     return()
--     A.Result eid err2 <- addlink gid "relation" "RelationTitle" nid1 nid2 ginfo
--     putStr (show ontology)
--     getLine
--     delgraph gid ginfo


emptyNodeMap :: NodeMapping
emptyNodeMap = emptyFM 


createDaVinciGraph :: NodeMapping -> Gr String String -> String -> A.Descr -> A.GraphInfo -> IO (NodeMapping)

createDaVinciGraph nodeMap classGraph nodeType gid ginfo =
  do nodeMap1 <- foldM (createNode gid ginfo) nodeMap (labNodes classGraph)
     nodeMap2 <- foldM (createLink gid ginfo) nodeMap1 (labEdges classGraph)
     A.Result _ _ <- A.writeOntoGraph gid classGraph ginfo
     return nodeMap2
  where
    createNode :: Int -> A.GraphInfo -> NodeMapping -> LNode String -> IO (NodeMapping)
    createNode gid ginfo nMap (nodeID, label) = 
      do (A.Result nid _) <- A.addnode gid nodeType label ginfo
         return (addToFM nMap nodeID nid)

    createLink :: A.Descr -> A.GraphInfo -> NodeMapping -> LEdge String -> IO (NodeMapping)
    createLink gid ginfo nMap (node1, node2, edgeLabel) = 
      do dNodeID_1 <- case lookupFM nMap node1 of
                        Nothing -> return (-1)
                        Just(n) -> return(n)
         dNodeID_2 <- case lookupFM nMap node2 of
                        Nothing -> return (-1)
                        Just(n) -> return(n)                        
         if ((dNodeID_1 == -1) || (dNodeID_2 == -1))
           then return nMap
           else do A.Result eid _ <- if (edgeLabel == "isa")
                                       then A.addlink gid edgeLabel edgeLabel dNodeID_2 dNodeID_1 ginfo 
                                       else A.addlink gid edgeLabel edgeLabel dNodeID_1 dNodeID_2 ginfo 
                   return nMap


showObjectGraph :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
showObjectGraph onto gv (name,descr,gid) = 
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     nMap <- createDaVinciGraph (emptyNodeMap) (getObjectGraph onto) "object" gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


showWholeGraph :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
showWholeGraph onto gv (name, descr, gid) = 
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     nMap <- createDaVinciGraph (emptyNodeMap) (getClassGraph onto) "class" gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


showAllRelations :: MMiSSOntology -> A.GraphInfo -> [String] -> (String, Int, Int) -> IO ()
showAllRelations onto gv rels (name, _, gid) =
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     nMap <- createDaVinciGraph (emptyNodeMap) 
                                (reduceToRelations (getClassGraph onto) name rels) "class" gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


reduceToRelations :: Gr String String -> String -> [String] -> Gr String String
reduceToRelations g name forbiddenRels = 
  let g1 = elfilter (mynotElem forbiddenRels) g
  in case findLNode g1 name of
       Nothing -> g1
       Just(node) -> let nodeList = udfs [node] g1
                         toDelete = (nodes g1) \\ nodeList
                     in delNodes toDelete g1 
  where 
    mynotElem l a = notElem a l


showSuperSubClassesForVisible :: MMiSSOntology -> A.GraphInfo -> Bool -> Bool -> (String, Int, Int) -> IO ()
showSuperSubClassesForVisible onto gv showSuper transitive (name, descr, gid) =
  do oldGv <- readIORef gv
     nodeList <- myGetNodes gid gv
     nMap <- if transitive
               then createDaVinciGraph (emptyNodeMap) 
                      (foldl (showSubSuperClosure (getClassGraph onto) showSuper) empty nodeList) 
                      "class" gid gv
               else createDaVinciGraph (emptyNodeMap) 
                      (foldl (showSubSuperSingle (getClassGraph onto) showSuper) empty nodeList)
                      "class" gid gv
     A.redisplay gid gv
     return () 


showSuperSubClasses :: MMiSSOntology -> A.GraphInfo -> Bool -> Bool -> (String, Int, Int) -> IO ()
showSuperSubClasses onto gv showSuper transitive (name, descr, gid) =
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     nMap <- if transitive
               then createDaVinciGraph (emptyNodeMap) 
                      (showSubSuperClosure (getClassGraph onto) showSuper empty name) "class" gid gv
               else createDaVinciGraph (emptyNodeMap) 
                      (showSubSuperSingle (getClassGraph onto) showSuper empty name) "class" gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


showSubSuperSingle :: Gr String String -> Bool -> Gr String String -> String -> Gr String String
showSubSuperSingle g showSuper newGr name =
  case find ((== name). snd) (labNodes g) of
    Nothing -> g
    Just((nodeID, _)) -> 
      let subClassEdges = filter ((== "isa"). (\(_,_,a) -> a)) (inn g nodeID)
          ng = foldl (insPredecessorAndEdge g) (([], nodeID, name,[]) & newGr) subClassEdges 
      in if showSuper 
           then let superClassEdges = filter ((== "isa").(\(_,_,a) -> a)) (out g nodeID)
                in foldl (insSuccessorAndEdge g) ng superClassEdges 
           else ng
  where
    insPredecessorAndEdge :: Gr String String -> Gr String String -> LEdge String -> Gr String String
    insPredecessorAndEdge oldGr newGr (fromNode, toNode, edgeLabel) =
      case match fromNode oldGr of
        (Nothing, _) -> newGr
        (Just ((_,_,nodeLabel,_)),_) ->
           case match fromNode newGr of
             (Nothing, _) -> ([], fromNode, nodeLabel, [(edgeLabel, toNode)]) & newGr
             _ -> insEdge (fromNode, toNode, edgeLabel) newGr

    insSuccessorAndEdge :: Gr String String -> Gr String String -> LEdge String -> Gr String String
    insSuccessorAndEdge oldGr newGr (fromNode, toNode, edgeLabel) =
      case match toNode oldGr of
        (Nothing, _) -> newGr
        (Just ((_,_,nodeLabel,_)),_) ->
           case match toNode newGr of
             (Nothing,_) -> ([(edgeLabel, fromNode)], toNode, nodeLabel, []) & newGr
             _ -> insEdge (fromNode, toNode, edgeLabel) newGr


showSubSuperClosure :: Gr String String -> Bool -> Gr String String -> String -> Gr String String
showSubSuperClosure g showSuper newGr name =
  case find ((== name). snd) (labNodes g) of
    Nothing -> g
    Just((nodeID, _)) -> 
      let ng = foldl (subClassClosure g) newGr [nodeID]
      in if showSuper
           then foldl (superClassClosure g nodeID) ng [nodeID]
           else ng
  where
    superClassClosure :: Gr String String -> Node -> Gr String String -> Node -> Gr String String 
    superClassClosure g specialNodeID ng nodeID = 
      case match nodeID g of
        (Nothing, _) -> ng
        (Just((_,_,label,outAdj)), _) -> 
          let isaAdj = filter ((== "isa") . fst) outAdj
              ng1 = foldl (superClassClosure g specialNodeID) ng (map snd isaAdj) 
          in if (nodeID == specialNodeID)
               then case match specialNodeID ng1 of
                      -- This should never be the case, but we somehow have to deal with it
                      (Nothing, _) -> (isaAdj, nodeID, label, []) & ng1
                      (Just((inAdj,_,_,_)), ng2) -> (inAdj, nodeID, label, isaAdj) & ng2
               else ([], nodeID, label, isaAdj) & ng1

{-- subClassClosure hunts transitively all isa-Ajacencies that goes into the given node (nodeID).
    For all nodes collected, their outgoing adjacencies are ignored because we only want to
    show the isa-Relation to the superclass. The given specialNodeID is the ID of the node from 
    which the search for subclasses startet. Because this node is already in the graph, we
    have to delete and reinsert it with its outgoing adjacencies (which consists of the
    isa-relations to it's superclasses, build by superClassClosure beforehand).
--}
    subClassClosure :: Gr String String -> Gr String String -> Node -> Gr String String 
    subClassClosure g ng nodeID = 
      case match nodeID g of
        (Nothing, _) -> ng
        (Just((inAdj,_,label, outAdj)), _) -> 
          let isaAdj = filter ((== "isa") . fst) inAdj
              ng1 = foldl (subClassClosure g) ng (map snd isaAdj) 
          in (isaAdj, nodeID, label, []) & ng1       


createEdgeTypes :: Gr String String -> [(String,DaVinciArcTypeParms (String,A.Descr))]
createEdgeTypes g = map createEdgeType (nub (map (\(_,_,l) -> l) (labEdges g)))
  where
    createEdgeType str =
      if (str == "isa") 
        then ("isa", 
               Thick
               $$$ Dir "first"
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int))
        else (str,
              Solid
              $$$ Head "arrow"
              $$$ ValueTitle (\ (name, _) -> return name)
--               $$$ TitleFunc (\ (name, _) -> name)
              $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int))


createLocalMenu onto ginfo =
                   LocalMenu (Menu Nothing 
                    ([(Menu (Just "For this node") 
                        [  (Menu (Just "Show transitively") 
                             [Button "Subclasses" (showSuperSubClasses onto ginfo False True),
                              Button "Sub/Superclasses" (showSuperSubClasses onto ginfo True True)])
                          ,(Menu (Just "Show adjacent") 
                             [Button "Subclasses" (showSuperSubClasses onto ginfo False False),
                              Button "Sub/Superclasses" (showSuperSubClasses onto ginfo True False)])
                        ]
                      ),
                      (Menu (Just "For visible nodes") 
                        [  (Menu (Just "Show transitively") 
                             [Button "Subclasses" (showSuperSubClassesForVisible onto ginfo False True),
                              Button "Sub/Superclasses" (showSuperSubClassesForVisible onto ginfo True True)])
                          ,(Menu (Just "Show adjacent") 
                             [Button "Subclasses" (showSuperSubClassesForVisible onto ginfo False False),
                              Button "Sub/Superclasses" (showSuperSubClassesForVisible onto ginfo True False)])
                        ]
                      ),
                      (Menu (Just "Show relations") 
                        ([Button "All relations" (showAllRelations onto ginfo ["isa"]),
                          Blank]
                          ++ (createRelationMenuButtons (getRelationNames onto) onto ginfo))
                      ),
                      Button "Show whole graph" (showWholeGraph onto ginfo),
                      Blank,
                      Button "Show object graph" (showObjectGraph onto ginfo),
                      Blank]
                     ))


createRelationMenuButtons relNames onto ginfo = map createButton relNames
  where 
    createButton name = (Button (name) 
                                (showAllRelations onto ginfo (delete name (relNames ++ ["isa"]))))


findLNode :: Gr String String -> String -> Maybe Node

findLNode gr label = case (gsel (\(p,v,l,s) -> l == label) gr) of
                      [] -> Nothing
                      conList -> Just(node' (head conList))               

myDeleteNode :: A.Descr -> A.GraphInfo -> A.Result -> (Int,(String,DaVinciNode (String,Int,Int))) -> IO (A.Result)
myDeleteNode gid gv _ node = A.delnode gid (fst node) gv 


purgeGraph :: Int -> A.GraphInfo -> IO (A.Result)
purgeGraph gid gv = 
  do (gs,ev_cnt) <- readIORef gv
     case lookup gid gs of 
       Just g -> foldM (myDeleteNode gid gv) (A.Result 0 Nothing) (A.nodes g)
       Nothing -> return (A.Result 0 (Just ("Graph id "++show gid++" not found")))


myGetNodes :: Int -> A.GraphInfo -> IO ([String])
myGetNodes gid gv = 
  do (gs,ev_cnt) <- readIORef gv
     case lookup gid gs of 
       Just g -> return (map (\(_,(name,_)) -> name) (A.nodes g))
       Nothing -> return([])