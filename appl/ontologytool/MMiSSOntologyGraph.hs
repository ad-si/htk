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


displayClassGraph :: MMiSSOntology -> Maybe String -> IO ()

displayClassGraph onto startClass =
  do ginfo <- A.initgraphs
     classGraph <- case startClass of 
                     Nothing -> return (getPureClassGraph (getClassGraph onto))
                     Just(className) -> case (gsel (\(p,v,(l,_,_),s) -> l == className) (getClassGraph onto)) of
                                          [] -> return (getPureClassGraph (getClassGraph onto))
                                          ((p,v,l,s):_) -> return(([],v,l,[]) & empty)
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
           (createEdgeTypes (getClassGraph onto))
           []
           ginfo
     updateDaVinciGraph classGraph gid ginfo
     A.Result gid _ <- A.redisplay gid ginfo
     return()
--     A.Result eid err2 <- addlink gid "relation" "RelationTitle" nid1 nid2 ginfo
--     putStr (show ontology)
--     getLine
--     delgraph gid ginfo

{--
emptyNodeMap :: A.NodeMapping
emptyNodeMap = emptyFM 
--}

{--
createDaVinciGraph :: A.NodeMapping -> Gr (String, String, OntoObjectType) String 
                        -> String -> A.Descr -> A.GraphInfo -> IO (A.NodeMapping)

createDaVinciGraph nodeMap classGraph nodeType gid ginfo =
  do nodeMap1 <- foldM (createNode gid ginfo) nodeMap (labNodes classGraph)
     nodeMap2 <- foldM (createLink gid ginfo) nodeMap1 (labEdges classGraph)
--     A.Result _ _ <- A.writeOntoGraph gid classGraph ginfo
     return nodeMap2
  where
    createNode :: Int -> A.GraphInfo -> A.NodeMapping -> LNode (String, String, OntoObjectType) -> IO (A.NodeMapping)
    createNode gid ginfo nMap (nodeID, (label, _, _)) = 
      do (A.Result nid _) <- A.addnode gid nodeType label ginfo
         return (addToFM nMap nodeID nid)

    createLink :: A.Descr -> A.GraphInfo -> A.NodeMapping -> LEdge String -> IO (A.NodeMapping)
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
--}


-- Klassengraph -> Objekte dazu (mit Links auf Klasse)
-- 

-- Klassengraph vorhanden -> Objektgraph als Input -> Objekte und Links sowie 'instanceOf' einfügen
-- Klassen vorhanden -> Objekte hinzu: 

updateDaVinciGraph :: Gr (String,String,OntoObjectType) String -> 
                              A.Descr -> A.GraphInfo -> IO ()
 
updateDaVinciGraph newGraph gid gv =
  do (gs,_) <- readIORef gv
     case lookup gid gs of
       Nothing -> return()
       Just g ->
        do oldGraph <- return(A.ontoGraph g)
           nMap <- return(A.nodeMap g)
	   nodeMap1 <- foldM (createNode gid gv oldGraph) nMap (labNodes newGraph)
	   nodeMap2 <- foldM (createLink gid gv) nodeMap1 (labEdges newGraph)
	   A.Result gid err <- A.writeOntoGraph gid newGraph gv
           A.Result gid err2 <- A.writeNodeMap gid nodeMap2 gv
           case err of
             Nothing -> return()
             Just(str) -> putStr str
           return()
	where
          getTypeLabel OntoClass = "class"
          getTypeLabel OntoObject = "object"
	  createNode :: Int -> A.GraphInfo -> Gr (String,String,OntoObjectType) String -> 
                          A.NodeMapping -> LNode (String, String, OntoObjectType) -> IO (A.NodeMapping)
	  createNode gid ginfo oldGraph nMap (nodeID, (name, className, objectType)) = 
	    case lookupFM nMap nodeID of
              Just(_) -> return nMap
              Nothing ->
                do (A.Result nid err) <- A.addnode gid (getTypeLabel objectType) name ginfo
                   case err of
                     Nothing -> return (addToFM nMap nodeID nid)
                     Just(str) -> do putStr str
                                     return (addToFM nMap nodeID nid)

	  createLink :: A.Descr -> A.GraphInfo -> A.NodeMapping -> LEdge String -> IO (A.NodeMapping)
	  createLink gid ginfo nMap (node1, node2, edgeLabel) = 
	    do dNodeID_1 <- case lookupFM nMap node1 of
			      Nothing -> return (-1)
			      Just(n) -> return(n)
	       dNodeID_2 <- case lookupFM nMap node2 of
			      Nothing -> return (-1)
			      Just(n) -> return(n)                        
	       if ((dNodeID_1 == -1) || (dNodeID_2 == -1))
		 then return nMap
		 else do A.Result eid err <- if (edgeLabel == "isa") || (edgeLabel == "instanceOf")
					       then A.addlink gid edgeLabel edgeLabel dNodeID_2 dNodeID_1 ginfo 
					       else A.addlink gid edgeLabel edgeLabel dNodeID_1 dNodeID_2 ginfo 
                         case err of
                           Nothing -> return()
                           Just(str) -> putStr str
			 return nMap


showObjectsForVisible :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
showObjectsForVisible onto gv (name,descr,gid) = 
  do (gs,_) <- readIORef gv
     case lookup gid gs of
       Nothing -> return()
       Just g ->
         do oldGraph <- return(A.ontoGraph g)
            let classesInOldGraph = map (\(_,_,(className,_,_),_) -> className)
                                        (filter (\(_,_,(_,_,objectType),_) -> objectType == OntoClass)  
                                             (map (context oldGraph) (nodes oldGraph)))
                objectList = map (\(nid,_) -> nid) 
                                 (filter (findObjectsOfClass classesInOldGraph) 
                                            (getTypedNodes (getClassGraph onto) OntoObject))
                objectGr = nfilter (`elem` objectList) (getClassGraph onto)
            updateDaVinciGraph (makeObjectGraph oldGraph (getPureClassGraph (getClassGraph onto)) objectGr) gid gv
            A.redisplay gid gv
            return () 
  where
    findObjectsOfClass classList (_,(_,className,_)) = className `elem` classList


showWholeObjectGraph :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
showWholeObjectGraph onto gv (name,descr,gid) = 
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     let objectList = map (\(nid,_) -> nid) (getTypedNodes (getClassGraph onto) OntoObject)
         objectGraph = nfilter (`elem` objectList) (getClassGraph onto)
     updateDaVinciGraph (makeObjectGraph empty (getClassGraph onto) objectGraph) gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 



{-- makeObjectGraph bekommt den alten Graphen, in den die Objekte und deren Klassen einzubeziehen sind, den Klassen-Graphen, in dem alle Klassen vorhanden sein sollten, sowie den Graphen mit den einzufügenden Objekten und deren Links übergeben. Die Funktion geht den Objektgraphen durch, fügt die Objekt-Knoten in den alten Graphen ein.
Für jeden eingefügten Objekt-Knoten sucht die Funktion im Klassengraphen dessen Klasse und fügt diese als Klassen-Knoten ebenfalls in den alten Graphen ein. Zwischen Klasse und Objekt wird eine InstanceOf-Kante eingefügt. Bei allen Einfüge-Operationen wird vorher geprüft, ob der Knoten schon drin war oder nicht.
--}

makeObjectGraph :: Gr (String,String,OntoObjectType) String
                   -> Gr (String,String,OntoObjectType) String -> Gr (String,String,OntoObjectType) String
                        -> Gr (String,String,OntoObjectType) String

makeObjectGraph oldGr classGr objectGr =
  let newGr = insNodes (labNodes objectGr) oldGr 
      newGr2 = foldl insEdgeSecurely newGr (labEdges objectGr)
      newGr3 = foldl (insInstanceOfEdge classGr) newGr2 (labNodes objectGr)
  in newGr3
  where 
    insEdgeSecurely gr (node1,node2,label) = 
      case match node1 gr of
        (Nothing,_) -> gr
        (Just(_),_) -> 
          case match node2 gr of
            (Nothing,_) -> gr
            (Just(_),_) -> insEdge (node1,node2,label) gr        

    insInstanceOfEdge classGr gr (_,(objectName, className,_)) =  
      case findLNode gr className of
        Nothing -> case findLNode classGr className of
                     Nothing -> gr
                     Just(classNodeID) -> insInstanceOfEdge1 (insNode (classNodeID,(className, "", OntoClass)) gr)
                                            classNodeID objectName 
        Just(classNodeID) -> insInstanceOfEdge1 gr classNodeID objectName 

    insInstanceOfEdge1 gr classNodeID objectName =
      case findLNode gr objectName of
        Nothing -> gr
        Just(objectNodeID) -> insEdge (objectNodeID, classNodeID, "instanceOf") gr  


showWholeClassGraph :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
showWholeClassGraph onto gv (name, descr, gid) = 
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     updateDaVinciGraph (getPureClassGraph (getClassGraph onto)) gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


showAllRelations :: MMiSSOntology -> A.GraphInfo -> [String] -> (String, Int, Int) -> IO ()
showAllRelations onto gv rels (name, _, gid) =
  do oldGv <- readIORef gv
     (A.Result descr error) <- purgeGraph gid gv
     updateDaVinciGraph (reduceToRelations (getClassGraph onto) name rels) gid gv
     case error of
       Just _ -> do writeIORef gv oldGv
		    return ()
       Nothing -> do A.redisplay gid gv
		     return () 


reduceToRelations :: Gr (String,String,OntoObjectType) String -> String -> [String] 
                     -> Gr (String,String,OntoObjectType) String
reduceToRelations g name forbiddenRels = 
  let g1 = elfilter (mynotElem forbiddenRels) g
  in case findLNode g1 name of
       Nothing -> g1
       Just(node) -> let nodeList = dfs [node] g1
                         toDelete = (nodes g1) \\ nodeList
                     in delNodes toDelete g1 
  where 
    mynotElem l a = notElem a l


showSuperSubClassesForVisible :: MMiSSOntology -> A.GraphInfo -> Bool -> Bool -> (String, Int, Int) -> IO ()
showSuperSubClassesForVisible onto gv showSuper transitive (name, descr, gid) =
  do nodeList <- myGetNodes gid gv
     if transitive
       then updateDaVinciGraph
		 (foldl (getSubSuperClosure (getClassGraph onto) showSuper) empty nodeList) 
		 gid gv
       else updateDaVinciGraph
		 (foldl (getSubSuperSingle (getClassGraph onto) showSuper) empty nodeList)
		 gid gv
     A.redisplay gid gv
     return () 


reduceToThisNode :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
reduceToThisNode onto gv (name, descr, gid) =
  do oldGv <- readIORef gv
     A.Result _ _ <- purgeGraph gid gv
     case (gsel (\(p,v,(l,_,_),s) -> l == name) (getClassGraph onto)) of
       [] -> return()
       ((p,v,l,s):_) -> do 
                           updateDaVinciGraph (([],v,l,[]) & empty) gid gv     
                           A.redisplay gid gv
                           return() 

showSuperSubClasses :: MMiSSOntology -> A.GraphInfo -> Bool -> Bool -> (String, Int, Int) -> IO ()
showSuperSubClasses onto gv showSuper transitive (name, descr, gid) =
  do oldGv <- readIORef gv
     if transitive
       then updateDaVinciGraph
              (getSubSuperClosure (getClassGraph onto) showSuper empty name) gid gv
       else updateDaVinciGraph (getSubSuperSingle (getClassGraph onto) showSuper empty name) gid gv
     A.redisplay gid gv
     return () 


getSubSuperSingle :: Gr (String,String,OntoObjectType) String -> Bool -> Gr (String,String,OntoObjectType) String
                        -> String -> Gr (String,String,OntoObjectType) String
getSubSuperSingle g showSuper newGr name =
  case findLNode g name of
    Nothing -> g
    Just(nodeID) -> 
      let subClassEdges = filter ((== "isa"). (\(_,_,a) -> a)) (inn g nodeID)
          ng = foldl (insPredecessorAndEdge g) (insertInitialNode nodeID name newGr) subClassEdges 
      in if showSuper 
           then let superClassEdges = filter ((== "isa").(\(_,_,a) -> a)) (out g nodeID)
                in foldl (insSuccessorAndEdge g) ng superClassEdges 
           else ng
  where
    insertInitialNode :: Node -> String ->  Gr (String,String,OntoObjectType) String 
                          ->  Gr (String,String,OntoObjectType) String
    insertInitialNode nodeID name gr =
      case match nodeID gr of
        (Nothing,_) -> ([], nodeID, (name,"",OntoClass),[]) & gr
        otherwise -> gr
        
    insPredecessorAndEdge :: Gr (String,String,OntoObjectType) String -> Gr (String,String,OntoObjectType) String 
                               -> LEdge String -> Gr (String,String,OntoObjectType) String
    insPredecessorAndEdge oldGr newGr (fromNode, toNode, edgeLabel) =
      case match fromNode oldGr of
        (Nothing, _) -> newGr
        (Just ((_,_,nodeLabel,_)),_) ->
           case match fromNode newGr of
             (Nothing, _) -> ([], fromNode, nodeLabel, [(edgeLabel, toNode)]) & newGr
             (Just((p,fromNodeID,nodeLabel,s)), newGr2) -> (p,fromNodeID,nodeLabel, ((edgeLabel,toNode):s)) & newGr2

    insSuccessorAndEdge :: Gr (String,String,OntoObjectType) String -> Gr (String,String,OntoObjectType) String 
                            -> LEdge String -> Gr (String,String,OntoObjectType) String
    insSuccessorAndEdge oldGr newGr (fromNode, toNode, edgeLabel) =
      case match toNode oldGr of
        (Nothing, _) -> newGr
        (Just ((_,_,(nodeLabel,_,_),_)),_) ->
           case match toNode newGr of
             (Nothing,_) -> ([(edgeLabel, fromNode)], toNode, (nodeLabel,"",OntoClass), []) & newGr
             (Just((p, toNodeID, nodeLabel, s)), newGr2) -> (((edgeLabel, fromNode):p), toNodeID, nodeLabel, s) & newGr2


getSubSuperClosure :: Gr (String,String,OntoObjectType) String -> Bool 
                        -> Gr (String,String,OntoObjectType) String -> String
                        -> Gr (String,String,OntoObjectType) String
getSubSuperClosure g showSuper newGr name =
  case findLNode g name of
    Nothing -> g
    Just(nodeID) -> 
      let ng = foldl (subClassClosure g) newGr [nodeID]
      in if showSuper
           then foldl (superClassClosure g nodeID) ng [nodeID]
           else ng
  where
    superClassClosure :: Gr (String,String,OntoObjectType) String -> Node 
                         -> Gr (String,String,OntoObjectType) String -> Node 
                         -> Gr (String,String,OntoObjectType) String
    superClassClosure g specialNodeID ng nodeID = 
      case match nodeID g of
        (Nothing, _) -> ng
        (Just((_,_,(label,_,_),outAdj)), _) -> 
          let isaAdj = filter ((== "isa") . fst) outAdj
              ng1 = foldl (superClassClosure g specialNodeID) ng (map snd isaAdj) 
          in if (nodeID == specialNodeID)
               then case match specialNodeID ng1 of
                      -- This should never be the case, but we somehow have to deal with it
                      (Nothing, _) -> (isaAdj, nodeID, (label,"",OntoClass), []) & ng1
                      (Just((inAdj,_,_,_)), ng2) -> (inAdj, nodeID, (label, "",OntoClass), isaAdj) & ng2
               else case match nodeID ng1 of
                      (Nothing, _) -> ([], nodeID, (label,"",OntoClass), isaAdj) & ng1
                      (Just((inAdj,_,_,outAdj)), ng2) -> (inAdj ++ isaAdj,nodeID,(label,"",OntoClass),outAdj) & ng2

{-- subClassClosure hunts transitively all isa-Ajacencies that goes into the given node (nodeID).
    For all nodes collected, their outgoing adjacencies are ignored because we only want to
    show the isa-Relation to the superclass. The given specialNodeID is the ID of the node from 
    which the search for subclasses startet. Because this node is already in the graph, we
    have to delete and reinsert it with its outgoing adjacencies (which consists of the
    isa-relations to it's superclasses, build by superClassClosure beforehand).
--}
    subClassClosure ::  Gr (String,String,OntoObjectType) String ->  Gr (String,String,OntoObjectType) String
                         -> Node -> Gr (String,String,OntoObjectType) String
    subClassClosure g ng nodeID = 
      case match nodeID g of
        (Nothing, _) -> ng
        (Just((inAdj,_,(label,_,_), outAdj)), _) -> 
          let isaAdj = filter ((== "isa") . fst) inAdj
              ng1 = foldl (subClassClosure g) ng (map snd isaAdj) 
          in case match nodeID ng1 of
               (Nothing, _) -> (isaAdj, nodeID, (label,"",OntoClass), []) & ng1       
               (Just(_),_) -> ng1



hideObjectsForVisible :: MMiSSOntology -> A.GraphInfo -> (String, Int, Int) -> IO ()
hideObjectsForVisible onto gv (name,descr,gid) = 
  do (gs,_) <- readIORef gv
     case lookup gid gs of
       Nothing -> return()
       Just g ->
         do oldGraph <- return(A.ontoGraph g)
            let objectNodeIDs = map (\(_,v,_,_) -> v) (gsel (\(_,_,(_,_,t),_) -> t == OntoClass) oldGraph)
            updateDaVinciGraph (nfilter (`notElem` objectNodeIDs) oldGraph) gid gv
            A.redisplay gid gv
            return () 
    


createEdgeTypes ::  Gr (String,String,OntoObjectType) String -> [(String,DaVinciArcTypeParms (String,A.Descr))]
createEdgeTypes g = map createEdgeType ((nub (map (\(_,_,l) -> l) (labEdges g))) ++ ["instanceOf"])
  where
    createEdgeType str =
      case str of
        "isa" ->
             ("isa", 
               Thick
               $$$ Dir "first"
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int))
        "instanceOf" ->
             ("instanceOf", 
               Dotted
               $$$ Dir "first"
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int))
        otherwise ->
             (str,
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
                          ,Blank
                          ,Button "Show objects" (showObjectsForVisible onto ginfo)
                          ,Button "Hide objects" (hideObjectsForVisible onto ginfo)
                        ]
                      ),
                      (Menu (Just "Show relations") 
                        ([Button "All relations" (showAllRelations onto ginfo ["isa"]),
                          Blank]
                          ++ (createRelationMenuButtons (getRelationNames onto) onto ginfo))
                      ),
                      Button "Show whole class graph" (showWholeClassGraph onto ginfo),
                      Button "Show whole object graph" (showWholeObjectGraph onto ginfo),
                      Button "Reduce to this node" (reduceToThisNode onto ginfo)
                      ]
                     ))


createRelationMenuButtons relNames onto ginfo = map createButton relNames
  where 
    createButton name = (Button (name) 
                                (showAllRelations onto ginfo (delete name (relNames ++ ["isa"]))))


findLNode :: Gr (String,String,OntoObjectType) String -> String -> Maybe Node

findLNode gr label = case (gsel (\(p,v,(l,_,_),s) -> l == label) gr) of
                      [] -> Nothing
                      conList -> Just(node' (head conList))               

myDeleteNode :: A.Descr -> A.GraphInfo -> A.Result -> (Int,(String,DaVinciNode (String,Int,Int))) -> IO (A.Result)
myDeleteNode gid gv _ node = A.delnode gid (fst node) gv 


purgeGraph :: Int -> A.GraphInfo -> IO (A.Result)
purgeGraph gid gv = 
  do (gs,ev_cnt) <- readIORef gv
     case lookup gid gs of 
       Just g -> do A.Result _ _ <- A.writeOntoGraph gid empty gv
                    A.Result _ _ <- A.writeNodeMap gid emptyFM gv
                    foldM (myDeleteNode gid gv) (A.Result 0 Nothing) (A.nodes g)
       Nothing -> return (A.Result 0 (Just ("Graph id "++show gid++" not found")))


myGetNodes :: Int -> A.GraphInfo -> IO ([String])
myGetNodes gid gv = 
  do (gs,ev_cnt) <- readIORef gv
     case lookup gid gs of 
       Just g -> return(map (\(_,(name,_,_)) -> name) (labNodes (A.ontoGraph g)))
--       Just g -> return (map (\(_,(name,_)) -> name) (A.nodes g))
       Nothing -> return([])


getPureClassGraph :: Gr (String,String,OntoObjectType) String -> Gr (String,String,OntoObjectType) String
-- getPureClassGraph g = efilter (\(_,_,edgeType) -> edgeType == "isa") g
getPureClassGraph g = 
  let classNodeList = map (\(nid,_) -> nid) (getTypedNodes g OntoClass)
  in nfilter (`elem` classNodeList) g


nfilter :: DynGraph gr => (Node -> Bool) -> gr a b -> gr a b 
nfilter f = ufold cfilter empty
            where cfilter (p,v,l,s) g = if (f v) 
                                          then (p',v,l,s') & g
                                          else g
                   where p' = filter (\(b,u)->f u) p
                         s' = filter (\(b,w)->f w) s


getTypedNodes :: Gr (String,String,OntoObjectType) String -> OntoObjectType 
                 -> [LNode (String, String, OntoObjectType)]
getTypedNodes g t = 
  map labNode' (gsel (\(_,_,(_,_,objType),_) -> objType == t) g)