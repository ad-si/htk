-- die ev_cnt Handhabung ueberpruefen
-- in hidenodes die Listen on nn oe ne mit notElem-Variante erstellen
-- vielleicht: beim erzeugen der Pfade den Typ erst ausrechnen, wenn Pfad vollstaendig ??

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
   -> Die Funktionen sollen scheitern, wenn sie nicht fuer alle
      Knoten/Kanten funktionieren!
-}

import DaVinciGraph

import GraphDisp
import GraphConfigure

import Destructible

import List(nub)

-- Which graph display tool to be used, perhaps make it more tool independent?
-- Eq fuer DaVinci umschiffen ueber Deskriptoren
-- abstrakten Knotentypen erschaffen
-- Namen der DaVincis nachguckbar machen

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
-- type Event = (Descr,[(Int,(String,DaVinciNode (String,Int)))],[(Int,(String,DaVinciNode (String,Int)))],[(Int,(Int,Int,String,DaVinciArc (String,Int)))],[(Int,(Int,Int,String,DaVinciArc (String,Int)))]) -- gid, new nodes, old nodes, new edges, old edges
data AbstractionGraph = AbstractionGraph {
       theGraph :: OurGraph, -- (DaVinciNode,String) () (DaVinciEdge,String) (),
       nodeTypes :: [(String,DaVinciNodeType (String,Int))],
       edgeTypes :: [(String,DaVinciArcType (String,Int))],
       nodes :: [(Int,(String,DaVinciNode (String,Int)))], -- id, type, daVinci
       edges :: [(Int,(Int,Int,String,DaVinciArc (String,Int)))], -- id, src, tar, type, daVinci
       -- probably, also the abstracted graph needs to be stored,
       -- and a list of hide/abstract events with the hidden nodes/edges (for each event),
       -- which is used to restore things when showIt is called
       edgeComp :: CompTable,
       eventTable :: [(Int,Entry)]}

type Descr = Int
type GraphInfo = ([(Descr,AbstractionGraph)],Descr) -- for each graph the descriptor and the graph,
                                                    -- plus a global counter for new descriptors
data Result = Result GraphInfo                      -- the new graph list
                     Descr                          -- graph, node or edge descriptor
                     (Maybe String)                 -- a possible error message


data Entry = Entry {newNodes :: [(Descr,(String,DaVinciNode (String,Int)))],
		    oldNodes :: [(Descr,(String,DaVinciNode (String,Int)))],
		    newEdges :: [(Int,(Int,Int,String,DaVinciArc (String,Int)))],
		    oldEdges :: [(Int,(Int,Int,String,DaVinciArc (String,Int)))]
		--    counter :: Descr
		    }


-- creates a new entry of the eventTable and fills it with the data contained in its parameters
createEntry :: [(Descr,(String,DaVinciNode (String,Int)))] -> [(Descr,(String,DaVinciNode (String,Int)))] -> [(Descr,(Int,Int,String,DaVinciArc (String,Int)))] -> [(Descr,(Int,Int,String,DaVinciArc (String,Int)))] -> Descr -> (Int,Entry)
createEntry nn on ne oe cnt = (cnt, Entry {newNodes = nn, oldNodes = on, newEdges = ne, oldEdges = oe}) --, counter = cnt}


-- zips two lists by pairing each element of the first with each element of the second
specialzip :: [a] -> [b] -> [(a,b)]
specialzip [] _ = []
specialzip _ [] = []
specialzip (x:xs) (y:ys) = (x,y):(specialzip [x] ys)++(specialzip xs (y:ys))


-- similar to lookup, but also returns the decriptor
-- should only be used, if lookup will be successful (otherwise an error is thrown)
get :: Descr -> [(Descr,a)] -> (Descr,a)
get d list = case lookup d list of
                Just r -> (d,r)
		Nothing -> error ("get: descriptor unknowm: "++(show d))


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
      abstractNodetypeparams = LocalMenu (Menu Nothing []) $$$            
	                       Rhombus  $$$
                               ValueTitle ( \ (s,i) -> return s) $$$
                               emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int)
      (nodetypenames,nodetypeparams1) = unzip (("ABSTRACT",abstractNodetypeparams):nodetypeparams)
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
            edgeComp = comptable,
	    eventTable = [] }
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
        return (g{nodes = (ev_cnt,(nodetype,node)):nodes g},ev_cnt,Nothing)
   )

delnode :: Descr -> Descr -> GraphInfo -> IO Result
delnode gid node (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
      case lookup node (nodes g) of
        Just n -> do deleteNode (theGraph g) (snd n)
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
       edge <- newArc (theGraph g) et (name,ev_cnt) (snd src_node) (snd tar_node)
       return (g{edges = (ev_cnt,(src,tar,edgetype,edge)):edges g},ev_cnt,Nothing)
    (Nothing,_,_) -> return (g,0,Just ("addlink: illegal edge type: "++edgetype))
    (_,Nothing,_) -> return (g,0,Just ("addlink: illegal source node id: "++show src))
    (_,_,Nothing) -> return (g,0,Just ("addlink: illegal target node id: "++show tar))
   )


dellink :: Descr -> Descr -> GraphInfo -> IO Result
dellink gid edge (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case lookup edge (edges g) of
        Just (_,_,_,e) -> 
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

-- 0000000000000000000000000000000000000

-- determines from the types of two edges the type of the path replacing them (using the edgeComp table of the graph)
determineedgetype :: AbstractionGraph -> (String,String) -> Maybe String
determineedgetype g (t1,t2) = case result of
                              [] -> Nothing
			      x:xs -> Just x

  where result = [t| (tp1,tp2,t) <- (edgeComp g), (tp1==t1)&&(tp2==t2)]
	
-- noch nicht richtig implementiert!!
determinenodetype :: AbstractionGraph -> [String] -> Maybe String
determinenodetype g [] = Nothing
determinenodetype g typelist = Just (head typelist)


-- returns a pair of lists: one list of all in- and one of all out-going edges of the node
fetchEdgesOfNode :: AbstractionGraph -> Descr -> Maybe ([Descr],[Descr])
fetchEdgesOfNode g node = case sequence (map ((flip lookup) (edges g)) (map fst (edges g))) of
                            Just el -> Just ([descr|ed@(descr,(_,t,_,_)) <- (edges g), t == node],[descr|ed@(descr,(s,_,_,_)) <- (edges g), s == node])
			    Nothing -> Nothing


-- Handhabung von ev_cnt ueberpruefen!!!!!!!!!!!

-- sind delEdges wirklich die einzigen zu loeschenden Edges? - ja, denn jede zu loeschende egde ist ja eine in-/outgoing edge eines zu versteckenden Knotens

hidenodes :: Descr -> [Descr] -> GraphInfo -> IO Result
hidenodes gid node_list (gs,ev_cnt) = 
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case sequence (map (\node -> lookup node (nodes g)) node_list) of
      Just nl -> do -- try to determine the path to add and the edges to remove
                    case makepathsMain g node_list of
	            -- try to create the paths
		      Just (newEdges,delEdges) -> do -- descriptor list of the edges to be removed
		                                     let oeDescr = nub ((concat (map fst delEdges))++(concat (map snd delEdges)))
		                                     -- list of the complete edges to be removed (with descriptor)
						         oe = map (flip get (edges g)) oeDescr
			              	             -- try to remove these edges
						     deletedEdges@(Result info1 de1 error1) <- hideedgesaux gid oeDescr (gs,ev_cnt)
						     case error1 of
						       Nothing -> do let existingEdges = [(src,tgt,tp)|(descr,(src,tgt,tp,daVinci)) <- (edges (snd (get gid (fst info1))))]
						                         filteredNewEdges = [path| path@(src,tgt,tp) <- newEdges, notElem (src,tgt,tp) existingEdges]
						                     paths@(Result info2 de2 error2) <- addpaths gid filteredNewEdges info1
								     case error2 of
								       Nothing -> do -- descriptor list of the new edges
 								                     let neDescr = [ev_cnt..((snd info2) -1)] -- oder de statt ((snd info1) -1)??
								                     -- list of the complete new edges (with descriptor)
								                         ne = map (flip get (edges (snd (get gid (fst info2))))) neDescr
								                     -- list of the complete nodes (with descriptor) to be hidden
								 	                 on = map (flip get (nodes g)) node_list
										     -- try to remove these nodes
										     deletedNodes@(Result info3 de3 error3) <- hidenodesaux gid node_list info2
										     case error3 of
										       Nothing -> do -- save the changes in an entry
										                     let newEvent = createEntry [] on ne oe de3
										            	         g' = snd (get gid (fst info3))
                                                                                                     return (g'{eventTable = newEvent:eventTable g'},ev_cnt+1,Nothing)
										       Just t -> return (g,0,Just ("hidenodes: error hiding nodes: "++t))
								       Just text -> return (g,0,Just ("hidenodes: error adding paths: "++text))
						       Just text -> return (g,0,Just ("hidenodes: error deleting edges: "++text))
		      Nothing -> return (g,0,Just "hidenodes: error making paths\n(possible reasons: an error occured getting the edges of the nodes\nor a pathtype could not be determined (missing entry in edgeComp table))")
      Nothing -> return (g,0,Just "hidenodes: unknown node(s)")
  )			   


-- auxiliary function which does the actual hiding of the nodes
hidenodesaux :: Descr -> [Descr] -> GraphInfo -> IO Result
hidenodesaux gid [] (gs,ev_cnt) = return (Result (gs,ev_cnt) ev_cnt Nothing)
hidenodesaux gid (d:delNodes) (gs,ev_cnt) = do deletedNode@(Result info de error) <- delnode gid d (gs,ev_cnt)
                                               case error of
				                 Nothing -> do let gs' = fst info
				                               hidenodesaux gid delNodes (gs',ev_cnt+1)
				                 Just t -> return deletedNode


makepathsMain :: AbstractionGraph -> [Descr] -> Maybe ([(Descr,Descr,String)],[([Descr],[Descr])]) -- Liste der neu einzuf., Liste der zu entf. edges
makepathsMain g node_list =
  -- try to determine the in- and outgoing edges of the nodes
  case sequence (map (fetchEdgesOfNode g) node_list) of
     -- try to make paths of these edges
     Just edgelistPairs -> case sequence (map (makepaths g node_list) edgelistPairs) of
                                         -- return the paths to add and the edges to remove
                                         Just paths -> Just (nub (concat paths),edgelistPairs)
					 Nothing -> Nothing
     Nothing -> Nothing    
  

makepaths :: AbstractionGraph ->  [Descr] -> ([Descr],[Descr]) -> Maybe [(Descr,Descr,String)]
makepaths g node_list (inEdges,outEdges) =
  -- try to lookup the edges of the node
  case (sequence (map (\ed -> lookup ed (edges g)) inEdges),sequence (map (\ed -> lookup ed (edges g)) outEdges)) of
    (Just ie, Just oe) -> 
       -- try to make paths out of them
       case sequence (map (makepathsaux g node_list) (specialzip ie oe)) of
          -- return the paths
          Just paths -> Just (concat paths)
          Nothing -> Nothing
    (Nothing,_) -> Nothing
    (_,Nothing) -> Nothing
    

makepathsaux :: AbstractionGraph -> [Descr] -> ((Descr,Descr,String,DaVinciArc(String,Int)),(Descr,Descr,String,DaVinciArc(String,Int))) -> Maybe [(Descr,Descr,String)]
makepathsaux g node_list ((s1,t1,ty1,ed1),(s2,t2,ty2,ed2)) =
  -- try to determine the type of the path
  case determineedgetype g (ty1,ty2) of
    -- return the checked path
    Just ty -> checkpath g node_list (s1,t2,ty,ed1)  -- ed1 dient hier nur als Dummiewert
    Nothing -> Nothing


-- check, if either the source or the target of an edge or both are element of the list of nodes that are to be hidden
-- if so, find out the "next" sources/targets and check again
checkpath :: AbstractionGraph -> [Descr] -> (Descr,Descr,String,DaVinciArc(String,Int)) -> Maybe [(Descr,Descr,String)]
checkpath g node_list path@(src,tgt,ty,ed)
--  | (elem src node_list)&&(elem tgt node_list) = map (checkpath g node_list) ...
  | elem src node_list = -- try to determine the in- and outgoing edges of the source node
                         case fetchEdgesOfNode g src of
                           -- try to lookup ingoing edges
                           Just (inEdges,outEdges) -> case sequence (map (\ed -> lookup ed (edges g)) inEdges) of
			                                -- try to make paths of these edges and the "tail" of the path
			                                Just el -> case sequence (map (makepathsaux g node_list) (specialzip el [path])) of
							             -- recursively check the new paths
							             Just p -> case sequence (map (checkpath g node_list) [(s,t,tp,ed)|(s,t,tp) <- concat p]) of
								                 Just ps -> Just (concat ps)
									         Nothing -> Nothing
								     Nothing -> Nothing
						        Nothing -> Nothing
  
  | elem tgt node_list = -- try to determine the in- and outgoing edges of the target node
                         case fetchEdgesOfNode g tgt of
			   -- try to lookup the outgoing edges
                           Just (inEdges,outEdges) -> case sequence (map (\ed -> lookup ed (edges g)) outEdges) of
			                                -- try to make paths of these edges and the "init" of the path
			                                Just el -> case sequence (map (makepathsaux g node_list) (specialzip [path] el)) of
							             -- recursively check the new paths
							             Just p -> case sequence (map (checkpath g node_list) [(s,t,tp,ed)|(s,t,tp) <- concat p]) of
								                 Just ps -> Just (concat ps)
									         Nothing -> Nothing
										 --Just (concat p)
								     Nothing -> Nothing
							Nothing -> Nothing
			   Nothing -> Nothing
  | otherwise = Just [(src,tgt,ty)]
  

addpaths :: Descr -> [(Descr,Descr,String)] -> GraphInfo -> IO Result -- ([IO Result],Descr)
addpaths gid [] (gs,ev_cnt) = return (Result (gs,ev_cnt) ev_cnt Nothing) -- richtiger Descriptor?
addpaths gid ((src,tgt,ty):newEdges) (gs,ev_cnt) = do edge@(Result inf d err) <- addlink gid ty "" src tgt (gs,ev_cnt)
                                                      case err of
						        Nothing -> do let gs' = fst inf
							              addpaths gid newEdges (gs',ev_cnt+1) -- oder (gs', d) ??
							Just t -> return edge	   

-- 00000000000000000000000000000000000000000000
-- fetches all the nodes of the given type and hides them using hidenodes
hidenodetype :: Descr -> String -> GraphInfo -> IO Result
hidenodetype gid nodetype (gs,ev_cnt) = fetch_graph gid (gs,ev_cnt) False (\g ->
                                           -- check if the node type is valid
                                           do case lookup nodetype (nodeTypes g) of
                                               Just nt ->
                                                 do (Result info de err) <- hidenodes gid [descr|(descr,(tp,_)) <- (nodes g), tp == nodetype] (gs,ev_cnt)
					            return (snd (get gid (fst info)), de, err)
					       Nothing -> return (g,0,Just ("hidenodetype: illegal node type: "++nodetype))

                                        )

-- 00000000000000000000000000
-- ##########################

-- like hidenodes, but replaces the hidden nodes by a new node
-- with a menu to unhide the nodes
abstractnodes :: Descr -> [Descr] -> GraphInfo -> IO Result
abstractnodes gid node_list (gs,ev_cnt) = 
    fetch_graph gid (gs,ev_cnt) False (\g ->
      case sequence (map (\nd -> lookup nd (nodes g)) node_list) of
        Just nl -> case sequence (map (fetchEdgesOfNode g) node_list) of
	             Just el -> do let oldEdges = nub ((concat (map fst el))++(concat (map snd el)))
		                   (Result info de err) <- replaceByAbstractNode gid node_list nl oldEdges (gs,ev_cnt)
				   case err of
				     Nothing -> do (Result inf d er) <- hideedgesaux gid oldEdges info
				                   case er of
						     Nothing -> do (Result infor des erro) <- hidenodesaux gid node_list inf
						                   case erro of
								     Nothing -> do let g' = snd (get gid (fst infor))
								     
-- @@@ hier am Dienstag nach Ostern weiter machen
								                   let nn = [nd| nd <- nodes g', notElem nd (nodes g)]
										   let on = [nd| nd <- nodes g, notElem nd (nodes g')]
								                   let ne = [ed| ed <- edges g', notElem ed (edges g)]
										   let oe = [ed| ed <- edges g, notElem ed (edges g')]
								                   let newEntry = createEntry nn on ne oe (des+1) -- der Deskriptor, der in einem Result zurueckgegeben wird, ist doch der Deskriptor fuer das vorherige Ereignis, oder?
								                   return (g'{eventTable=newEntry:eventTable g'},des+1,Nothing)
								     Just t -> return (g,0,Just ("abstractnodes: error hiding nodes: "++t))
						     Just t -> return (g,0,Just ("abstractnodes: error hiding edges: " ++ t))
				     Just t -> return (g,0,Just ("abstractnodes: error making abstract node: "++ t))
		     Nothing -> return (g,0,Just "abstractnodes: error fetching the edges of the nodes")
      	Nothing -> return (g,0,Just "abstractnodes: unknown nodes")
    )
instance Eq (DaVinciNode (String, Int)) where
    (==) = eq1
    
instance Eq (DaVinciArc (String, Int)) where
    (==) = eq1
    
-- noch nicht fertig: newEntry fehlt
replaceByAbstractNode :: Descr -> [Descr] -> [(String,DaVinciNode(String,Int))] -> [Descr] -> GraphInfo -> IO Result
replaceByAbstractNode gid node_list nl edge_list (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case sequence (map (\ed -> lookup ed (edges g)) edge_list) of
      Just el -> do (Result inf d er) <- addnode gid "ABSTRACT" "" (gs,ev_cnt)
                    case er of
                      Nothing -> do let newEdges = [(src,d,tp)| (src,tgt,tp,_) <- el, ((notElem src node_list) && (elem tgt node_list))]
	                                             ++ [(d,tgt,tp)| (src,tgt,tp,_) <- el, ((elem src node_list) && (notElem tgt node_list))]
	                            (Result info de err) <- addpaths gid (nub newEdges) inf
                                    case err of
                                      Nothing -> do let g' = snd (get gid (fst info))
			                            return (g',de,err)
                                      Just t -> return (g,0,err)
	              Just text -> return (g,0,Just ("replaceByAbstractNode: error creating abstract node: "++text))
      Nothing -> return (g,0, Just "replaceByAbstractNode: error looking up the edges of the nodes")
  )
  

hideedges :: Descr -> [Descr] -> GraphInfo -> IO Result
hideedges gid edge_list (gs,ev_cnt) = fetch_graph gid (gs,ev_cnt) False (\g ->
                                        -- check if all of the edges exist
                                        case sequence (map (\edge -> lookup edge (edges g)) edge_list) of
                                           Just el -> do -- try to hide them
					                 (Result info descr err) <- hideedgesaux gid edge_list (gs,ev_cnt)
					                 case err of
							   Nothing -> do -- make an entry in the eventTable
							                 let g' = snd (get gid (fst info))
							                 let oe = [ed|ed@(descr,_) <- (edges g), notElem descr (map fst (edges g'))]
							                 let newEntry = createEntry [] [] [] oe (descr+1)
									 return (g'{eventTable = newEntry:eventTable g'},descr+1,Nothing)
							   Just text -> return (g,0,Just ("hideedges: error hiding edges: "++text))
                                           Nothing -> return (g,0,Just "hideedges: unknown edges")
                                      )

-- an auxiliary function used by hideedges and hidenodes; actually does the hiding of the edges
hideedgesaux :: Descr -> [Descr] -> GraphInfo -> IO Result
hideedgesaux gid [] (gs,ev_cnt) = return (Result (gs,ev_cnt) ev_cnt Nothing) -- richtiger Descriptor?
hideedgesaux gid (d:delEdges) (gs,ev_cnt) = do dle@(Result info descr err) <- dellink gid d (gs,ev_cnt)
                                               case err of
					         Nothing -> do let gs' = fst info
						               hideedgesaux gid delEdges (gs',ev_cnt+1) -- oder (gs', descr) ??
						 Just t -> return dle
{-
                                                      rest <- addpaths gid newEdges (gs',ev_cnt+1) -- oder (gs', d) ??
                                                      return (edge:rest)
     -} 

-- fetches all the edges of the given type and hides them using hideedges
hideedgetype :: Descr -> String -> GraphInfo -> IO Result
hideedgetype gid edgetype (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    -- check if the edge type is valid
    case lookup edgetype (edgeTypes g) of
      Just et -> do (Result info de err) <- hideedges gid [descr|(descr,(_,_,tp,_)) <- (edges g), tp == edgetype] (gs,ev_cnt)
                    return (snd (get gid (fst info)), de, err)
      Nothing -> return (g,0,Just ("hideedgetype: illegal edge type: "++edgetype))
  )

showIt :: Descr -> Descr -> GraphInfo -> IO Result
showIt gid hide_event (gs,ev_cnt) =
  fetch_graph gid (gs,ev_cnt) False (\g ->
    case lookup hide_event (eventTable g) of
      Just entry -> do (Result info1 de1 err1) <- hideedgesaux gid (map fst (newEdges entry)) (gs,ev_cnt) -- kanten verstecken
                       case err1 of
		         Nothing -> do (Result info2 de2 err2) <- shownodes gid (oldNodes entry) info1 -- knoten erzeugen
			               case err2 of
				         Nothing -> do (Result info3 de3 err3) <- hidenodesaux gid (map fst (newNodes entry)) info2 --knoten verstecken
					               case err3 of
						         Nothing -> do (Result info4 de4 err4) <- showedges gid (oldEdges entry) info3 -- kanten erzeugen
							               case err4 of
								         Nothing -> do let g' = snd (get gid (fst info4))
									               return (g'{eventTable = remove hide_event (eventTable g')},0,Nothing) --entry loeschen
									 Just t4 -> return (g,0,Just ("showIt: error restoring old edges: "++t4))
							 Just t3 -> return (g,0,Just ("showIt: error removing nodes: "++t3))
					 Just t2 -> return (g,0,Just ("showIt: error restoring nodes: "++t2))
			 Just t1 -> return (g,0,Just ("showIt: error removing edges: "++t1))
      Nothing -> return (g,0,Just ("showIt: invalid event descriptor: "++(show hide_event)))
  )


-- wie kann ich auf den Namen des Knoten zugreifen, der im DaVinciNode steht??

shownodes :: Descr -> [(Descr,(String,DaVinciNode (String,Int)))] -> GraphInfo -> IO Result
shownodes gid [] (gs,ev_cnt) = return (Result (gs,ev_cnt) ev_cnt Nothing)
shownodes gid ((node@(d,(tp,davincinode))):list) (gs,ev_cnt) = 
  do let g = snd (get gid gs)
     value <- (getNodeValue (theGraph g) davincinode)
     nd@(Result info de error) <- addnode gid tp (fst value) (gs,d)
     case error of
       Nothing -> do let g' = snd (get gid (fst info)) 
	             shownodes gid list (gs,ev_cnt)
       Just _ -> return nd

									      

-- wie kann ich auf den Namen der Kante zugreifen, der im DaVinciArc steht??
  
showedges :: Descr -> [(Int,(Int,Int,String,DaVinciArc (String,Int)))] -> GraphInfo -> IO Result
showedges gid [] (gs,ev_cnt) = return (Result (gs,ev_cnt) ev_cnt Nothing)
showedges gid ((edge@(d,(src,tgt,tp,davinciarc))):list) (gs,ev_cnt) =
  do let g = snd (get gid gs)
     value <- (getArcValue (theGraph g) davinciarc)
     ed@(Result info de err) <- addlink gid tp (fst value) src tgt (gs,ev_cnt)
     case err of
       Nothing -> do let g' = snd (get gid (fst info))
	             showedges gid list (gs,ev_cnt)
       Just _ -> return ed





-- showIt = undefined
-- hideedges = undefined
-- abstractnodes = undefined