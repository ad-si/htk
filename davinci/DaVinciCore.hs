{- #########################################################################

MODULE        : DaVinciCore
AUTHOR        : Carla Blanck Purper, 
                Einar W. Karlsen 
                University of Bremen
                email:  {ewk,cpurper}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : 

Encapsulation of the DaVinci core. Provides a cache for the state of
the DaVinci tools and its graphs. Also ensures  that commands are
delegateDavd onwards to the external child process.

Each object (DaVinci or Graph) is represented as a protected variable.
Thus, the API is MT-Safe.

Graph operations always acquires the graph first, then davinci. This
way it is ensured that at most one thread can change the graph at any
time. Operations on distinct graph are furthermore ensured to be
exclusive, once they attempt to flush the command to davinci. Flushing
a set-context command and the graph command proper are ensured to be
atomic operations.

The mentioned locking hierarchy must be obeyed. Deadlocks may occur if
the DaVinci handle is locked first floowed by a graph lock within the
cirtical section of daVinci.


   ######################################################################### -}


module DaVinciCore (
   DaVinci(..),
   withDaVinci,
   withDaVinciOneWay,
   
   getGraphs,
   
   Dispatcher,
   
   Graph,
   createGraph,
   withGraph,
   withGraphOneWay,
   redrawGraph,
   closeGraph,
   cleanupGraph,
   fDaVinci,
   fGraphObj,
   fRegister,
   newType,
   delType,
   getType,
   typeNotFound,
   
   getNodeSelection,
   setNodeSelection,
   getEdgeSelection,
   setEdgeSelection,
   
   EventID,
   EventDesignator(..),
   
   listenDaVinci,
   
   Node(..),
   EdgeId(..),
   Edge(..),
   NodeId(..),
   newNodeId,
   newEdgeId,
   
   createNode,
   createEdge,
   getNode,
   getEdge,
   setNodeAttr,
   getNodeAttr,
   setEdgeAttr,
   getEdgeAttr,
   delNode,
   delEdge,
   
   existNode, 
   existEdge,
   getIncoming,
   getOutgoing,
   getSource,
   getTarget,
   getNodes,
   getEdges,
   
   nodeNotFound,
   edgeNotFound
   
   ) where

import List
import IO

import qualified Exception
import qualified Posix
import qualified IOExts(unsafePerformIO)
import FiniteMap

import qualified ExtendedPrelude
import WBFiles
import Object
import Debug(debug)

import SIM
import ChildProcess

import GUICore                         

import DaVinciGraphTerm
import DaVinciEvent
import DaVinciActions


-- ---------------------------------------------------------------------------
-- DaVinci Data Definition & instantiations
-- ---------------------------------------------------------------------------

data DaVinci = 
   DaVinci {
      fDispatcher     :: (Dispatcher DaVinciEventInfo),
      fDaVinciState   :: (PVar DaVinciState),
      fSession        :: GUIOBJECT 
         -- only used to construct an event designator for DaVinci events,
         -- so far as I know.
      }

data DaVinciState = 
   DaVinciState {
      fContextID :: (Maybe ContextID),
      fGraphs    :: [Graph]
      }

type ContextID = Int

-- ---------------------------------------------------------------------------
-- Graph Data Definition & instantiations
-- ---------------------------------------------------------------------------

data Graph = 
   Graph {
      fDaVinci    :: DaVinci,
      fGraphObj   :: GUIOBJECT, 
         -- The object id number also identifies the graph context. 
      fGraphState :: (RVar GraphState),
      fTypes      :: (PVar (FiniteMap TypeId GUIOBJECT)),
      fRegister   :: DaVinciActions.Register
      }

data GraphState =
   GraphState {
      fNextId         :: Int,
      fNodeUpd        :: [NodeUpd],
      fEdgeUpd        :: [EdgeUpd],
      fAttrUpd        :: [AttrUpd],
      fNodes          :: [Node],
      fLinks          :: [Link],
      fMenus          :: [MenuId],
      fNodeSelection  :: [Node],
      fEdgeSelection  :: [Edge]
      }


type Link = (Edge, Node, Node)  -- (edge, source, target)

type MenuId = String

emptyGraphState = GraphState 0 [] [] [] [] [] [] [] []

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance Object DaVinci where
   objectID daVinci = objectID(fSession daVinci)
 
instance SingleInstanceTool DaVinci where 
   getToolInstance = getDaVinciHandle

instance Destructible DaVinci where
   destroy daVinci = shutdownDaVinci daVinci 
   destroyed daVinci = 
            destroyed (fDispatcher daVinci)
         +> disconnected daVinci
      where 
         disconnected :: DaVinci -> IA ()
         disconnected daVinci =  
            (listenDaVinci (daVinci,DisConnect) :: IA DaVinciEventInfo) >>> 
               (do 
                  try (destroy daVinci)
                  done
                )

instance Tool DaVinci where
   getToolStatus daVinci = getToolStatus (fDispatcher daVinci)

instance UnixTool DaVinci where
   getUnixProcessID daVinci = getUnixProcessID (fDispatcher daVinci)

instance CommandTool DaVinci where
   evalCmd cmd daVinci = evalCmd cmd (fDispatcher daVinci)
   execCmd cmd daVinci = execCmd cmd (fDispatcher daVinci)
   execOneWayCmd cmd daVinci = execOneWayCmd cmd (fDispatcher daVinci)

instance Object Graph where 
   objectID g = objectID(fGraphObj g)

instance Eq Graph where
    g1 == g2 = (objectID g1) == (objectID g2)

instance Ord Graph where
    g1 <= g2 = (objectID g1) <= (objectID g2)

instance Synchronized Graph where
    synchronize g = synchronize (fGraphObj g)


-- ---------------------------------------------------------------------------
--  Edge & Node Type
-- ---------------------------------------------------------------------------

data Edge = Edge {
                fEdgeContext :: Graph,
                fEdgeTypeId  :: TypeId,
                fEdgeId      :: EdgeId,
                fEdgeObject  :: GUIOBJECT
                }

data Node = Node {
                fNodeContext :: Graph,
                fNodeTypeId  :: TypeId,
                fNodeId      :: NodeId,
                fNodeObject  :: GUIOBJECT
                }

instance Eq Node where
        n1 == n2 = (fNodeObject n1) == (fNodeObject n2)

instance Ord Node where
        n1 <= n2 = (fNodeObject n1) <= (fNodeObject n2)

instance Eq Edge where
        e1 == e2 = (fEdgeObject e1) == (fEdgeObject e2)

instance Ord Edge where
        e1 <= e2 = (fEdgeObject e1) <= (fEdgeObject e2)

-- ---------------------------------------------------------------------------
--  Startup and Shutdown DaVinci
-- ---------------------------------------------------------------------------

startDaVinci :: IO DaVinci
startDaVinci = 
   do
      contextVar <- newPVar 0 -- passed to the DaVinci dispatcher
      msgQueue <- newMsgQueue
      toolName <- getWBToolFilePath "daVinci"
      daVinciDispatcher <- 
         newDispatcher toolName [arguments ["-pipe"]] finaliser 
            (handleEvent msgQueue)
      session <- newAbstractGUIObject
      daVinciState <- newPVar (
         DaVinciState{
            fContextID = Nothing,
            fGraphs = []
            })
      daVinci <- return( 
         DaVinci{
            fDispatcher = daVinciDispatcher,
            fDaVinciState = daVinciState,
            fSession = session
            })
      forkIO(dispatcher daVinci msgQueue contextVar True)
      return daVinci
   where
      finaliser :: Dispatcher DaVinciEventInfo -> IO ()
      finaliser = execOneWayCmd "menu(file(exit))" 


-- ---------------------------------------------------------------------------
--  Tool Termination
-- ---------------------------------------------------------------------------

shutdownDaVinci :: DaVinci -> IO ()
shutdownDaVinci daVinci = 
   do                                      
      graphs <- 
         updVar (fDaVinciState daVinci) 
            (\ daVinciState -> 
               do
                  destroy (fDispatcher daVinci)
                  destroy (fSession daVinci) 
                  return (DaVinciState Nothing [],fGraphs daVinciState)
               )
      foreach graphs cleanupGraph
      done

-- ---------------------------------------------------------------------------
--  Sending DaVinci Commands
-- ---------------------------------------------------------------------------

withDaVinci :: IO String -> IO ()
withDaVinci commandAct =
   do
      daVinci <- getDaVinciHandle
      withVar (fDaVinciState daVinci) 
         (\ _ -> 
            do 
               command <- commandAct
               result <- evalCmd command daVinci
               return result
            )
      done

withDaVinciOneWay :: IO String -> IO ()
withDaVinciOneWay commandAct = 
   do
      daVinci <- getDaVinciHandle
      withVar (fDaVinciState daVinci) 
         (\ _ -> 
            do
               command <- commandAct
               execOneWayCmd command daVinci
            )
      done

-- ---------------------------------------------------------------------------
-- TYPES/RULES
-- ---------------------------------------------------------------------------

newType :: Graph -> Maybe String -> IO (TypeId,GUIOBJECT)
newType g mtnm = do {
        o <- newAbstractGUIObject;
        tnm <- return (getTypeId mtnm o);
        changeVar' (fTypes g) (\fm -> addToFM fm tnm o);
        return (tnm,o)
        } where getTypeId Nothing o    = (TypeId . show . objectID) o
                getTypeId (Just tnm) _ = TypeId tnm

delType :: Graph -> TypeId -> IO ()
delType g tnm = do {changeVar' (fTypes g) (\fm -> delFromFM fm tnm); done}


getType :: Graph -> TypeId -> IO GUIOBJECT
getType g tnm = do {
        mtp <- withVar' (fTypes g) (\fm -> lookupFM fm tnm);
        case mtp of
                Nothing -> raise typeNotFound
                (Just t) -> return t
        }

typeNotFound :: IOError
typeNotFound = userError "daVinci type not found"


-- ---------------------------------------------------------------------------
-- GRAPHS
-- ---------------------------------------------------------------------------

createGraph :: IO Graph
createGraph = 
   do 
      daVinci <- getDaVinciHandle
      graphObj <- createGUIObject GRAPH defMethods
      graphState <- newRVar emptyGraphState
      types <- newPVar emptyFM
      register <- newRegister

      graph <- 
         return (Graph {
            fDaVinci = daVinci,
            fGraphObj = graphObj,
            fGraphState = graphState,
            fTypes = types,
            fRegister = register
            })
      newType graph (Just "")
      insertGraph daVinci graph
      openContext (contextID graph)
      return graph
   where
      insertGraph :: DaVinci -> Graph -> IO ()
      insertGraph daVinci graph = 
         changeVar' (fDaVinciState daVinci) 
            (\ daVinciState -> 
               daVinciState {fGraphs = graph:(fGraphs daVinciState)}
               )

withGraph ::  Graph -> IO String -> IO Graph
withGraph graph commandAct = 
   synchronize graph (
      do
         changeVar (fDaVinciState daVinci) 
            (\ daVinciState -> 
               do
                  command <- commandAct
                  newContext <- 
                     changeContext daVinci (fContextID daVinciState) graph
                  evalCmd command daVinci
                  return (daVinciState {fContextID = Just newContext})
               )
         return graph
      )
   where 
      daVinci = fDaVinci graph

withGraphOneWay ::  Graph -> IO String -> IO Graph
withGraphOneWay graph commandAct = 
   synchronize graph (
      do
         changeVar (fDaVinciState daVinci) 
            (\ daVinciState -> 
               do
                  command <- commandAct
                  newContext <- 
                     changeContext daVinci (fContextID daVinciState) graph
                  execOneWayCmd command (fDispatcher daVinci)
                  return (daVinciState {fContextID = Just newContext})
               )
         return graph
      )
   where 
      daVinci = fDaVinci graph

getGraphs :: DaVinci -> IO [Graph]
getGraphs daVinci = withVar' (fDaVinciState daVinci) fGraphs

lookupByContext :: DaVinci -> ContextID -> IO Graph
lookupByContext daVinci contextID =
   do
     
      let
         thisObjectID = ObjectID contextID
      daVinciState <- getVar (fDaVinciState daVinci)
      let
         graphs = fGraphs daVinciState
      case find (\ graph -> objectID graph == thisObjectID ) graphs of
         Just graph -> return graph
         Nothing -> ioError (userError "DaVinciCore.lookupByContext")

-- ---------------------------------------------------------------------------
-- Redisplay
-- ---------------------------------------------------------------------------

redrawGraph :: Graph -> IO ()
redrawGraph graph = 
   synchronize graph (
       changeVar (fGraphState graph)
          (\ graphState -> 
             do
                let
                   daVinci = fDaVinci graph
                changeVar (fDaVinciState daVinci) 
                   (\ daVinciState -> 
                      do
                         newContext <- changeContext daVinci 
                            (fContextID daVinciState) graph
                         flushCmds (fNodeUpd graphState) 
                            (fEdgeUpd graphState) (fAttrUpd graphState) 
                            daVinci
                         return (daVinciState{fContextID = Just newContext})
                      )
                return (graphState{fNodeUpd = [],fEdgeUpd = [],fAttrUpd = []})
             ))


flushCmds [] [] [] dav = done
flushCmds nupd eupd aupd dav = do {
        evalCmd (updGraph (reverseNL nupd) (reverseEL eupd) (reverse aupd)) dav;
        done
} where reverseNL [] = []
        reverseNL ((NodeUpd nid tid attrs) : nl)=  
                 (reverseNL nl) ++ [NodeUpd nid tid (reverse attrs)]
        reverseNL (delnode : nl)=  
                 (reverseNL nl) ++ [delnode]
        reverseEL [] = []
        reverseEL ((EdgeUpd eid tid attrs srg trg) : nl)=  
                 (reverseEL nl) ++ [EdgeUpd eid tid (reverse attrs) srg trg]
        reverseEL (deledge : nl)=  
                 (reverseEL nl) ++ [deledge]

        updGraph :: [NodeUpd] -> [EdgeUpd] -> [AttrUpd] -> String
        updGraph [] [] [] = []
        updGraph x y a  = 
                ("graph(update_and_change_attr("++ show x ++","++ show y
                      ++ "," ++ show a ++ "))")


-- ---------------------------------------------------------------------------
-- Changing Context
-- ---------------------------------------------------------------------------

changeContext ::  DaVinci -> Maybe ContextID -> Graph -> IO ContextID
changeContext dav _ g = setContext dav (contextID g)

setContext ::  DaVinci -> ContextID -> IO ContextID
setContext dav cid = do {
        evalCmd ("multi(set_context("++ show (show cid) ++"))") dav;
        return cid
}


openContext :: ContextID -> IO ()
openContext context = 
   withDaVinci (return ("multi(open_context("++show (show context)++"))"))


contextID :: Graph -> ContextID 
contextID graph = 
   let 
      ObjectID objectId = objectID graph 
   in
      objectId

-- ---------------------------------------------------------------------------
-- Close Graph
-- ---------------------------------------------------------------------------

closeGraph :: Graph -> IO ()
closeGraph g = 
   changeVar graphState (\gs -> do {
        changeVar (fDaVinciState dav) ( \dst -> do {
                cntx' <- changeContext dav (fContextID dst) g;
                try(execOneWayCmd "menu(file(close))" dav);
                return (dst{    fContextID = Nothing, -- Just cntx',
                                fGraphs = filter (/= g) (fGraphs dst)
                                })
                });
        return emptyGraphState
        })
 where  dav  = fDaVinci g
        graphState  = fGraphState g
                


cleanupGraph :: Graph -> IO ()
cleanupGraph g = changeVar graphState (\gs -> do {
        changeVar' (fDaVinciState (fDaVinci g)) ( \dst -> 
                dst{fGraphs = filter (/= g) (fGraphs dst)});
        return emptyGraphState
        }) 
 where  graphState = fGraphState g
                


-- ---------------------------------------------------------------------------
--  Creating New Nodes and Edges
-- ---------------------------------------------------------------------------

newNodeId :: Graph -> Maybe String -> IO NodeId
newNodeId g mnid = getNewId mnid g >>= return . NodeId


createNode :: Graph -> Node -> IO ()
createNode g n@(Node _ tid nid _) =  changeVar' (fGraphState g) (\gs -> 
        gs{fNodes = n:(fNodes gs), fNodeUpd = nupd : (fNodeUpd gs)})
 where  nupd = NodeUpd nid tid []


newEdgeId :: Graph -> Maybe String -> IO EdgeId
newEdgeId g meid = getNewId meid g >>= return . EdgeId


createEdge :: Graph -> Edge -> Node -> Node -> IO ()
createEdge g e@(Edge _ tid eid _) src trg = changeVar' graphState (\gs ->
        gs{fLinks = (e,src,trg) : (fLinks gs), fEdgeUpd = eupd : (fEdgeUpd gs)})
  where eupd = EdgeUpd eid tid [] (fNodeId src) (fNodeId trg)
        graphState  = fGraphState g



getNewId :: Maybe String -> Graph -> IO String
getNewId Nothing g = updVar' (fGraphState g) (\gs ->
           let no' = fNextId gs + 1 
               nid = show no' 
           in (gs{fNextId = no'},nid)
        )
getNewId (Just str) g  = return str




-- ---------------------------------------------------------------------------
--  Looking up nodes and edges
-- ---------------------------------------------------------------------------

getNode :: Graph -> NodeId -> IO Node
getNode g nid  = do {
        nl <- getNodes g;
        case (filter (\(Node _ _ nid2 _) -> nid == nid2 ) nl) of
                [] -> raise nodeNotFound
                n:_ ->  return n                
        }
        
getEdge :: Graph -> EdgeId -> IO Edge
getEdge g eid  = do {
        el <- getEdges g;
        case (filter (\(Edge _ _ eid2 _) -> eid == eid2 ) el) of
                []  -> raise edgeNotFound
                e:_ -> return e 
        }
        

-- ---------------------------------------------------------------------------
--  Changing the Graph
-- ---------------------------------------------------------------------------

delNode :: Node -> IO ()
delNode n@(Node g _ (nid @ (NodeId str)) _)  = synchronize g (do {
        eupd <- withVar' (fGraphState g) fEdgeUpd;
        ls1 <- getIncoming n;                   -- local effect only
        ls2 <- getOutgoing n;                   -- local effect only
        foreach ls1 delEdge;
        foreach ls2 delEdge;    
        changeVar' (fGraphState g) (\gs -> 
                gs{
                        fEdgeUpd        = eupd,   -- ignore the upd commands
                        fNodes          = filter (/= n) (fNodes gs),
                        fNodeUpd        = (NodeDel nid): (fNodeUpd gs)
                        }
                )
        })
        

delEdge :: Edge -> IO ()
delEdge e@(Edge g _ (eid @ (EdgeId str)) _) =
        changeVar' (fGraphState g) (\gs -> gs {
                fEdgeUpd = (EdgeDel eid) : (fEdgeUpd gs),
                fLinks   = filter (\(e2,s,t) -> e /= e) (fLinks gs)     
                })



-- ---------------------------------------------------------------------------
--  Setting and Retrieving Node and Edge Attributes
-- ---------------------------------------------------------------------------


setNodeAttr :: GUIValue a => Node -> ConfigID -> a -> IO ()
setNodeAttr (Node g _ nid@(NodeId oid) gobj) cid val = 
   synchronize gobj (do {
        setConfigValue gobj cid val';           
        changeVar' (fGraphState g) (\gs ->
            addNodeAttrCmd gs (fNodeUpd gs) (fAttrUpd gs) nid cid assoc)
        })
 where  val' = toGUIValue val
        assoc = AttrAssoc cid val'
              

addNodeAttrCmd gs ((NodeUpd nid' ntp attrs) : nupds) _ nid attr assoc
        | nid == nid' = 
        gs{fNodeUpd = (NodeUpd nid' ntp (assoc : attrs)) : nupds}
addNodeAttrCmd gs _ aupds nid cid assoc =
        gs{fAttrUpd = (NodeAttrUpd nid [assoc]) :aupds} 
        

setEdgeAttr :: GUIValue a => Edge -> ConfigID -> a -> IO ()
setEdgeAttr (Edge g _ eid@(EdgeId oid) gobj) cid val = 
   synchronize gobj (do {
        setConfigValue gobj cid val';           
        changeVar' (fGraphState g) (\gs ->
             addEdgeAttrCmd gs (fEdgeUpd gs) (fAttrUpd gs) eid assoc)
        })
 where  val' = toGUIValue val
        assoc = AttrAssoc cid val'


addEdgeAttrCmd gs ((EdgeUpd eid' etp attrs src trg) : eupds) aupds eid assoc
         | eid == eid' = 
        gs{fEdgeUpd = (EdgeUpd eid' etp (assoc : attrs) src trg) : eupds}

addEdgeAttrCmd gs _ aupds eid assoc =
        gs{fAttrUpd = (EdgeAttrUpd eid [assoc]) :aupds} 


getNodeAttr :: GUIValue a => Node -> ConfigID -> IO a
getNodeAttr (Node _ _ _ gobj) cid = do 
        mval <- getConfigValue gobj cid
        case mval of
                Nothing  -> return cdefault
                (Just v) -> return (fromGUIValue v)


getEdgeAttr :: GUIValue a => Edge -> ConfigID -> IO a
getEdgeAttr(Edge _ _ _ gobj) cid = do
        mval <- getConfigValue gobj cid
        case mval of
                Nothing  -> return cdefault
                (Just v) -> return (fromGUIValue v)


-- ---------------------------------------------------------------------------
--  Queries over Graph
-- ---------------------------------------------------------------------------

getIncoming :: Node -> IO [Edge]
getIncoming n =do {
        links <- getLinks (fNodeContext n);
        return (map (\(e,s,t) -> e) (filter (\(e,s,t) -> t == n) links))
        }


getOutgoing :: Node -> IO [Edge]
getOutgoing n = do {
        links <- getLinks (fNodeContext n);
        return (map (\(e,s,t) -> e) (filter (\(e,s,t) -> s == n) links))
        }


getSource :: Edge -> IO Node
getSource e1 = do {
        links <- getLinks (fEdgeContext e1);
        case (filter (\(e2,_,_) -> e2 == e1) links) of
                   [(e,s,t)]    -> return s
                   []           -> raise nodeNotFound
        }


getTarget :: Edge -> IO Node
getTarget e1 = do {
        links <- getLinks (fEdgeContext e1);
        case (filter (\(e2,_,_) -> e2 == e1) links) of
                   [(_,s,t)]    -> return t
                   []           -> raise nodeNotFound
        }


getNodes :: Graph -> IO [Node]
getNodes g  = withVar' (fGraphState g) fNodes 
        

getEdges :: Graph -> IO [Edge]
getEdges g  = withVar' (fGraphState g) ((map (\(e,_,_) -> e)) . fLinks)


getLinks :: Graph -> IO [Link]
getLinks g  = withVar' (fGraphState g) (fLinks)


existNode :: Node -> IO Bool
existNode n = do {
        nl <- getNodes (fNodeContext n); 
        return (elem n nl)
        }


existEdge :: Edge -> IO Bool
existEdge e = do {
        el <- getEdges (fEdgeContext e); 
        return (elem e el)
        }


nodeNotFound :: IOError
nodeNotFound = userError "node not found"

edgeNotFound :: IOError
edgeNotFound = userError "edge not found"




-- ---------------------------------------------------------------------------
-- Selection
-- ---------------------------------------------------------------------------

getNodeSelection :: Graph -> IO [Node]
getNodeSelection g = withVar' (fGraphState g) fNodeSelection


setNodeSelection :: Graph -> [Node] -> IO ()
setNodeSelection g ns = changeVar' (fGraphState g) (\gs -> 
        gs{fNodeSelection = ns, fEdgeSelection = []})

getEdgeSelection :: Graph -> IO [Edge]
getEdgeSelection g = withVar' (fGraphState g) fEdgeSelection


setEdgeSelection :: Graph -> [Edge] -> IO ()
setEdgeSelection g es = changeVar' (fGraphState g) (\gs -> 
        gs{fEdgeSelection = es, fNodeSelection = []})


-- ---------------------------------------------------------------------------
--  Event Instances
-- ---------------------------------------------------------------------------

instance EventDesignator (ObjectID,DaVinciEvent) where
        toEventID (oid,dev) =  EventID oid (show dev)

instance EventDesignator (Graph,DaVinciFileEvent) where
   toEventID (graph,dev) =  EventID (objectID graph) (show dev)

instance EventDesignator (ObjectID,DaVinciFileEvent) where
        toEventID (oid,dev) =  EventID oid (show dev)

instance EventDesignator (Graph,DaVinciEvent) where
        toEventID (g,dev) =  EventID (objectID g) (show dev)

instance EventDesignator (DaVinci,DaVinciEvent) where
   toEventID (daVinci,dev) =  EventID (objectID daVinci) (show dev)


listenDaVinci :: EventDesignator e => e -> IA DaVinciEventInfo
listenDaVinci e = interaction e reg unreg
 where  disp  = fDispatcher readDaVinciState
        eid   = toEventID e
        reg   = register disp eid Notice done
        unreg = deregister disp eid done


-- ---------------------------------------------------------------------------
-- EVENT HANDLER
-- ---------------------------------------------------------------------------

handleEvent :: MsgQueue DaVinciAnswer -> String -> Dispatcher DaVinciEventInfo -> IO ()
handleEvent msgQueue string _ = 
   do
      answer <- decodeDaVinciAnswer string     
      sendIO msgQueue answer
      case answer of
         (DaVinciAnswer DisConnect _) -> deadlock
         (DaVinciAnswer InternalError _) -> deadlock
         _ -> done


-- ---------------------------------------------------------------------------
-- EVENT DISPATHER
-- The MsgQueue in dispatcher comes from DaVinciEvent.
-- The PVar appears to be the current DaVinci context.
-- ---------------------------------------------------------------------------

-- dispatcher - delegateDav (which further calls dispatchDav events as
-- necessary) - dispatcher' is the main loop.

dispatcher :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int ->  Bool -> IO ()
dispatcher daVinci msgQueue contextVar ignoreOk = 
   do
      ans <- receiveIO msgQueue
      delegateDav daVinci msgQueue contextVar ignoreOk ans     
-- dispatcher' dav mq pv ans

dispatcher' :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int -> DaVinciAnswer
   -> IO ()
dispatcher'  daVinci msgQueue contextVar (DaVinciAnswer ComError _ ) = 
   dispatcher daVinci msgQueue contextVar True
dispatcher'  daVinci msgQueue contextVar _  = 
   dispatcher daVinci msgQueue contextVar False

delegateDav :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int ->  Bool 
   -> DaVinciAnswer -> IO ()
delegateDav daVinci msgQueue contextVar ignoreOk 
      ans@(DaVinciAnswer NodeSelectionLabels nodesel) = 
   do
      mans' <- sync(withTimeout (secs 0.25) (receive msgQueue))
      case mans' of
         Nothing -> 
            do 
               dispatchDav daVinci contextVar ignoreOk ans        
               dispatcher' daVinci msgQueue contextVar ans      
         (Just ans'@(DaVinciAnswer NodeDoubleClick _)) -> 
            do
               dispatchDav daVinci contextVar ignoreOk 
                  (DaVinciAnswer NodeDoubleClick nodesel)    
               dispatcher' daVinci msgQueue contextVar ans' 
         (Just ans') -> 
            do
               dispatchDav daVinci contextVar ignoreOk ans
               dispatchDav daVinci contextVar False ans'  
               dispatcher' daVinci msgQueue contextVar ans'
delegateDav daVinci msgQueue contextVar ignoreOk
         ans@(DaVinciAnswer EdgeSelectionLabel edgesel) =
   do
      mans' <- sync(withTimeout (secs 0.25) (receive msgQueue))
      case mans' of
         Nothing -> 
            do
               dispatchDav daVinci contextVar ignoreOk ans        
               dispatcher' daVinci msgQueue contextVar ans
         (Just ans'@(DaVinciAnswer EdgeDoubleClick _)) -> 
            do
               dispatchDav daVinci contextVar ignoreOk 
                  (DaVinciAnswer EdgeDoubleClick  edgesel)   
               dispatcher' daVinci msgQueue contextVar ans'
         (Just ans') -> 
            do
               dispatchDav daVinci contextVar ignoreOk ans
               dispatchDav daVinci contextVar False ans'  
               dispatcher' daVinci msgQueue contextVar ans'
         
delegateDav daVinci msgQueue contextVar ignoreOk ans = 
   do 
      dispatchDav daVinci contextVar ignoreOk ans        
      dispatcher' daVinci msgQueue contextVar ans

dispatchDav :: DaVinci -> PVar Int -> Bool -> DaVinciAnswer -> IO ()
dispatchDav daVinci contextVar alreadyReplied answer =
   case answer of
      DaVinciAnswer Ok (Reply str) -> 
         unless alreadyReplied (sR "dd1" (Right str) disp)
      DaVinciAnswer ComError _ -> 
         sR "dd2" (Left daVinciFailure) disp
      DaVinciAnswer DisConnect _ -> 
         sendEvent (daVinci, DisConnect) NoDaVinciEventInfo
      DaVinciAnswer Close _ ->
         do 
            context <- getVar contextVar
            setVar contextVar 0
            sendEvent (ObjectID context, Close) NoDaVinciEventInfo
      DaVinciAnswer ContextChanged (Context context) ->
         setVar contextVar context
      DaVinciAnswer NodeSelectionLabels nodesel ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,NodeSelectionLabels) nodesel
      DaVinciAnswer EdgeSelectionLabel edgesel -> 
         do
            context <- getVar contextVar
            sendEvent (ObjectID context,EdgeSelectionLabel) edgesel
      DaVinciAnswer NodeDoubleClick nodesel ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,NodeDoubleClick) nodesel
      DaVinciAnswer EdgeDoubleClick edgesel -> 
         do
            context <- getVar contextVar
            sendEvent (ObjectID context,EdgeSelectionLabel) edgesel
      DaVinciAnswer IconSelection (MenuInvocation menuItemId) -> 
         do
            context <- getVar contextVar
            graph <- lookupByContext daVinci context
            invokeGlobalEvent (fRegister graph) menuItemId 
      DaVinciAnswer MenuSelection (MenuInvocation menuItemId) ->
         do 
            context <- getVar contextVar
            graph <- lookupByContext daVinci context
            invokeGlobalEvent (fRegister graph) menuItemId
      DaVinciAnswer FileMenuSelection (FileMenuInvocation m) ->  
         do
            context <- getVar contextVar
            sendEvent (ObjectID context, m) NoDaVinciEventInfo
      DaVinciAnswer InternalError (Reply str) -> 
         do
            debug "DDAV2"
            debug str
            sendEvent (daVinci,DisConnect) NoDaVinciEventInfo
      DaVinciAnswer PopupSelectionNode 
            inf@(PopupSelectionNodeInf nodeId menuItemId) ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,PopupSelectionNode) inf
            graph <- lookupByContext daVinci context
            invokeNodeEvent (fRegister graph) nodeId menuItemId
      DaVinciAnswer PopupSelectionEdge 
            inf@(PopupSelectionEdgeInf edgeId menuItemId) ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,PopupSelectionEdge) inf
            graph <- lookupByContext daVinci context
            invokeEdgeEvent (fRegister graph) edgeId menuItemId
      DaVinciAnswer CreateNode nodesel -> 
         do
            context <- getVar contextVar
            sendEvent (ObjectID context,CreateNode) nodesel
      DaVinciAnswer CreateNodeAndEdge nodesel ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,CreateNodeAndEdge) nodesel
      DaVinciAnswer CreateEdge nodesel ->
         do 
            context <- getVar contextVar
            sendEvent (ObjectID context,CreateEdge) nodesel
   where
      disp = fDispatcher daVinci
      sendEvent event extraInfo = dispatch disp event extraInfo done

      sR label message disp =
         do
            res <- Exception.tryAllIO (sendReply message disp)
            case res of 
               Left error ->
                  do
                     debug ("Had to discard reply in "++label)
                     debug message
                     debug error
               Right _ -> done

-- --------------------------------------------------------------------------
--  DaVinci State
-- --------------------------------------------------------------------------

readDaVinciState :: DaVinci
readDaVinciState = IOExts.unsafePerformIO startDaVinci

getDaVinciHandle :: IO DaVinci
getDaVinciHandle =  return readDaVinciState


-- --------------------------------------------------------------------------
--  DaVinci Exceptions
-- --------------------------------------------------------------------------

daVinciFailure :: IOError
daVinciFailure = userError "request to daVinci failed"
     
