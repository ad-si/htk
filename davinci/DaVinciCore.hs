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
        Object(..),
        Tool(..),
        ToolStatus(..),

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

        module DaVinciEvent,

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

import SIM
import qualified ExtendedPrelude
import qualified Posix
import qualified IOExts(unsafePerformIO)

import ChildProcess
import GUICore                          
import FiniteMap
import MenuItem


import DaVinciGraphTerm
import DaVinciEvent

import WBFiles

import Debug(debug)

-- ---------------------------------------------------------------------------
-- DaVinci Data Definition & instantiations
-- ---------------------------------------------------------------------------

data DaVinci = 
        DaVinci {
                fDispatcher     :: (Dispatcher DaVinciEventInfo),
                fSession        :: GUIOBJECT,
                fDaVinciState   :: (PVar DST)
                }

data DST = DST {
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
                fGraphState :: (RVar GST),
                fTypes      :: (PVar (FiniteMap TypeId GUIOBJECT))
                }

data GST =
        GST {
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

emptyGST = GST 0 [] [] [] [] [] [] [] []

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------


instance Object DaVinci where
        objectID dav = objectID (fSession dav)

 
instance SingleInstanceTool DaVinci where 
        getToolInstance = getDaVinciHandle


instance Destructible DaVinci where
         destroy dav = shutdownDaVinci dav                                      
         destroyed dav = 
                        destroyed (fDispatcher dav)
                  +>    disconnected dav
          where disconnected :: DaVinci -> IA ()
                disconnected dav =  
                   (listenDaVinci (dav,DisConnect) :: IA DaVinciEventInfo) >>>= 
                        \_ -> do {try (destroy dav); done}
 

instance Tool DaVinci where
         getToolStatus dav = getToolStatus (fDispatcher dav)

instance UnixTool DaVinci where
         getUnixProcessID dav = getUnixProcessID (fDispatcher dav)


instance CommandTool DaVinci where
        evalCmd cmd dav = evalCmd cmd (fDispatcher dav)
        execCmd cmd dav = execCmd cmd (fDispatcher dav)
        execOneWayCmd cmd dav = execOneWayCmd cmd (fDispatcher dav)


instance Object Graph where 
         objectID g = objectID (fGraphObj g)

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
startDaVinci = do {
        gobj <- newAbstractGUIObject;
        cntx <- newPVar 0;
        mq <- newMsgQueue;
        tname <- getWBToolFilePath "daVinci";   
        dist <- newDispatcher tname [arguments ["-pipe"]] fin (handleEvent mq);
        mtx <- newPVar (DST Nothing []);
        dav <- return (DaVinci dist gobj mtx);
        forkIO(dispatcher dav mq cntx True);
        return dav
} where fin :: Dispatcher DaVinciEventInfo -> IO ()
        fin = execOneWayCmd "menu(file(exit))" 


-- ---------------------------------------------------------------------------
--  Tool Termination
-- ---------------------------------------------------------------------------

shutdownDaVinci :: DaVinci -> IO ()
shutdownDaVinci dav = do {                                      
        graphs <- updVar (fDaVinciState dav) (\dst -> do {
                destroy (fDispatcher dav); 
                destroy (fSession dav);
                return (DST Nothing [],fGraphs dst)
                });
        foreach graphs cleanupGraph;
        done
        }


-- ---------------------------------------------------------------------------
--  Sending DaVinci Commands
-- ---------------------------------------------------------------------------

withDaVinci :: IO String -> IO ()
withDaVinci beh = do {
        dav <- getDaVinciHandle;
        withVar (fDaVinciState dav) (\ _ -> do {msg <- beh; evalCmd msg dav});
        done
        }

withDaVinciOneWay :: IO String -> IO ()
withDaVinciOneWay beh = do {
        dav <- getDaVinciHandle;
        withVar (fDaVinciState dav) (\_ -> do {msg <- beh;execOneWayCmd msg dav });
        done
        }



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
createGraph = do {
        dav <- getDaVinciHandle;
        gobj <- createGUIObject GRAPH defMethods;
        mv <- newRVar (emptyGST);
        types <- newPVar emptyFM;
        g <- return (Graph dav gobj mv types);
        newType g (Just "");
        insertGraph dav g;
        openContext (contextID g);
        return g
} where insertGraph :: DaVinci -> Graph -> IO ()
        insertGraph dav g = changeVar' (fDaVinciState dav) (\dst -> 
                dst{fGraphs = g:(fGraphs dst)})



withGraph ::  Graph -> IO String -> IO Graph
withGraph g beh = synchronize g (do {
        changeVar (fDaVinciState dav) (\dst -> do {
                cmd <- beh;
                cntx' <- changeContext dav (fContextID dst) g;
                evalCmd cmd dav;
                return (dst{fContextID = Just cntx'})
                });
        return g
}) where dav = fDaVinci g


withGraphOneWay ::  Graph -> IO String -> IO Graph
withGraphOneWay g beh = synchronize g (do {
        changeVar (fDaVinciState dav) (\dst -> do {
                cmd <- beh;
                cntx' <- changeContext dav (fContextID dst) g;
                execOneWayCmd cmd (fDispatcher dav);
                return (dst{fContextID = Just cntx'})
                });
        return g
}) where dav = fDaVinci g


getGraphs :: DaVinci -> IO [Graph]
getGraphs dav = withVar' (fDaVinciState dav) fGraphs


-- ---------------------------------------------------------------------------
-- Redisplay
-- ---------------------------------------------------------------------------

redrawGraph :: Graph -> IO ()
redrawGraph g @ (Graph dav _ gst _) = synchronize g (
    changeVar gst (\gs -> do {
        changeVar (fDaVinciState dav) ( \dst -> do {
                cntx' <- changeContext dav (fContextID dst) g;
                flushCmds (fNodeUpd gs) (fEdgeUpd gs) (fAttrUpd gs) dav;
                return (dst{fContextID = Just cntx'})
                });
        return (gs{fNodeUpd = [],fEdgeUpd = [],fAttrUpd = []})
        }))


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
openContext cntx = 
        withDaVinci (return ("multi(open_context("++show (show cntx)++"))"))


contextID :: Object w => w -> Int 
contextID w = let (ObjectID n) = objectID w in n


-- ---------------------------------------------------------------------------
-- Close Graph
-- ---------------------------------------------------------------------------

closeGraph :: Graph -> IO ()
closeGraph g = changeVar gst (\gs -> do {
        destroy (fGraphObj g);
        changeVar (fDaVinciState dav) ( \dst -> do {
                cntx' <- changeContext dav (fContextID dst) g;
                try(execOneWayCmd "menu(file(close))" dav);
                return (dst{    fContextID = Nothing, -- Just cntx',
                                fGraphs = filter (/= g) (fGraphs dst)
                                })
                });
        return emptyGST
        })
 where  dav  = fDaVinci g
        gst  = fGraphState g
        guio = fGraphObj
                


cleanupGraph :: Graph -> IO ()
cleanupGraph g = changeVar gst (\gs -> do {
        changeVar' (fDaVinciState (fDaVinci g)) ( \dst -> 
                dst{fGraphs = filter (/= g) (fGraphs dst)});
        try(destroy guio);
        return emptyGST
        }) 
 where  gst = fGraphState g
        guio = fGraphObj g
                


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
createEdge g e@(Edge _ tid eid _) src trg = changeVar' gst (\gs ->
        gs{fLinks = (e,src,trg) : (fLinks gs), fEdgeUpd = eupd : (fEdgeUpd gs)})
  where eupd = EdgeUpd eid tid [] (fNodeId src) (fNodeId trg)
        gst  = fGraphState g



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
        toEventID ((Graph _ guio _ _),dev) =  EventID (objectID guio) (show dev)

instance EventDesignator (ObjectID,DaVinciFileEvent) where
        toEventID (oid,dev) =  EventID oid (show dev)

instance EventDesignator (Graph,DaVinciEvent) where
        toEventID (g,dev) =  EventID (objectID g) (show dev)

instance EventDesignator (DaVinci,DaVinciEvent) where
        toEventID (dav,dev) =  EventID (objectID dav) (show dev)


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
handleEvent mq str _ = do {     
        sendIO mq ans;
        case ans of
                (DaVinciAnswer DisConnect _) -> deadlock
                (DaVinciAnswer InternalError _) -> deadlock
                _ -> done
} where ans = read str


-- ---------------------------------------------------------------------------
-- EVENT DISPATHER
-- ---------------------------------------------------------------------------

dispatcher :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int ->  Bool -> IO ()
dispatcher dav mq pv ignoreOk = do {
        ans <- sync(receive mq);
        delegateDav dav mq pv ignoreOk ans;     
        dispatcher' dav mq pv ans
        }

dispatcher' :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int -> DaVinciAnswer -> IO ()

dispatcher'  dav mq pv (DaVinciAnswer ComError _)       = 
        dispatcher dav mq pv True
dispatcher'  dav mq pv _                = 
        dispatcher dav mq pv False


delegateDav :: DaVinci -> MsgQueue DaVinciAnswer -> PVar Int ->  
                        Bool -> DaVinciAnswer -> IO ()
delegateDav dav mq pv ignoreOk ans@(DaVinciAnswer NodeSelectionLabels nodesel) = do {
        mans' <- sync(withTimeout (secs 0.25) (receive mq));
        case mans' of
                Nothing -> do {
                        dispatchDav dav pv ignoreOk ans;        
                        dispatcher' dav mq pv ans
                        }
                (Just ans'@(DaVinciAnswer NodeDoubleClick _)) -> do {
                        dispatchDav dav pv ignoreOk (DaVinciAnswer NodeDoubleClick nodesel);    
                        dispatcher' dav mq pv ans'
                        }
                (Just ans') -> do {
                        dispatchDav dav pv ignoreOk ans;
                        dispatchDav dav pv False ans';  
                        dispatcher' dav mq pv ans'
                        }
        }
delegateDav dav mq pv ignoreOk ans@(DaVinciAnswer EdgeSelectionLabel edgesel) = do {
        mans' <- sync(withTimeout (secs 0.25) (receive mq));
        case mans' of
                Nothing -> do {
                        dispatchDav dav pv ignoreOk ans;        
                        dispatcher' dav mq pv ans
                        }
                (Just ans'@(DaVinciAnswer EdgeDoubleClick _)) -> do {
                        dispatchDav dav pv ignoreOk (DaVinciAnswer EdgeDoubleClick  edgesel);   
                        dispatcher' dav mq pv ans'
                        }
                (Just ans') -> do {
                        dispatchDav dav pv ignoreOk ans;
                        dispatchDav dav pv False ans';  
                        dispatcher' dav mq pv ans'
                        }
        }
delegateDav dav mq pv ignoreOk ans = do {
        dispatchDav dav pv ignoreOk ans;        
        dispatcher' dav mq pv ans
        }
        

dispatchDav :: DaVinci -> PVar Int -> Bool -> DaVinciAnswer -> IO ()

dispatchDav dav pv alreadyReplied (DaVinciAnswer Ok (Reply str)) = do {
        unless alreadyReplied (sendReply (Right str) disp)
        } where disp = fDispatcher dav 

dispatchDav dav pv _ (DaVinciAnswer ComError _) = do {
        sendReply (Left daVinciFailure) disp
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer DisConnect _) = do {
        debug "DDAV1";
        dispatch disp (dav, DisConnect) NoDaVinciEventInfo done;
        debug "DDAV1E";
        }  where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer Close _) = do {
        cntx <- getVar pv;
        setVar pv 0;
        dispatch disp (ObjectID cntx, Close) NoDaVinciEventInfo done
        } where disp = fDispatcher dav

dispatchDav dav pv  _ (DaVinciAnswer ContextChanged (Context cntx)) =
        setVar pv cntx

dispatchDav dav pv _ (DaVinciAnswer NodeSelectionLabels nodesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,NodeSelectionLabels) nodesel done;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer EdgeSelectionLabel edgesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,EdgeSelectionLabel) edgesel done
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer NodeDoubleClick nodesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,NodeDoubleClick) nodesel done;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer EdgeDoubleClick edgesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,EdgeSelectionLabel) edgesel done
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer IconSelection (MenuInvocation iid)) = do {
        butt <- lookupGUIObject (ObjectID iid);
        invoke butt;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer MenuSelection (MenuInvocation iid)) = do {
        butt <- lookupGUIObject (ObjectID iid);
        invoke butt;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer FileMenuSelection (FileMenuInvocation m)) = do { 
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx, m) NoDaVinciEventInfo done
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer InternalError (Reply str)) = do {
        debug "DDAV2";
        debug str;
        dispatch disp (dav,DisConnect) NoDaVinciEventInfo done;
        debug "DDAV2E";
        } where disp = fDispatcher dav
                
dispatchDav dav pv _ (DaVinciAnswer PopupSelectionNode inf@(PopupSelectionNodeInf _ iid)) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,PopupSelectionNode) inf done;
        butt <- lookupGUIObject (ObjectID iid);
        invoke butt;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer PopupSelectionEdge inf@(PopupSelectionEdgeInf _ iid)) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,PopupSelectionEdge) inf done;
        butt <- lookupGUIObject (ObjectID iid);
        invoke butt;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer CreateNode nodesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,CreateNode) nodesel done;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer CreateNodeAndEdge nodesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,CreateNodeAndEdge) nodesel done;
        } where disp = fDispatcher dav

dispatchDav dav pv _ (DaVinciAnswer CreateEdge nodesel) = do {
        cntx <- getVar pv;
        dispatch disp (ObjectID cntx,CreateEdge) nodesel done;
        } where disp = fDispatcher dav


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
     
