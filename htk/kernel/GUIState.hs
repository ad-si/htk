{- #########################################################################

MODULE        : GUIState
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : The local cache of GUI Objects. The commands may have
                effect upon the cache as well as the state of Tk. The
                state of the latter is changed by sending commands to
                wish.

                The computations deal with:
                        
                        -- creation of graphical objects
                        -- looking up inf in the widget tree
                        -- destruction of graphical objects
                        -- destruction events
                        -- setting and retrieving config options
                        -- modification events
                        -- user interactions
                        -- interface to Tk
                        
                Conceptually not that complex. The code is however
                slightly complicated and deals with a lot of 
                technicalitites of the underlying representation.               


   ######################################################################### -}


module GUIState (
        ChildProcess,
        FiniteMap,

        GUI(..),
        Object(..),
        Destructible(..),
        Tool(..),
        UnixTool(..),
        SingleInstanceTool(..),

        EventDesignator(..),
        GUIObject(..),
        DispatchMode(..),
        Interactive(..),

        tkcget,
        pset,
        pget,

        module GUIObject,
        module GUIObjectName,
        module GUIObjectKind,
        module GUIMethods,

        lookupGUIObjectByName,
        lookupGUIObject,

        getParentPathName,
        checkIfUnpacked,
        checkIfPacked,

        createGUIObject,
        createWidget,
        getParentObject,


        module GUIWish,
        evalMethod,
        execMethod,     
        execTclScript,
        evalTclScript,
        execScript,
        evalScript,
        callWish,
        execWish,
        setTclVariable,
        getTclVariable
        
        ) where

import FiniteMap
import qualified IOExts(unsafePerformIO)

import SIM
import qualified ExtendedPrelude

import Resources
import GUIWish
import GUIMethods
import GUIObject
import GUIObjectName
import GUIObjectKind
import Debug(debug)


-- --------------------------------------------------------------------------
-- Class GUIObject 
-- --------------------------------------------------------------------------

class GUIObject w where
        toGUIObject     :: w -> GUIOBJECT
        cname           :: w -> String
        cset            :: GUIValue a => w -> ConfigID -> a -> IO w
        cget            :: GUIValue a => w -> ConfigID -> IO a
        cset w cid v    = setConfig (toGUIObject w) cid v >> return w
        cget w cid      = getConfig (toGUIObject w) cid


-- --------------------------------------------------------------------------
-- Interactive Objects 
-- --------------------------------------------------------------------------

class GUIObject w => Interactive w where
        userinteraction :: GUIEventDesignator e => w -> e -> DispatchMode -> IA EventInfo
        emitGUIEvent    :: GUIEventDesignator e => w -> e -> EventInfo -> IO ()
        userinteraction  = listenGUI
        emitGUIEvent w e info =
                execWish (\(Wish d) -> dispatch d (oid,ev) info done)
                 where  (GUIOBJECT oid mon) = toGUIObject w
                        ev = toEventPatternID e



-- --------------------------------------------------------------------------
--  Base Event
-- --------------------------------------------------------------------------

instance EventDesignator (GUIOBJECT,[EventPattern]) where
        toEventID (guid, e) = EventID (objectID guid) (toEventPatternID e)


-- --------------------------------------------------------------------------
--  Base GUI Object
-- --------------------------------------------------------------------------

instance GUIObject GUIOBJECT where
        toGUIObject = id
        cname _ = "GUIOBJECT"
        cset w cid val = do {setConfig w cid val; return w}
        cget w = getConfig w


instance Destructible GUIOBJECT where
        destroy w = do {try(deleteGUIObject (toGUIObject w)); done}
        destroyed w = 
                userinteraction w (Any,Destroy) Notice >>> done
           +>   destroyed readGUIState


instance Interactive GUIOBJECT


-- --------------------------------------------------------------------------
--  GUI State
-- --------------------------------------------------------------------------

data GUI        = GUI Wish GUIOBJECT (PVar GST)

type GST        = FiniteMap ObjectID GUIOBJECT


-- --------------------------------------------------------------------------
--  GUI Instances
-- --------------------------------------------------------------------------

instance Object GUI where
        objectID (GUI wish obj _) = objectID obj


instance Destructible GUI where
        destroy gui =  do {
                emitGUIEvent (toGUIObject gui) Destroy NoEventInfo;
                try(execWish destroy); 
                try(applyGUI (\_ -> emptyFM));
                done
                }
        destroyed gui = userinteraction (toGUIObject gui) Destroy Notice >>> done

        
instance Tool GUI where
        getToolStatus (GUI wish obj _) = getToolStatus wish


instance UnixTool GUI where
        getUnixProcessID (GUI wish obj _) = getUnixProcessID wish


instance SingleInstanceTool GUI where
        getToolInstance = return readGUIState


instance GUIObject GUI where
        toGUIObject (GUI wish obj _) = obj
        cname _ = "GUI"


instance Synchronized GUI where 
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
--  GUI State
-- --------------------------------------------------------------------------

initGUI :: IO GUI
initGUI = do {
        wdg <- newPVar emptyFM;
        wish <- newWish;
        obj <- newGUIObject SESSION defMethods;
        gui <- return (GUI wish obj wdg);
        registerTool gui;
        return gui
        }

readGUIState :: GUI
readGUIState = IOExts.unsafePerformIO initGUI

applyGUI :: (GST -> GST) -> IO ()
applyGUI f =  getToolInstance >>= \ (GUI _ _ gui) -> changeVar' gui f

queryGUI :: (GST -> a) -> IO a
queryGUI f = getToolInstance >>= \ (GUI _ _ gui) -> withVar' gui f


-- --------------------------------------------------------------------------
--  GUIObject/Widget Creation
-- --------------------------------------------------------------------------

createGUIObject :: ObjectKind -> Methods -> IO GUIOBJECT
createGUIObject kind meths = do {
        guio <- newGUIObject kind meths;
        (GUI _ _ gui) <- getToolInstance;
        changeVar' gui (newObj guio);
        return guio
} where newObj guio @ (GUIOBJECT oid mon) wd = addToFM wd oid guio


createWidget :: ObjectKind -> IO GUIOBJECT
createWidget kind = createGUIObject kind defMethods


-- --------------------------------------------------------------------------
--  OBJECT Deletion
-- --------------------------------------------------------------------------

deleteGUIObject :: GUIOBJECT -> IO ()
deleteGUIObject (wid @ (GUIOBJECT key mon)) = do {
        emitGUIEvent wid (Any,Destroy) NoEventInfo;
        applyGUI (\wd -> delFromFM wd key);
        changeVar mon (\o ->
           case objectname o of
                Nothing -> do {
                        foreach (childobjs o) deleteGUIObject;
                        return (delObject o)
                        }
                (Just name) -> do {
                        dcmd <- return ((destroyCmd (methods o)) key name);
                        ccmd <- return ((cleanupCmd (methods o)) key name);
                        cccmd <- sequence (map cleanupGUIObject (childobjs o));
                        execTclScript (dcmd ++ ccmd ++ concat cccmd);
                        return (delObject o)
                        }
        )}


delObject :: OST -> OST
delObject (OST t n o p c b f m) = OST t Nothing o p c b f m


cleanupGUIObject :: GUIOBJECT -> IO TclScript
cleanupGUIObject (wid @ (GUIOBJECT key mon)) = do {
        updVar mon (\o ->
           case objectname o of
                Nothing -> return (delObject o,[])
                (Just name) -> do {
                        ccmd <- return ((cleanupCmd (methods o)) key name);
                        cccmd <- sequence (map cleanupGUIObject (childobjs o));
                        return (delObject o, ccmd ++ concat cccmd)
                        }
        )}
        
        
-- --------------------------------------------------------------------------
--  Cached Configuration Options 
-- --------------------------------------------------------------------------

pset :: (GUIObject w, GUIValue a) => w -> ConfigID -> a -> IO w
pset w cid v = setPackConfig (toGUIObject w) cid v >> return w
{-# INLINE pset #-}

pget :: (GUIObject w, GUIValue a) => w -> ConfigID -> IO a
pget w cid = getPackConfig (toGUIObject w) cid
{-# INLINE pget #-}


setConfig :: GUIValue a => GUIOBJECT -> ConfigID -> a -> IO ()
setConfig (wid @ (GUIOBJECT _ mon)) cid v  =
        changeVar mon (\o -> do {
           (case objectname o of
                Nothing -> done
                (Just name) -> 
                        execTclScript ((csetCmd (methods o)) name [(cid, v')]));
           return (o{configopts = addToFM (configopts o) cid v'})
           }
        ) where v' = (toGUIValue v)


getConfig :: GUIValue a => GUIOBJECT -> ConfigID -> IO a
getConfig (wid @ (GUIOBJECT _ mon)) cid = withVar mon (\o ->
        case objectname o of
                Nothing -> lookupConfig (configopts o) cid
                (Just name) -> 
                   case lookupFM (configopts o) cid of
                        Nothing -> readTkConfig name cid (methods o)            
                        (Just v) -> fromGUIValueIO v
        )


tkcget :: (GUIObject w, GUIValue a) => w -> ConfigID -> IO a
tkcget w cid = withVar mon (\o ->
        case objectname o of
                Nothing -> lookupConfig (configopts o) cid
                (Just name) -> readTkConfig name cid (methods o)
        ) where wid @ (GUIOBJECT _ mon) = toGUIObject w


readTkConfig :: GUIValue a => ObjectName -> ConfigID -> Methods -> IO a
readTkConfig name cid meths =  do {
        str <- execWish(evalScript ((cgetCmd meths) name cid));
        case str of
                "" ->  return cdefault
                _ ->   creadTk str
        }

-- --------------------------------------------------------------------------
--  Lookup Object Handle etc.
-- --------------------------------------------------------------------------

lookupGUIObject :: ObjectID -> IO GUIOBJECT
lookupGUIObject key = do {
        mwid <- queryGUI (\wd -> lookupFM wd key);
        case mwid of
                Nothing    -> error "Haskell-Tk Error: gui object not found"
                (Just wid) -> return wid
        }

 
getParentPathName :: GUIObject w => w -> IO (Maybe ObjectName)
getParentPathName w = do {
        prnt <- getParentObject w;
        case prnt of
                Nothing -> return Nothing       -- object is not yet packed
                (Just pid) -> getObjectName pid
        }


lookupGUIObjectByName :: WidgetName -> IO (Maybe GUIOBJECT)
lookupGUIObjectByName (WidgetName "") = return Nothing
lookupGUIObjectByName (WidgetName str) = 
        queryGUI (\wd -> lookupFM wd no)
        where   wnm = head (reverse (ExtendedPrelude.split (== '.') str)) 
                no = ObjectID (read ( drop 1 wnm))


getParentObject :: GUIObject w => w -> IO (Maybe GUIOBJECT)
getParentObject w =do {
        oid <- getParentObjectID (toGUIObject w);
        case oid of
                Nothing -> return Nothing
                Just k ->  queryGUI (\wd -> lookupFM wd k)
        } 

-- --------------------------------------------------------------------------
-- Interactions 
-- --------------------------------------------------------------------------

listenGUI :: (Interactive w,GUIEventDesignator e) 
          => w -> e -> DispatchMode -> IA EventInfo
listenGUI w e md = interaction gev bnd unbnd
 where  wid     = toGUIObject w
        eid     = toEventPatternID e
        gev     = EventID (objectID wid) eid 
        fields  = (toEventFields e)
        bnd     = registerGUIEvent wid (eid,EventFieldDsgs fields) md 
        unbnd   = deregisterGUIEvent wid (eid,EventFieldDsgs fields)


registerGUIEvent :: GUIOBJECT -> Binding -> DispatchMode -> Listener -> IO ()
registerGUIEvent (GUIOBJECT oid mon) bnd @ (ev,_) md lst =  
    changeVar mon (\o -> do {
        try(execWish (\(Wish d) -> register d (oid,ev) md (bind o oid bnd) lst));
        return (addBinding bnd o);
        })
 where  addBinding bnd @ (_,f) o  = o{bindings = bnd : (bindings o)}
        mkBindCmd o nm oid bnd = (bindCmd (methods o)) nm oid bnd
        bind o oid bnd = 
                incase (objectname o) (\nm -> 
                        execTclScript (mkBindCmd o nm oid bnd))


deregisterGUIEvent :: GUIOBJECT -> Binding -> Listener -> IO ()
deregisterGUIEvent (GUIOBJECT oid mon) bnd @ (ev,_) lst = 
   changeVar mon (\o -> do {
        try(execWish (\(Wish d) -> deregister d (oid,ev) (unbind o oid bnd) lst));
        return (remBinding bnd o);
        })
 where  remBinding :: Binding -> OST -> OST
        remBinding (ev,f) o = o{bindings = b'}
                        where b' = filter (\(pt,_) -> pt /= ev) (bindings o)
        mkUnbindCmd o nm oid bnd = (unbindCmd (methods o)) nm oid bnd
        unbind o oid bnd = 
                incase (objectname o) (\nm -> 
                        execTclScript (mkUnbindCmd o nm oid bnd))


-- --------------------------------------------------------------------------
-- Command Script Execution and Evaluation
-- --------------------------------------------------------------------------

callWish :: GUIObject w => w -> (ObjectName -> Wish -> IO a) -> IO a
callWish w cmd = withVar mon (\ o ->
        case (objectname o) of
                Nothing -> errorObjectNotPacked wid
                (Just p) -> execWish (cmd p)
        ) where wid @ (GUIOBJECT _ mon) = toGUIObject w


execWish :: (Wish -> IO a) -> IO a
execWish cmd = getToolInstance >>= \ (GUI wish _ _) -> cmd wish

execTclScript :: TclScript -> IO ()
execTclScript scr = (execWish . execScript) scr

evalTclScript :: GUIValue a => TclScript -> IO a
evalTclScript scr = execWish (evalScript scr) >>= creadTk

setTclVariable :: GUIValue a => String -> a -> IO ()
setTclVariable name v = execWish (setTclVar name v) 

getTclVariable :: GUIValue a => String -> IO a
getTclVariable name = execWish (getTclVar name)

evalMethod :: (GUIObject w,GUIValue a) => w -> (ObjectName -> TclScript) -> IO a
evalMethod w f = callWish (toGUIObject w) (\nm -> evalScript (f nm)) >>= creadTk

execMethod :: (GUIObject w) => w -> (ObjectName -> TclScript) -> IO ()
execMethod w f = callWish (toGUIObject w) (\nm -> execScript (f nm)) >> done
