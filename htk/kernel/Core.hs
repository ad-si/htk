-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Core (

  Wish(..),
  wish,

  TclCmd,
  TclScript,
  TclResponse(..),
  TclMessageType(..),

  execCmd,
  evalCmd,
  execTclScript,
  evalTclScript,
  execMethod,
  evalMethod,
  setTclVariable,
  getTclVariable,

-- ** submodules **

  module GUIValue,
  module GUIObjectName,
  module GUIObjectKind,


-- ** tool instance **

  GUI(..),
  getGUI,


-- ** Widget configuration **

  ConfigOption,
  ConfigID,

  showConfigs,
  showConfig,


-- ** enabling / disabling of widgets **

  HasEnable(..),


-- ** GUIObjects and methods (internal representation of Tk-Widgets) **

  GUIOBJECT(..), 
  OST(..),              -- the gui objects state
  GUIObject(..),

  Object(..),
  ObjectID(..),
  getObjectNo,
  getParentObjectID,

  createGUIObject,
  createHTkObject,
  createWidget,

  lookupGUIObjectByName,
  lookupGUIObject,
  getParentPathName,
  getParentObject,

  getObjectKind,
  setObjectKind,

  getObjectName,
  setObjectName,

  Methods(..),
  defMethods,
  voidMethods,
  setMethods,


-- ** events **

  WishEvent(..),
  WishEventType(..),
  WishEventModifier(..),
  KeySym(..),
  BNo(..),
  bind,
  bindSimple,

  HasCommand(..),

-- needed to build bind and unbind methods in widget classes:
  bindTagS,
  showP,
  mkBoundCmdArg,
  delimitString,
  BindTag,
  EventInfoSet,

-- ** Tk variables **

  tkDeclVar,
  tkUndeclVar,

) where

import FiniteMap
import qualified ExtendedPrelude(split)
import Maybe
import GUIValue
import ReferenceVariables
import Destructible
import Concurrent
import IOExts(unsafePerformIO)
import Channels
import GuardedEvents
import EqGuard
import Computation
import Events
import EventInfo
import Object
import GUIObjectName
import GUIObject
import GUIObjectKind
import Wish
import BaseClasses(Widget)
import Configuration
import PackOptions
import GridPackOptions


-- -----------------------------------------------------------------------
--  base GUI object
-- -----------------------------------------------------------------------

instance GUIObject GUIOBJECT where
  toGUIObject = id
  cname _ = "GUIOBJECT"


-- -----------------------------------------------------------------------
-- destruction of GUI objects
-- -----------------------------------------------------------------------

instance GUIObject a => Destroyable a where
  destroy wid =
    do
      let (GUIOBJECT oid ostref) = toGUIObject wid
      meth <- withRef ostref methods
      nm <- withRef ostref objectname
      execTclScript ((destroyCmd meth) nm)


-- -----------------------------------------------------------------------
--  GUI / GUI State
-- -----------------------------------------------------------------------

data GUI        = GUI GUIOBJECT (Ref GST)

type GST        = FiniteMap ObjectID GUIOBJECT


-- -----------------------------------------------------------------------
--  GUI Instances
-- -----------------------------------------------------------------------

instance Object GUI where
        objectID (GUI obj _) = objectID obj

instance GUIObject GUI where
  toGUIObject (GUI obj _) = obj
  cname _ = "GUI"


-- -----------------------------------------------------------------------
--  GUI state
-- -----------------------------------------------------------------------

getGUI :: IO GUI          -- IO because of old htk stuff, may change
getGUI =
  return (unsafePerformIO (do
                             wdg <- newRef emptyFM
                             obj <- newGUIObject ROOT SESSION defMethods
                             let gui = GUI obj wdg
                             return gui))

applyGUI :: (GST -> GST) -> IO ()
applyGUI f =  getGUI >>= \ (GUI _ gui) -> changeRef gui f

queryGUI :: (GST -> a) -> IO a
queryGUI f = getGUI >>= \ (GUI _ gui) -> withRef gui f


-- ----------------------------------------------------------------------
--  GUIObject/Widget Creation
-- ----------------------------------------------------------------------

createGUIObject :: GUIOBJECT -> ObjectKind -> Methods -> IO GUIOBJECT
createGUIObject par@(GUIOBJECT _ postref) kind meths =
  do
    guio@(GUIOBJECT oid ostref) <- newGUIObject par kind meths
    GUI _ gstref <- getGUI
    changeRef gstref (newObj guio)
    name <- withRef ostref objectname
    pname <- withRef postref objectname
    execTclScript ((createCmd meths) pname kind name oid [])
    return guio
  where newObj guio @ (GUIOBJECT oid ost) wd = addToFM wd oid guio
createGUIObject ROOT kind meths =
  do
    guio@(GUIOBJECT oid ostref) <- newGUIObject ROOT kind meths
    GUI _ gstref <- getGUI
    changeRef gstref (newObj guio)
    name <- withRef ostref objectname
    execTclScript ((createCmd meths) (ObjectName ".") kind name oid [])
    return guio
  where newObj guio @ (GUIOBJECT oid ost) wd = addToFM wd oid guio


createHTkObject :: Methods -> IO GUIOBJECT
createHTkObject meths =
  do
    oid <- newObject
    ost <- newRef (OST ABSTRACT (ObjectName ".") oid meths)
    return (GUIOBJECT oid ost)

createWidget :: GUIOBJECT -> ObjectKind -> IO GUIOBJECT
createWidget par kind = createGUIObject par kind defMethods


-- -----------------------------------------------------------------------
-- lookup object handle etc.
-- -----------------------------------------------------------------------

lookupGUIObject :: ObjectID -> IO GUIOBJECT
lookupGUIObject key = do {
        mwid <- queryGUI (\wd -> lookupFM wd key);
        case mwid of
                Nothing    ->
                  error "Haskell-Tk Error: gui object not found"
                (Just wid) -> return wid
        }                                               -- TD ???
 
getParentPathName :: GUIObject w => w -> IO (Maybe ObjectName)
getParentPathName w =
  do
    par' <- getParentObject w
    case par' of Nothing -> return Nothing
                 Just par -> do
                               nm <- getObjectName par
                               return (Just nm)

lookupGUIObjectByName :: WidgetName -> IO (Maybe GUIOBJECT)
lookupGUIObjectByName (WidgetName "") = return Nothing
lookupGUIObjectByName (WidgetName str) = 
        queryGUI (\wd -> lookupFM wd no)
        where   wnm = head (reverse (ExtendedPrelude.split (== '.') str)) 
                no = ObjectID (read ( drop 1 wnm))

getParentObject :: GUIObject w => w -> IO (Maybe GUIOBJECT)
getParentObject w =
  do
    oid <- getParentObjectID (toGUIObject w)
    queryGUI (\wd -> lookupFM wd oid)             -- TD ???

getParentObjectID :: GUIOBJECT -> IO ObjectID
getParentObjectID (GUIOBJECT _ ostref) = withRef ostref parentobj


-- -----------------------------------------------------------------------
-- instances (Show)
-- -----------------------------------------------------------------------

showConfig (cid, cval) =
  "-" ++ cid ++ " " ++
  case cid of
    "tag" -> "\"" ++ (drop 2 (show cval))
    _     -> show cval

showConfigs [] = " "
showConfigs (x : ol) = (showConfig x) ++ " " ++ (showConfigs ol)


-- -----------------------------------------------------------------------
--  GUIObject default methods (for widgets and foreign objects mainly)
-- -----------------------------------------------------------------------

defMethods :: Methods
defMethods = Methods tkGetWidgetConfig
                     tkSetWidgetConfigs
                     tkCreateWidget
                     tkPack
		     tkGrid
                     tkDestroyWidget
                     tkBindWidget
                     tkUnbindWidget
                     tkCleanupWidget

voidMethods :: Methods
voidMethods = Methods (\_ _ -> [])
                      (\_ _ -> [])
                      (\_ _ _ _ _ -> [])
                      (\_ _ -> [])
                      (\_ _ -> [])
                      (\_ -> [])
                      (\_ _ _ _ -> [])
                      (\_ _ _ -> [])
                      (\_ _ -> [])


-- -----------------------------------------------------------------------
-- unparsing of widget (default methods) 
-- -----------------------------------------------------------------------

tkCreateWidget :: ObjectName -> ObjectKind -> ObjectName -> ObjectID ->
                  [ConfigOption] -> TclScript
tkCreateWidget _ kind name _ opts =
  [show kind ++ " " ++ show name ++ " " ++ showConfigs opts]

tkPack :: ObjectName -> [PackOption] -> TclScript
tkPack name opts = ["pack " ++ show name ++ " " ++ showPackOptions opts]

tkGrid :: ObjectName -> [GridPackOption] -> TclScript
tkGrid name opts =
  ["grid " ++ show name ++ " " ++ showGridPackOptions opts]

tkBindWidget :: ObjectName -> BindTag -> [WishEvent] -> EventInfoSet ->
                TclScript
tkBindWidget nm bindTag wishEvents eventInfoSet =
  let doBind = "bind " ++ show nm ++ " " ++
               delimitString (foldr (\ event soFar -> showP event soFar)
                                    "" wishEvents) ++ " " ++
               mkBoundCmdArg bindTag eventInfoSet
  in [doBind]

tkUnbindWidget :: ObjectName -> BindTag -> [WishEvent] -> TclScript
tkUnbindWidget nm bindTag wishEvents =
  let {-doRm = "rmtag " ++ show nm ++ " " ++ bindTagS bindTag-}
      doUnBind = "bind " ++ show nm ++ " " ++
                 delimitString (foldr (\ event soFar -> showP event soFar)
                                      "" wishEvents) ++ " {}"
  in [{-doRm,-} doUnBind]

tkDestroyWidget :: ObjectName -> TclScript
tkDestroyWidget name = ["destroy " ++ show name]

tkCleanupWidget :: ObjectID -> ObjectName -> TclScript
tkCleanupWidget _ _ = []

tkGetWidgetConfig :: ObjectName -> ConfigID -> TclScript
tkGetWidgetConfig name cid = [(show name) ++ " cget -" ++ cid]

tkSetWidgetConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetWidgetConfigs _ [] = []
tkSetWidgetConfigs name args =
  [show name ++ " configure " ++ showConfigs args]


-- -----------------------------------------------------------------------
-- widget commands
-- -----------------------------------------------------------------------

class GUIObject w => HasCommand w where
  clicked :: w -> IO (Event ())
  clicked w =
     do
       let (GUIOBJECT oid _) = toGUIObject w
       cset w "command" (TkCommand ("puts \"CO " ++ show oid ++ "\""))
       return (toEvent (listen (coQueue wish) |> Eq (CallBackId oid))
                 >>> return ())


-- ---------------------------------------------------------------------
-- bindings
-- ---------------------------------------------------------------------

---
-- Binds an event for this widget.  The second action returns unbinds
-- the event.
bind :: GUIObject wid => wid -> [WishEvent] -> IO (Event EventInfo,IO ())
bind wid wishEvents {- eventInfoSet -} =
   do
      -- Allocate a bindtag
      let mVar = bindTags wish
      bindTag <- takeMVar mVar
      putMVar mVar (succBindTag bindTag)
      -- do the binding
      let (GUIOBJECT oid ostref) = toGUIObject wid
      meth <- withRef ostref methods
      nm <- getObjectName (toGUIObject wid)
      execTclScript ((bindCmd meth) nm bindTag wishEvents
                                    defaultEventInfoSet)
      let
         event =
            toEvent (listen (eventQueue wish) |> Eq bindTag)
               >>>= (\ (_,eventInfoSet) -> return eventInfoSet)
         unbind :: IO ()
         unbind = execTclScript ((unbindCmd meth) nm bindTag wishEvents)
      return (event,unbind)

bindSimple :: GUIObject wid => wid -> WishEventType ->
                               IO (Event (),IO ())
bindSimple wid wishEventType =
   do
      (event1, deregister) <- bind wid [WishEvent [] wishEventType]
      return (event1 >>> done,deregister)


-- -----------------------------------------------------------------------
-- convenient execution of Tcl commands
-- -----------------------------------------------------------------------

evalMethod :: (GUIObject a, GUIValue b) =>
              a -> (ObjectName -> TclScript) -> IO b
evalMethod wid meth =
  do
    let (GUIOBJECT _ ostref) = toGUIObject wid
    nm <- withRef ostref objectname
    str <- evalTclScript (meth nm)
    creadTk str

execMethod :: GUIObject a => a -> (ObjectName -> TclScript) -> IO ()
execMethod wid meth =
  do
    let (GUIOBJECT _ ostref) = toGUIObject wid
    nm <- withRef ostref objectname
    execTclScript (meth nm)


-- -----------------------------------------------------------------------
-- Tk variables (for internal use)
-- -----------------------------------------------------------------------

tkDeclVar :: String -> String -> TclScript
tkDeclVar var val = ["global " ++ var, "set " ++ var ++ " " ++ val]

tkUndeclVar :: String -> TclScript
tkUndeclVar var = ["global " ++ var, "unset " ++ var]


-- -----------------------------------------------------------------------
-- Tcl variables
-- -----------------------------------------------------------------------

setTclVariable :: GUIValue a => String -> a -> IO ()
setTclVariable name v =
  execTclScript ["global " ++ name,
                 "set " ++ name ++ " " ++ show (toGUIValue v)]

getTclVariable :: GUIValue a => String -> IO a
getTclVariable name =
  evalTclScript ["global " ++ name ++ "; set res $" ++ name] >>= creadTk

