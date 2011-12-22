module HTk.Kernel.GUIObject(

  GUIObject(..),
  GUIOBJECT(..),
  newGUIObject,
  setObjectKind,
  setObjectName,
  getMethods,
  setMethods,
  getObjectName,
  getObjectNo,
  getObjectKind,
  OST(..),
  ConfigID,
  ConfigOption,
  Methods(..)

) where

import Reactor.ReferenceVariables
import Events.Synchronized
import HTk.Kernel.GUIObjectKind
import HTk.Kernel.GUIObjectName
import Util.Object
import HTk.Kernel.Wish
import HTk.Kernel.EventInfo
import HTk.Kernel.GUIValue
import HTk.Kernel.PackOptions
import HTk.Kernel.GridPackOptions


-- -----------------------------------------------------------------------
-- class GUIObject
-- -----------------------------------------------------------------------

class GUIObject w where
  toGUIObject     :: w -> GUIOBJECT
  cname           :: w -> String
  cset            :: GUIValue a => w -> ConfigID -> a -> IO w
  cget            :: GUIValue a => w -> ConfigID -> IO a
  cset w cid v    = setConfig (toGUIObject w) cid v >> return w
  cget w cid      = getConfig (toGUIObject w) cid

setConfig :: GUIValue a => GUIOBJECT -> ConfigID -> a -> IO ()
setConfig (GUIOBJECT _ ostref) cid val =
  do
    ost <- getRef ostref
    execTclScript
      ((csetCmd (methods ost)) (objectname ost) [(cid, toGUIValue val)])

getConfig :: GUIValue a => GUIOBJECT -> ConfigID -> IO a
getConfig (GUIOBJECT _ ostref) cid =
  do
    ost <- getRef ostref
    resp <- evalTclScript ((cgetCmd (methods ost)) (objectname ost) cid)
    creadTk resp


-- -----------------------------------------------------------------------
-- internal GUI object
-- -----------------------------------------------------------------------

data GUIOBJECT = GUIOBJECT ObjectID (Ref OST) | ROOT

data OST =                          -- GUI Object State
  OST { objectkind :: ObjectKind,
        objectname :: ObjectName,
        parentobj  :: ObjectID,
        methods    :: Methods }


-- -----------------------------------------------------------------------
--  GUIOBJECT instances
-- -----------------------------------------------------------------------

instance Eq GUIOBJECT where
  (GUIOBJECT key1 _) == (GUIOBJECT key2 _) = key1 == key2
  wid1 /= wid2 = not (wid1 == wid2)

instance Ord GUIOBJECT where
  (GUIOBJECT key1 _) <= (GUIOBJECT key2 _) = key1 <= key2

instance Object GUIOBJECT where
  objectID (GUIOBJECT oid _) = oid

instance Synchronized GUIOBJECT where
  synchronize (GUIOBJECT _ ostref) = synchronize ostref


-- -----------------------------------------------------------------------
--  object creation
-- -----------------------------------------------------------------------

-- do not call directly / use createGUIObject instead
newGUIObject :: GUIOBJECT -> ObjectKind -> Methods -> IO GUIOBJECT
newGUIObject par@(GUIOBJECT parId parostref) kind meths =
  do
    oid <- newObject
    parnm <- withRef parostref objectname
    case kind of
      TEXTTAG _ -> do
                     ost <- newRef (OST kind (TextPaneItemName parnm
                                                (TextTagID oid))
                                        parId meths)
                     return (GUIOBJECT oid ost)
      EMBEDDEDTEXTWIN _ _ -> do
                               ost <- newRef (OST kind (TextPaneItemName parnm
                                                          (TextTagID oid))
                                                  parId meths)
                               return (GUIOBJECT oid ost)
      MENUITEM _ i -> do
                        ost <- newRef (OST kind (MenuItemName parnm i)
                                           parId meths)
                        return (GUIOBJECT oid ost)
      CANVASITEM _ _ -> do
                          ost <- newRef
                                   (OST kind (CanvasItemName
                                                parnm
                                                (CanvasTagOrID oid))
                                        parId meths)
                          return (GUIOBJECT oid ost)
      NOTEBOOKPAGE _ -> do
                          ost <- newRef (OST kind (NoteBookPageName oid)
                                             parId meths)
                          return (GUIOBJECT oid ost)
      WINDOWPANE -> do
                      ost <- newRef (OST kind (PaneName oid)
                                         parId meths)
                      return (GUIOBJECT oid ost)
      LABELFRAME -> do
                      let nm = show parnm ++
                               (if show parnm == "." then "" else ".") ++
                               show oid
                      ost <- newRef (OST kind (LabelFrameName
                                                 (ObjectName nm) oid)
                                         parId meths)
                      return (GUIOBJECT oid ost)
      SUBWIDGET subKind megaName ->
         do let objName = "["++show kind++"]"
            ost <- newRef (OST subKind (ObjectName objName) parId meths)
            return (GUIOBJECT oid ost)
      _ -> do
             let nm = show parnm ++
                      (if show parnm == "." then "" else ".") ++ show oid
             ost <- newRef (OST kind (ObjectName nm) parId meths)
             return (GUIOBJECT oid ost)
newGUIObject ROOT kind meths =
  do
    oid <- newObject
    ost <- newRef (OST kind (ObjectName ("." ++ show oid)) oid meths)
    return (GUIOBJECT oid ost)


-- -----------------------------------------------------------------------
--  GUI object identity
-- -----------------------------------------------------------------------

getObjectNo :: GUIOBJECT -> Int
getObjectNo (GUIOBJECT (ObjectID i) _) = i
{-# INLINE getObjectNo #-}


-- -----------------------------------------------------------------------
--  GUIObject methods
-- -----------------------------------------------------------------------

getMethods :: GUIOBJECT -> IO Methods
getMethods (GUIOBJECT _ ostref) = withRef ostref methods

setMethods :: GUIOBJECT -> Methods -> IO ()
setMethods (GUIOBJECT _ ostref) meth =
  changeRef ostref (\o -> o{methods = meth})


-- -----------------------------------------------------------------------
--  Object Kind
-- -----------------------------------------------------------------------

getObjectKind :: GUIOBJECT -> IO ObjectKind
getObjectKind (GUIOBJECT _ ostref) = withRef ostref objectkind

setObjectKind :: GUIOBJECT -> ObjectKind -> IO ()
setObjectKind (GUIOBJECT _ ostref) kind =
  changeRef ostref (\o -> o{objectkind = kind})


-- -----------------------------------------------------------------------
--  Object Name Related Functions
-- -----------------------------------------------------------------------

getObjectName :: GUIOBJECT -> IO ObjectName
getObjectName (GUIOBJECT _ ostref) = withRef ostref objectname

setObjectName :: GUIOBJECT -> ObjectName -> IO ()
setObjectName (GUIOBJECT _ ostref) name =
  changeRef ostref (\o -> o{objectname = name})


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

type ConfigID   = String
type ConfigOption = (ConfigID, GUIVALUE)


-- -----------------------------------------------------------------------
--  Methods
-- -----------------------------------------------------------------------

data Methods =
  Methods { cgetCmd     :: ObjectName -> ConfigID -> TclScript,
            csetCmd     :: ObjectName -> [ConfigOption] -> TclScript,
            createCmd   :: ObjectName -> ObjectKind -> ObjectName ->
                           ObjectID -> [ConfigOption] -> TclScript,
            packCmd     :: ObjectName -> [PackOption] -> TclScript,
            gridCmd     :: ObjectName -> [GridPackOption] -> TclScript,
            destroyCmd  :: ObjectName -> TclScript,
            bindCmd     :: ObjectName -> BindTag -> [WishEvent] ->
                           EventInfoSet -> Bool -> TclScript,
            unbindCmd   :: ObjectName -> BindTag -> [WishEvent] ->
                           Bool -> TclScript,
            cleanupCmd  :: ObjectID -> ObjectName -> TclScript }


