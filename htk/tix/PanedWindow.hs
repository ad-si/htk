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

---
-- HTk's <strong>PanedWindow</strong>.<br>
-- A paned window is a container widget, that is devided into scaleable
-- horizontal or vertical panes.
module PanedWindow (

  PanedWindow,
  newPanedWindow,

  Pane,
  createPane,

  after,
  before,
  at,
  expand,
  minsize,
  maxsize,
  initsize

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Computation
import Synchronized
import Destructible
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type PanedWindow
-- -----------------------------------------------------------------------

---
-- The <code>PanedWindow</code> datatype.
newtype PanedWindow = PanedWindow GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- paned window creation
-- -----------------------------------------------------------------------

---
-- Constructs a new paned window and returns it as a value.
-- @param cnf     - the list of configuration options for this
--                - paned window.
-- @return result - A paned window.
newPanedWindow :: Container par => par -> Orientation ->
                                   [Config PanedWindow] -> IO PanedWindow
newPanedWindow par or cnf =
  do
    w <- createGUIObject (toGUIObject par) (PANEDWINDOW or)
                         panedWindowMethods
    configure (PanedWindow w) cnf


-- -----------------------------------------------------------------------
-- paned window methods
-- -----------------------------------------------------------------------

panedWindowMethods :: Methods
panedWindowMethods = Methods (cgetCmd defMethods)
                             (csetCmd defMethods)
                             tkCreatePanedWindow
                             (packCmd defMethods)
                             (gridCmd defMethods)
                             (destroyCmd defMethods)
                             (bindCmd defMethods)
                             (unbindCmd defMethods)
                             (cleanupCmd defMethods)

tkCreatePanedWindow :: ObjectName -> ObjectKind -> ObjectName ->
                       ObjectID -> [ConfigOption] -> TclScript
tkCreatePanedWindow _ (PANEDWINDOW or) name _ opts =
  ["tixPanedWindow " ++ show name ++ " -orientation " ++ show or ++ " " ++
   showConfigs opts]
tkCreatePanedWindow _ _ _ _ _ = []
{-# INLINE tkCreatePanedWindow #-}


-- -----------------------------------------------------------------------
-- widget specific configuration options
-- -----------------------------------------------------------------------



-- -----------------------------------------------------------------------
-- paned window instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject PanedWindow where 
---
-- Internal.
  toGUIObject (PanedWindow f) = f
---
-- Internal.
  cname _ = "PanedWindow"

---
-- You can specify the size of a paned window.
instance HasSize PanedWindow

---
-- A paned window can be destroyed.
instance Destroyable PanedWindow where
---
-- Destroys a paned window.
  destroy = destroy . toGUIObject

---
-- A paned window has standard widget properties (focus, cursor, ...).
instance Widget PanedWindow

---
-- You can synchronize on a paned window object (in JAVA style).
instance Synchronized PanedWindow where
---
-- Synchronizes on a paned window object.
  synchronize = synchronize . toGUIObject

{-
---
-- The panes orientation can be vertical or horizontal.
instance HasOrientation PanedWindow where
  orient o win = cset win "orientation" o
  getOrient win = cget win "orientation"
-}


-- -----------------------------------------------------------------------
-- type Pane
-- -----------------------------------------------------------------------

---
-- The <code>Pane</code> datatype - a pane inside a paned window.
newtype Pane = Pane GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- pane creation
-- -----------------------------------------------------------------------

---
-- Constructs a new pane inside a paned window and returns it as a
-- value.
-- @param par     - the parent widget, which has to be a paned window.
-- @param cnf     - the list of configuration options for this pane.
-- @return result - A window pane.
createPane :: PanedWindow -> [CreationConfig Pane]  -> [Config Pane] ->
              IO Pane
createPane nb ccnf cnf =
  do
--    cconf' <- mapM id ccnf
    ccnfstr <- showCreationConfigs ccnf
    w <- createGUIObject (toGUIObject nb) WINDOWPANE
                         (windowPaneMethods ccnfstr)
    configure (Pane w) cnf


-- -----------------------------------------------------------------------
-- pane creation options
-- -----------------------------------------------------------------------

after :: Pane -> CreationConfig Pane
after pane =
  do nm <- getObjectName (toGUIObject pane)
     return ("after " ++ show nm)

before :: Pane -> CreationConfig Pane
before pane =
  do nm <- getObjectName (toGUIObject pane)
     return ("before " ++ show nm)

at :: Int -> CreationConfig Pane
at n = return ("at " ++ show n)

expand :: Double -> CreationConfig Pane
expand d = return ("expand " ++ show d)

minsize :: Int -> CreationConfig Pane
minsize i = return ("min " ++ show i)

maxsize :: Int -> CreationConfig Pane
maxsize i = return ("max " ++ show i)

initsize :: Int -> CreationConfig Pane
initsize i = return ("size " ++ show i)


-- -----------------------------------------------------------------------
-- window pane methods
-- -----------------------------------------------------------------------

windowPaneMethods ccnf = Methods tkGetPaneConfig
                                 tkSetPaneConfigs
                                 (tkCreatePane ccnf)
                                 (packCmd voidMethods)
                                 (gridCmd voidMethods)
                                 (destroyCmd defMethods)
                                 (bindCmd defMethods)
                                 (unbindCmd defMethods)
                                 (cleanupCmd defMethods)


-- -----------------------------------------------------------------------
-- unparsing of notebook page commands
-- -----------------------------------------------------------------------

tkGetPaneConfig :: ObjectName -> ConfigID -> TclScript
tkGetPaneConfig (PaneName oid) cid =
  ["global v" ++ show oid,
   "$v" ++ show oid ++ " cget -" ++ cid]
{-# INLINE tkGetPaneConfig #-}

tkSetPaneConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetPaneConfigs (PaneName oid) args =
  ["global v" ++ show oid,
   "$v" ++ show oid ++ " configure " ++ showConfigs args]
tkSetNoteBookPageConfigs _ _ = []
{-# INLINE tkSetPaneConfigs #-}

tkCreatePane :: String -> ObjectName -> ObjectKind -> ObjectName ->
                ObjectID -> [ConfigOption] -> TclScript
tkCreatePane ccnfstr parnm WINDOWPANE _ oid _ =
  [show parnm ++ " add " ++ show oid ++ " " ++ ccnfstr,
   "global v" ++ show oid,
   "set v" ++ show oid ++ " [" ++ show parnm ++ " subwidget " ++
   show oid ++ "]"]
tkCreatePane _ _ _ _ _ _ = []
{-# INLINE tkCreatePane #-}


-- -----------------------------------------------------------------------
-- window pane instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject Pane where 
---
-- Internal.
  toGUIObject (Pane f) = f
---
-- Internal.
  cname _ = "Pane"

---
-- A pane can be destroyed.
instance Destroyable Pane where
---
-- Destroys a pane.
  destroy   = destroy . toGUIObject

---
-- A pane has standard widget properties (focus, cursor...).
instance Widget Pane

---
-- A pane has a background colour.
instance HasColour Pane where 
---
-- Internal.
  legalColourID = hasBackGroundColour

---
-- A pane is a container for widgets. You can pack widgets to a pane via
-- the pack or grid command in the <code>module Packer</code>.
instance Container Pane

---
-- You can synchronize on a pane object (in JAVA style).
instance Synchronized Pane where
---
-- Synchronizes on a pane object.
  synchronize = synchronize . toGUIObject
