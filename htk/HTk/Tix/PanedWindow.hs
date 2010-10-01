-- | HTk\'s /PanedWindow/.
-- A paned window is a container widget, that is divided into scaleable
-- horizontal or vertical panes.
module HTk.Tix.PanedWindow (

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

import HTk.Kernel.Core
import HTk.Kernel.BaseClasses(Widget)
import HTk.Kernel.Configuration
import HTk.Kernel.Resources
import Util.Computation
import Events.Synchronized
import Events.Destructible
import HTk.Kernel.Packer

-- -----------------------------------------------------------------------
-- type PanedWindow
-- -----------------------------------------------------------------------

-- | The @PanedWindow@ datatype.
newtype PanedWindow = PanedWindow GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- paned window creation
-- -----------------------------------------------------------------------

-- | Constructs a new paned window and returns it as a value.
newPanedWindow :: Container par => par
   -- ^ the list of configuration options for this
   -- paned window.
   -> Orientation
   ->
   [Config PanedWindow]
   -> IO PanedWindow
   -- ^ A paned window.
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
-- paned window instances
-- -----------------------------------------------------------------------

-- | Internal.
instance GUIObject PanedWindow where
  toGUIObject (PanedWindow f) = f
  cname _ = "PanedWindow"

-- | You can specify the size of a paned window.
instance HasSize PanedWindow

-- | A paned window can be destroyed.
instance Destroyable PanedWindow where
  -- Destroys a paned window.
  destroy = destroy . toGUIObject

-- | A paned window has standard widget properties (focus, cursor, ...).
instance Widget PanedWindow

-- | You can synchronize on a paned window object (in JAVA style).
instance Synchronized PanedWindow where
  -- Synchronizes on a paned window object.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- type Pane
-- -----------------------------------------------------------------------

-- | The @Pane@ datatype - a pane inside a paned window.
newtype Pane = Pane GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- pane creation
-- -----------------------------------------------------------------------

-- | Constructs a new pane inside a paned window and returns it as a
-- value.
createPane :: PanedWindow
   -- ^ the parent widget, which has to be a paned window.
   -> [CreationConfig Pane]
   -- ^ the list of configuration options for this pane.
   -> [Config Pane]
   ->
   IO Pane
   -- ^ A window pane.
createPane nb ccnf cnf =
  do
    ccnfstr <- showCreationConfigs ccnf
    w <- createGUIObject (toGUIObject nb) WINDOWPANE
                         (windowPaneMethods ccnfstr)
    configure (Pane w) cnf


-- -----------------------------------------------------------------------
-- pane creation options
-- -----------------------------------------------------------------------

-- | Specifies that the new pane should be placed after pane in the list of
-- panes in this PanedWindow widget
-- (this is an initial configuration that cannot be changed later).
after :: Pane -> CreationConfig Pane
after pane =
  do nm <- getObjectName (toGUIObject pane)
     return ("after " ++ show nm)

-- | Specifies that the new pane should be placed before pane in the list of
-- panes in this PanedWindow widget
-- (this is an initial configuration that cannot be changed later).
before :: Pane -> CreationConfig Pane
before pane =
  do nm <- getObjectName (toGUIObject pane)
     return ("before " ++ show nm)

-- | Specifies the position of the new pane in the list of panes in this
-- PanedWindow widget. 0 means the first position, 1 means the second,
-- and so on.
at :: Int -> CreationConfig Pane
at n = return ("at " ++ show n)

-- | Specifies the expand\/shrink factor of this pane as a non-negative
-- floating point number. The default value is 0.0. The expand\/shrink
-- factor is used to calculate how much each pane should grow or shrink
-- when the size of the PanedWindow main window is changed. When the main
-- window expands\/shrinks by n pixels, then pane i will grow\/shrink by
-- about n \* factor(i) \/ summation(factors), where factor(i) is the
-- expand\/shrink factor of pane i and summation(factors) is the summation
-- of the expand\/shrink factors of all the panes. If summation(factors)
-- is 0.0, however, only the last visible pane will be grown or shrunk.
expand :: Double -> CreationConfig Pane
expand d = return ("expand " ++ show d)

-- | Specifies the minimum size, in pixels, of the new pane; the default
-- is 0.
minsize :: Int -> CreationConfig Pane
minsize i = return ("min " ++ show i)

-- | Specifies the maximum size, in pixels, of the new pane; the default is
-- 10000.
maxsize :: Int -> CreationConfig Pane
maxsize i = return ("max " ++ show i)

-- | Specifies the size, in pixels, of the new pane; if the -size option is
-- not given, the PanedWindow widget will use the natural size of the pane
-- subwidget.
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
-- unparsing of pane commands
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

-- | Internal.
instance GUIObject Pane where
  toGUIObject (Pane f) = f
  cname _ = "Pane"

-- | A pane can be destroyed.
instance Destroyable Pane where
  -- Destroys a pane.
  destroy   = destroy . toGUIObject

-- | A pane has standard widget properties (focus, cursor...).
instance Widget Pane

-- | A pane has a background colour.
instance HasColour Pane where
  legalColourID = hasBackGroundColour

-- | A pane is a container for widgets. You can pack widgets to a pane via
-- the pack or grid command in the @module Packer@.
instance Container Pane

-- | You can synchronize on a pane object (in JAVA style).
instance Synchronized Pane where
  -- Synchronizes on a pane object.
  synchronize = synchronize . toGUIObject
