{- GraphConfigure contains definitions for the various configuration
   options for GraphDisp objects.  These should be implemented
   using the Computation.HasConfig interface, applied to instances of
   GraphParms, NodeTypeParms and ArcTypeParms.
   -}
module GraphConfigure(
   HasConfig(($$),configUsed), -- from Computation
   HasConfigValue(($$$),configUsed'), 
                   -- HasConfig lifted to options/configurations of kind
                   -- 1 which take a Typeable value.

   -- LocalMenu describes menus or buttons for objects that carry a value,
   -- IE nodes or arcs.
   LocalMenu(..),

   -- GlobalMenu describes menus or buttons for objects that don't carry a
   -- value, IE graphs.
   GlobalMenu(..),

   -- MenuPrim is supposed to be the generalised Menu/Button type.
   MenuPrim(..), -- a type with TWO parameters.  We provide maps
                       -- and monadic methods for both.
   mapMenuPrim,
   mapMenuPrim',
   mapMMenuPrim,
   mapMMenuPrim',
   
   -- Titles for graphs and objects
   GraphTitle(..),
   ValueTitle(..),

   -- Shapes for nodes
   Shape(..),

   -- Drag and Drop actions.
   GraphGesture(..),
   NodeGesture(..),
   NodeDragAndDrop(..),

   -- Graph Miscellaneous Flags
   OptimiseLayout(..),   
   SurveyView(..),
   AllowDragging(..),

   ) where

import Computation(HasConfig(($$),configUsed))
import Dynamics(Dyn,Typeable)

import GraphDisp

------------------------------------------------------------------------
-- HasConfigValue is a useful extension of HasConfig for types that
-- take a Typeable value; EG node and arc configurations.
------------------------------------------------------------------------

class HasConfigValue option configuration where
   ($$$) :: Typeable value 
      => option value -> configuration value -> configuration value

   configUsed' :: Typeable value 
      => option value -> configuration value -> Bool

infixr 0 $$$

instance (Typeable value,HasConfigValue option configuration) 
   => HasConfig (option value) (configuration value) where
   ($$) = ($$$)
   configUsed = configUsed'

------------------------------------------------------------------------
-- Menus and buttons
-- As in DaVinci, a menu is simply considered as a tree of buttons,
-- allowing an elegant recursive definition.
-- We define MenuPrim as it may be useful for
-- implementations, so they don't have to define their own datatypes
-- for menus.
------------------------------------------------------------------------

instance GraphConfig GlobalMenu

newtype GlobalMenu = GlobalMenu(MenuPrim (Maybe String) (IO ()))

instance NodeTypeConfig LocalMenu

instance ArcTypeConfig LocalMenu

newtype LocalMenu value = 
   LocalMenu(MenuPrim (Maybe String) (value -> IO()))

data MenuPrim subMenuValue value =
      Button String value
      -- first argument is text to put on button.
      -- second argument generates an action to be performed when the
      -- button is pressed.
      -- The dynamic value is that supplied to the node/arc when it
      -- was created.
   |  Menu subMenuValue [MenuPrim subMenuValue value]
      -- List of buttons with a possible title.
   |  Blank
      -- A Blank can be used to separate groups of menu buttons in the
      -- same menu.     

mapMenuPrim :: (a -> b) -> MenuPrim c a -> MenuPrim c b
mapMenuPrim a2b (Button label a) = Button label (a2b a)
mapMenuPrim a2b (Menu subMenuValue menuButtons) =
   Menu subMenuValue (map (mapMenuPrim a2b) menuButtons)
mapMenuPrim a2b Blank = Blank

mapMenuPrim' :: (c -> d) -> MenuPrim c a -> MenuPrim d a
mapMenuPrim' c2d (Button title action) = Button title action
mapMenuPrim' c2d (Menu subMenuValue menuButtons) =
   Menu (c2d subMenuValue) (map (mapMenuPrim' c2d) menuButtons)
mapMenuPrim' c2d Blank = Blank

mapMMenuPrim :: (Monad m) => (a -> m b) -> MenuPrim c a 
   -> m (MenuPrim c b)
mapMMenuPrim a2bAct (Button label a) =
   do
      b <- a2bAct a
      return (Button label b)
mapMMenuPrim a2bAct (Menu subMenuValue menuButtons) =
   do
      bMenuButtons <- mapM (mapMMenuPrim a2bAct) menuButtons
      return (Menu subMenuValue bMenuButtons)
mapMMenuPrim a2bAct Blank = return Blank

mapMMenuPrim' :: (Monad m) => (c -> m d) -> MenuPrim c a 
   -> m (MenuPrim d a)
mapMMenuPrim' c2dAct (Button title action) = 
   return (Button title action)
mapMMenuPrim' c2dAct (Menu subMenuValue menuButtons) =
   do
      dMenuButtons <- mapM (mapMMenuPrim' c2dAct) menuButtons
      dSubMenuValue <- c2dAct subMenuValue
      return (Menu dSubMenuValue dMenuButtons) 
mapMMenuPrim' c2dAct Blank = return Blank

------------------------------------------------------------------------
-- Titles
------------------------------------------------------------------------

data GraphTitle = GraphTitle String
instance GraphConfig GraphTitle

data ValueTitle value = ValueTitle (value -> IO String)
-- ValueTitles are computed from the node or arc value using the supplied
-- computation when the node or arc is created or when 
-- setNodeValue/setArcValue are called.

instance NodeTypeConfig ValueTitle

instance ArcTypeConfig ValueTitle

------------------------------------------------------------------------
-- Drag and Drop
-- These are inspired by DaVinci's Drag and Drop functions.
-- Each configuration gives a corresponding action to perform.
-- We give DaVinci's suggested applications.
-- NB - where these are used the AllowDragging True
-- operator should also have been set for the graph.
------------------------------------------------------------------------

data GraphGesture = GraphGesture (IO ())
-- GraphGesture for a graphical interface is a mouse action
-- not involving any nodes but somewhere on the graph.
-- (suggested use: create a new node)

instance GraphConfig GraphGesture

data NodeGesture value = NodeGesture (value -> IO ())
-- A NodeGesture for a graphical interface is a mouse action
-- involving one node.  
-- (suggested use: create a new node, and link it to this one)

instance NodeTypeConfig NodeGesture

data NodeDragAndDrop value = NodeDragAndDrop (Dyn -> value -> IO ())
-- A NodeDragAndDrop corresponds to dragging some other node
-- (which could be of any type - hence its value must be encoded by
-- a Dyn) onto this node (value value).   

instance NodeTypeConfig NodeDragAndDrop

------------------------------------------------------------------------
-- Shapes etcetera
------------------------------------------------------------------------

-- This datatype is based on DaVinciClasses.hs, including several
-- name clashes.  However we omit Textual, add the file argument
-- to iconic and the shape Triangle.  This datatype may get bigger!
data Shape nodeLabel = Box | Circle | Ellipse | Rhombus | Triangle | 
   Icon FilePath deriving (Read,Show)

instance NodeTypeConfig Shape

------------------------------------------------------------------------
-- Graph Miscellaneous Flags.
-- (Fairly daVinci specific)
-- Where these are unset, they should always default to False.
------------------------------------------------------------------------

newtype OptimiseLayout = OptimiseLayout Bool
-- If True, try hard to optimise the layout of the graph
-- on redrawing it.

instance GraphConfig OptimiseLayout

newtype SurveyView = SurveyView Bool
-- If True, add a survey view of the graph; IE display
-- a picture of the whole graph which fits onto the
-- screen (without displaying everything)
-- as well as a picture of the details (which may not
-- fit onto the screen).

instance GraphConfig SurveyView

newtype AllowDragging = AllowDragging Bool
-- If True, allow Drag-and-Drop operators.  

instance GraphConfig AllowDragging
