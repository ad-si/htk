{- GraphConfigure contains definitions for the various configuration
   options for GraphDisp objects.  These should be implemented
   using the Computation.HasConfig interface, applied to instances of
   GraphParms, NodeTypeParms and ArcTypeParms.
   -}
module GraphConfigure(
   GraphAllConfig, -- this is a subclass of GraphAll plus ALL configuration
                   -- options in this file.
   HasGraphConfigs, -- all options for configuring graphs
   HasNodeTypeConfigs, -- ditto node types
   HasArcTypeConfigs, -- ditto arc types

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

   -- Colours
   Color(..),

   -- Edge patterns
   EdgePattern(..),

   -- Drag and Drop actions.
   GraphGesture(..),
   NodeGesture(..),
   NodeDragAndDrop(..),

   -- Double click actions
   DoubleClickAction(..),

   -- Graph Miscellaneous Flags
   OptimiseLayout(..),   
   SurveyView(..),
   AllowDragging(..),

   -- ($$$?) is used for Maybe (option), where Nothing means
   -- "No change".
   ($$$?),
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

---
-- $$$? can be a useful abbreviation
($$$?) :: (HasConfigValue option configuration,Typeable value)
    => Maybe (option value) -> configuration value -> configuration value
($$$?) Nothing configuration = configuration
($$$?) (Just option) configuration = ($$$) option configuration

infixr 0 $$$?

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

instance HasMapIO LocalMenu where
   mapIO a2bAct (LocalMenu menuPrim) =
      LocalMenu
         (mapMenuPrim
            (\ b2Act ->
               (\ aValue ->
                  do
                     bValue <- a2bAct aValue
                     b2Act bValue
                  )
               )
            menuPrim
            )


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

instance HasMapIO ValueTitle where
   mapIO a2bAct (ValueTitle b2StringAct) =
      ValueTitle (
         \ aValue -> 
            do
               bValue <- a2bAct aValue
               b2StringAct bValue
            )

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

instance HasMapIO NodeGesture where
   mapIO a2bAct (NodeGesture b2StringAct) =
      NodeGesture (
         \ aValue -> 
            do
               bValue <- a2bAct aValue
               b2StringAct bValue
            )

data NodeDragAndDrop value = NodeDragAndDrop (Dyn -> value -> IO ())
-- A NodeDragAndDrop corresponds to dragging some other node
-- (which could be of any type - hence its value must be encoded by
-- a Dyn) onto this node (value value).   

instance NodeTypeConfig NodeDragAndDrop

------------------------------------------------------------------------
-- Double click actions
------------------------------------------------------------------------

newtype DoubleClickAction value = DoubleClickAction (value -> IO ())

------------------------------------------------------------------------
-- Shape, colours, and edge patterns
------------------------------------------------------------------------

-- This datatype is based on DaVinciClasses.hs, including several
-- name clashes.  However we omit Textual, add the file argument
-- to iconic and the shape Triangle.  This datatype may get bigger!
data Shape value = Box | Circle | Ellipse | Rhombus | Triangle | 
   Icon FilePath deriving (Read,Show)

instance NodeTypeConfig Shape

newtype Color value = Color String deriving (Read,Show)
-- The user is responsible for making sure this String is properly
-- formatted.  To quote from the daVinci documentation:
-- > Can be used to define the background color of a node. The value of this 
-- > attribute may be any X-Window colorname (see file lib/rgb.txt in your X11
-- > directory) or any RGB color specification in a format like "#0f331e", 
-- > where 0f is the hexadecimal value for the red part of the color, 33 is 
-- > the green part and 1e is the blue.  Hence, a pallet of 16.7 million 
-- > colors is supported. The default color for nodes is "white".  
-- There is a function for constructing "RGB color specification"s in
-- htk/resources/Colour.hs
instance NodeTypeConfig Color

instance ArcTypeConfig Color

data EdgePattern value = Solid | Dotted | Dashed | Thick | Double 
   deriving (Read,Show)

instance ArcTypeConfig EdgePattern

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

------------------------------------------------------------------------
-- Grouping options
-- GraphAllConfig
------------------------------------------------------------------------

class (
   GraphParms graphParms,
   HasConfig GlobalMenu graphParms,HasConfig GraphTitle graphParms,
   HasConfig GraphGesture graphParms,HasConfig OptimiseLayout graphParms,
   HasConfig SurveyView graphParms,HasConfig AllowDragging graphParms)
   => HasGraphConfigs graphParms

instance (
   GraphParms graphParms,
   HasConfig GlobalMenu graphParms,HasConfig GraphTitle graphParms,
   HasConfig GraphGesture graphParms,HasConfig OptimiseLayout graphParms,
   HasConfig SurveyView graphParms,HasConfig AllowDragging graphParms)
   => HasGraphConfigs graphParms

class (
   NodeTypeParms nodeTypeParms,
   HasConfigValue LocalMenu nodeTypeParms,
   HasConfigValue ValueTitle nodeTypeParms,
   HasConfigValue NodeGesture nodeTypeParms,
   HasConfigValue NodeDragAndDrop nodeTypeParms,
   HasConfigValue DoubleClickAction nodeTypeParms,
   HasConfigValue Shape nodeTypeParms,
   HasConfigValue Color nodeTypeParms)
   => HasNodeTypeConfigs nodeTypeParms

instance (
   NodeTypeParms nodeTypeParms,
   HasConfigValue LocalMenu nodeTypeParms,
   HasConfigValue ValueTitle nodeTypeParms,
   HasConfigValue NodeGesture nodeTypeParms,
   HasConfigValue NodeDragAndDrop nodeTypeParms,
   HasConfigValue DoubleClickAction nodeTypeParms,
   HasConfigValue Shape nodeTypeParms,
   HasConfigValue Color nodeTypeParms)
   => HasNodeTypeConfigs nodeTypeParms


class (
   ArcTypeParms arcTypeParms,
   HasConfigValue DoubleClickAction arcTypeParms,
   HasConfigValue LocalMenu arcTypeParms,
   HasConfigValue ValueTitle arcTypeParms,
   HasConfigValue Color arcTypeParms,
   HasConfigValue EdgePattern arcTypeParms)
   => HasArcTypeConfigs arcTypeParms

instance (
   ArcTypeParms arcTypeParms,
   HasConfigValue DoubleClickAction arcTypeParms,
   HasConfigValue LocalMenu arcTypeParms,
   HasConfigValue ValueTitle arcTypeParms,
   HasConfigValue Color arcTypeParms,
   HasConfigValue EdgePattern arcTypeParms)
   => HasArcTypeConfigs arcTypeParms

class 
   (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
   HasGraphConfigs graphParms,
   HasNodeTypeConfigs nodeTypeParms,
   HasArcTypeConfigs arcTypeParms
   ) 
   => GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms  

instance 
   (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
   HasGraphConfigs graphParms,
   HasNodeTypeConfigs nodeTypeParms,
   HasArcTypeConfigs arcTypeParms
   ) 
   => GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms  

