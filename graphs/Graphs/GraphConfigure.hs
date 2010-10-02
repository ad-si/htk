{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- |
-- Description: Extended Interface for Graph Display
--
-- GraphConfigure contains definitions for the various configuration
-- options for "GraphDisp" objects.  These should be implemented
-- using the 'HasConfig', 'HasConfigValue' and 'ModifyHasDef',
-- applied to instances of
-- 'GraphParms', 'NodeTypeParms' and 'ArcTypeParms'.
module Graphs.GraphConfigure(
   GraphAllConfig, -- this is a subclass of GraphAll plus ALL configuration
                   -- options in this file.
   HasGraphConfigs, -- all options for configuring graphs
   HasNodeTypeConfigs, -- ditto node types
   HasNodeModifies, -- all options for modifying nodes.
   HasArcTypeConfigs, -- ditto arc types

   HasConfig(($$),configUsed), -- from Computation
   HasConfigValue(($$$),configUsed'),
                   -- HasConfig lifted to options/configurations of kind
                   -- 1 which take a Typeable value.
   HasModifyValue(..),
      -- used for changing properties of existing objects.

   -- LocalMenu describes menus or buttons for objects that carry a value,
   -- IE nodes or arcs.
   LocalMenu(..),

   -- GlobalMenu describes menus or buttons for objects that don't carry a
   -- value, IE graphs.
   GlobalMenu(..),

   -- function for combining global menus.
   combineGlobalMenus, -- :: [GlobalMenu] -> GlobalMenu

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
   ValueTitleSource(..),

   -- Shapes for nodes
   Shape(..),

   -- Colours
   Color(..),

   -- Edge patterns
   EdgePattern(..),

   -- Edge Direction (_DIR)
   EdgeDir(..),

   -- Edge Head (HEAD)
   Head(..),

   NodeArcsHidden(..), -- Setting if a node's arcs are hidden or not.
   Border(..), -- Specifying a node's border.
   BorderSource(..), -- allowing it to depend on a source.
   FontStyle(..), -- Specifying the font style for a node.
   FontStyleSource(..), -- allowing it to depend on a source.
   ModifyHasDef(..),
      -- specifies default values for these options.

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
   AllowClose(..),
   defaultAllowClose,
   FileMenuAct(..),FileMenuOption(..),

   Orientation(..),
   ActionWrapper(..),

   -- ($$$?) is used for Maybe (option), where Nothing means
   -- "No change".
   ($$$?),
   ) where

import Util.Computation(HasConfig(($$),configUsed),done)
import Util.ExtendedPrelude
import Util.Dynamics(Dyn,Typeable)
import Util.Messages
import Util.Sources
import Util.Delayer

import HTk.Toolkit.MenuType

import Graphs.GraphDisp

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

-- | $$$? can be a useful abbreviation
($$$?) :: (HasConfigValue option configuration,Typeable value)
    => Maybe (option value) -> configuration value -> configuration value
($$$?) Nothing configuration = configuration
($$$?) (Just option) configuration = ($$$) option configuration

infixr 0 $$$?


------------------------------------------------------------------------
-- HasModifyValue is used for dynamic changes to nodes and arcs.
------------------------------------------------------------------------

class HasModifyValue option graph object where
   modify :: Typeable value => option -> graph -> object value -> IO ()

instance HasModifyValue option graph object
   => HasModifyValue (Maybe option) graph object
   where
      modify Nothing _ _ = done
      modify (Just option) graph node = modify option graph node

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

instance HasCoMapIO LocalMenu where
   coMapIO a2bAct (LocalMenu menuPrim) =
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

-- | As a service to MMiSS we provide a function which combines
-- several GlobalMenus into one.
combineGlobalMenus :: [GlobalMenu] -> GlobalMenu
combineGlobalMenus globalMenus =
   GlobalMenu
      (Menu Nothing (map (\ (GlobalMenu menu) -> menu) globalMenus))


------------------------------------------------------------------------
-- Titles
------------------------------------------------------------------------

data GraphTitle = GraphTitle String
instance GraphConfig GraphTitle

instance GraphConfig (SimpleSource GraphTitle)

-- | Provide a function which computes a node or arc title string to be
-- displayed.
data ValueTitle value = ValueTitle (value -> IO String)

-- | Provide a function which computes a source which generates a dynamically-
-- changing title.
data ValueTitleSource value
    = ValueTitleSource (value -> IO (SimpleSource String))

instance NodeTypeConfig ValueTitle

instance NodeTypeConfig ValueTitleSource

instance ArcTypeConfig ValueTitle

instance HasCoMapIO ValueTitle where
   coMapIO a2bAct (ValueTitle b2StringAct) =
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

-- | Action to be performed after mouse action not involving any node but
-- somewhere on the graph.
--
-- If you want to use this, the graph parameters need to include
-- 'AllowDragging' 'True'
data GraphGesture = GraphGesture (IO ())

instance GraphConfig GraphGesture

-- | Action to be performed when the user drags a node somewhere else,
-- but not onto another node.
--
-- If you want to use this, the graph parameters need to include
-- 'AllowDragging' 'True'
data NodeGesture value = NodeGesture (value -> IO ())

instance NodeTypeConfig NodeGesture

instance HasCoMapIO NodeGesture where
   coMapIO a2bAct (NodeGesture b2StringAct) =
      NodeGesture (
         \ aValue ->
            do
               bValue <- a2bAct aValue
               b2StringAct bValue
            )

-- | Action to be performed when the user drags one node onto another.
-- The dragged node's value is passed as a Dyn (since it could have any
-- type).
--
-- If you want to use this, the graph parameters need to include
-- 'AllowDragging' 'True'
data NodeDragAndDrop value = NodeDragAndDrop (Dyn -> value -> IO ())

instance NodeTypeConfig NodeDragAndDrop

------------------------------------------------------------------------
-- Double click actions
------------------------------------------------------------------------

-- | Action to be performed when a node or arc is double-clicked.
newtype DoubleClickAction value = DoubleClickAction (value -> IO ())

instance NodeTypeConfig DoubleClickAction

instance ArcTypeConfig DoubleClickAction

------------------------------------------------------------------------
-- Shape, colours, and edge patterns
------------------------------------------------------------------------

-- | This datatype is based on "DaVinciClasses", including several
-- name clashes.  However we omit 'Textual', add the file argument
-- to 'Icon' and the shape 'Triangle'.  This datatype may get bigger!
data Shape value = Box | Circle | Ellipse | Rhombus | Triangle |
   Icon FilePath deriving (Read,Show)

instance NodeTypeConfig Shape

-- | The user is responsible for making sure this String is properly
-- formatted.  To quote from the daVinci documentation:
--
-- > Can be used to define the background color of a node. The value of this
-- > attribute may be any X-Window colorname (see file lib/rgb.txt in your X11
-- > directory) or any RGB color specification in a format like "#0f331e",
-- > where 0f is the hexadecimal value for the red part of the color, 33 is
-- > the green part and 1e is the blue.  Hence, a pallet of 16.7 million
-- > colors is supported. The default color for nodes is "white".
--
-- There is a function for constructing \"RGB color specification\"s in
-- "Colour".
newtype Color value = Color String deriving (Read,Show)
instance NodeTypeConfig Color

instance ArcTypeConfig Color

-- | The pattern of an edge
data EdgePattern value = Solid | Dotted | Dashed | Thick | Double
   deriving (Read,Show)

instance ArcTypeConfig EdgePattern

-- | The user is responsible for making sure this String is properly
-- formatted.  To quote from the daVinci documentation:
--
-- > This attribute is used to control the arrow of an edge. In a graph visualization,
-- > each edge usually has an arrow pointing to the child node. This attribute can be
-- > used to let the arrow be drawn inverse (i.e. pointing to the parent), to get an arrow
-- > at both sides of an edge or to suppress arrows for a particular edge. The supported
-- > attribute values are: "last" (1 arrow pointing to the child, default), \"first\"
-- >(1 arrow to the parent), "both" (2 arrows to the parent and to children) and "none"
-- >(no arrows).
--
data EdgeDir value = Dir String deriving (Read, Show)

instance ArcTypeConfig EdgeDir


-- | The user is responsible for making sure this String is properly
-- formatted.  To quote from the daVinci documentation:
--
-- >  With this attribute you can control the shape of the edge's arrows.
-- > The possible values are: "farrow" (default), "arrow", "fcircle", and "circle",
-- > where a leading 'f' means filled.
--
data Head value = Head String deriving (Read, Show)

instance ArcTypeConfig Head

------------------------------------------------------------------------
-- Node miscellaneous flags
------------------------------------------------------------------------

class ModifyHasDef modification where
   def :: modification
   isDef :: modification -> Bool

-- | If True, arcs from the node are not displayed.
newtype NodeArcsHidden = NodeArcsHidden Bool

instance ModifyHasDef NodeArcsHidden where
   def = NodeArcsHidden False
   isDef (NodeArcsHidden b) = not b

-- | The border of this node
data Border = NoBorder | SingleBorder | DoubleBorder

-- | Compute a 'Border' which dynamically changes.
data BorderSource value = BorderSource (value -> IO (SimpleSource Border))

instance NodeTypeConfig BorderSource

{- Modification is no longer approved of for Borders, which should be
   set by Sources. -}
{-
instance ModifyHasDef Border where
   def = SingleBorder
   isDef SingleBorder = True
   isDef _ = False
-}

-- | The font in which the label of this node is displayed.
data FontStyle = NormalFontStyle | BoldFontStyle | ItalicFontStyle
   | BoldItalicFontStyle deriving (Eq)

{- Modification is no longer approved for FontStyle's, which should
   be set by Sources.
instance ModifyHasDef FontStyle where
   def = BoldFontStyle
   isDef BoldFontStyle = True
   isDef _ = False
-}

-- | Compute a 'FontStyle' which dynamically changes.
data FontStyleSource value
    = FontStyleSource (value -> IO (SimpleSource FontStyle))

instance NodeTypeConfig FontStyleSource

------------------------------------------------------------------------
-- Graph Miscellaneous Flags.
-- (Fairly daVinci specific)
-- Where these are unset, they should always default to False.
------------------------------------------------------------------------

-- | If 'True', try hard to optimise the layout of the graph
-- on redrawing it.
newtype OptimiseLayout = OptimiseLayout Bool


instance GraphConfig OptimiseLayout

-- | If True, add a survey view of the graph; IE display
-- a picture of the whole graph which fits onto the
-- screen (without displaying everything)
-- as well as a picture of the details (which may not
-- fit onto the screen).
--
-- (The user can do this anyway from daVinci's menus.)
newtype SurveyView = SurveyView Bool

instance GraphConfig SurveyView

-- | If True, allow Drag-and-Drop operators.
newtype AllowDragging = AllowDragging Bool

-- | If set, action which is invoked if the user attempts to close the
-- window.  If the action returns True, we close it.
--
-- WARNING.  This action is performed in the middle of the event loop,
-- so please don't attempt to do any further graph interactions during it.
-- (But HTk interactions should be fine.)
newtype AllowClose = AllowClose (IO Bool)

defaultAllowClose :: AllowClose
defaultAllowClose = AllowClose (confirmMess "Really close window?")


-- | The following options are provided specially by DaVinci (see, for now,
-- <http://www.informatik.uni-bremen.de/daVinci/old/docs/reference/api/api_app_menu_cmd.html>
-- for the daVinci2.1 documentation.  If a 'FileMenuAct' is used as
-- a configuration with a specified action, the corresponding option is
-- enabled in the daVinci File menu, and the action is performed when the
-- option is selected.
--
-- The 'AllowClose' configuration and 'CloseMenuOption' both set the action
-- to be taken when the user selects a close event, and each overrides the
-- other.
--
-- By default the Close and Print options are enabled, however these
-- and other options can be disabled by specifing 'Nothing' as the
-- second argument to FileMenuAct.
data FileMenuOption =
      NewMenuOption | OpenMenuOption | SaveMenuOption | SaveAsMenuOption
   |  PrintMenuOption | CloseMenuOption | ExitMenuOption deriving (Ord,Eq)

data FileMenuAct = FileMenuAct FileMenuOption (Maybe (IO ()))

instance GraphConfig FileMenuAct

instance GraphConfig AllowDragging

-- | Allows the user to specify a 'Delayer'.  This will postpone redrawing
-- on the graph.
instance GraphConfig Delayer

instance GraphConfig AllowClose

-- | Which way up the graph is.
--
-- We copy the DaVinciTypes constructors, though of course this will
-- mean we have to painfully convert one to the other.
data Orientation = TopDown | BottomUp | LeftRight | RightLeft

instance GraphConfig Orientation

-- | Function to be applied to all user actions.  This is useful
-- for exception wrappers and so on.
newtype ActionWrapper = ActionWrapper (IO () -> IO ())


instance GraphConfig ActionWrapper

------------------------------------------------------------------------
-- Grouping options
-- GraphAllConfig
------------------------------------------------------------------------

class (
   GraphParms graphParms,
   HasConfig GlobalMenu graphParms,HasConfig GraphTitle graphParms,
   HasConfig GraphGesture graphParms,HasConfig OptimiseLayout graphParms,
   HasConfig SurveyView graphParms,HasConfig AllowDragging graphParms,
   HasConfig AllowClose graphParms,HasConfig Orientation graphParms,
   HasConfig FileMenuAct graphParms,HasConfig ActionWrapper graphParms,
   HasConfig (SimpleSource GraphTitle) graphParms,
   HasConfig Delayer graphParms
   )
   => HasGraphConfigs graphParms

instance (
   GraphParms graphParms,
   HasConfig GlobalMenu graphParms,HasConfig GraphTitle graphParms,
   HasConfig GraphGesture graphParms,HasConfig OptimiseLayout graphParms,
   HasConfig SurveyView graphParms,HasConfig AllowDragging graphParms,
   HasConfig AllowClose graphParms,HasConfig Orientation graphParms,
   HasConfig FileMenuAct graphParms,HasConfig ActionWrapper graphParms,
   HasConfig (SimpleSource GraphTitle) graphParms,
   HasConfig Delayer graphParms
   )
   => HasGraphConfigs graphParms

class (
   NodeTypeParms nodeTypeParms,
   HasConfigValue LocalMenu nodeTypeParms,
   HasConfigValue ValueTitle nodeTypeParms,
   HasConfigValue ValueTitleSource nodeTypeParms,
   HasConfigValue FontStyleSource nodeTypeParms,
   HasConfigValue BorderSource nodeTypeParms,
   HasConfigValue NodeGesture nodeTypeParms,
   HasConfigValue NodeDragAndDrop nodeTypeParms,
   HasConfigValue DoubleClickAction nodeTypeParms,
   HasConfigValue Shape nodeTypeParms,
   HasConfigValue Color nodeTypeParms
   )
   => HasNodeTypeConfigs nodeTypeParms


instance (
   NodeTypeParms nodeTypeParms,
   HasConfigValue LocalMenu nodeTypeParms,
   HasConfigValue ValueTitle nodeTypeParms,
   HasConfigValue ValueTitleSource nodeTypeParms,
   HasConfigValue FontStyleSource nodeTypeParms,
   HasConfigValue BorderSource nodeTypeParms,
   HasConfigValue NodeGesture nodeTypeParms,
   HasConfigValue NodeDragAndDrop nodeTypeParms,
   HasConfigValue DoubleClickAction nodeTypeParms,
   HasConfigValue Shape nodeTypeParms,
   HasConfigValue Color nodeTypeParms
   )
   => HasNodeTypeConfigs nodeTypeParms

class (
   HasModifyValue NodeArcsHidden graph node
--  HasModifyValue Border graph node
-- HasModifyValue FontStyle graph node
   ) => HasNodeModifies graph node

instance (
   HasModifyValue NodeArcsHidden graph node
--   HasModifyValue Border graph node
--  HasModifyValue FontStyle graph node
   ) => HasNodeModifies graph node

class (
   ArcTypeParms arcTypeParms,
   HasConfigValue DoubleClickAction arcTypeParms,
   HasConfigValue LocalMenu arcTypeParms,
   HasConfigValue ValueTitle arcTypeParms,
   HasConfigValue Color arcTypeParms,
   HasConfigValue EdgePattern arcTypeParms,
   HasConfigValue EdgeDir arcTypeParms,
   HasConfigValue Head arcTypeParms
  )
   => HasArcTypeConfigs arcTypeParms

instance (
   ArcTypeParms arcTypeParms,
   HasConfigValue DoubleClickAction arcTypeParms,
   HasConfigValue LocalMenu arcTypeParms,
   HasConfigValue ValueTitle arcTypeParms,
   HasConfigValue Color arcTypeParms,
   HasConfigValue EdgePattern arcTypeParms,
   HasConfigValue EdgeDir arcTypeParms,
   HasConfigValue Head arcTypeParms)
   => HasArcTypeConfigs arcTypeParms

class
   (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
   HasGraphConfigs graphParms,
   HasNodeTypeConfigs nodeTypeParms,
   HasNodeModifies graph node,
   HasArcTypeConfigs arcTypeParms
   )
   => GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms

instance
   (GraphAll graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms,
   HasGraphConfigs graphParms,
   HasNodeTypeConfigs nodeTypeParms,
   HasNodeModifies graph node,
   HasArcTypeConfigs arcTypeParms
   )
   => GraphAllConfig graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms
