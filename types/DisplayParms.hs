{- In this module we develop types for storing node and arc
   attributes for various DisplayTypes. -}
module DisplayParms(
   NodeTypes,
   ArcTypes,

   -- These simply return "Nothing" for any display type
   emptyNodeTypes, 
   emptyArcTypes,

   DisplayTypeFilter(..),

   addNodeRule,
   addArcRule,

   getNodeTypeParms,
   getArcTypeParms,

   SimpleNodeAttributes(..),
   SimpleArcAttributes(..),
   ) where

import Computation
import Dynamics

import GraphConfigure
import GraphDisp

import DisplayTypes
import CodedValue

-- -----------------------------------------------------------------------
-- The Types and Datatypes
-- -----------------------------------------------------------------------

---
-- Selects particular display types
data DisplayTypeFilter =
      AllDisplays  --' select every display
   |  ThisDisplay WrappedDisplayType  --' select just this display


---
-- NodeTypes is a list of display type filters with SimpleNodeAttributes
newtype NodeTypes value = 
   NodeTypes [(DisplayTypeFilter,SimpleNodeAttributes value)]

---
-- ArcTypes is a list of display type filters with SimpleArcAttributes
newtype ArcTypes value = 
   ArcTypes [(DisplayTypeFilter,SimpleArcAttributes value)]

-- -----------------------------------------------------------------------
-- The Datatypes
-- -----------------------------------------------------------------------

---
-- Returns "Nothing" for any display type
emptyNodeTypes :: NodeTypes value
emptyNodeTypes = NodeTypes []
   
---
-- Returns "Nothing" for any display type
emptyArcTypes :: ArcTypes value
emptyArcTypes = ArcTypes []

-- -----------------------------------------------------------------------
-- Instances of Typeable and HasCodedValue.
-- -----------------------------------------------------------------------

shape_tyCon = mkTyCon "DisplayParms" "Shape"
instance HasTyCon1 Shape where
   tyCon1 _ = shape_tyCon
instance Typeable value => HasCodedValue (Shape value) where
   encodeIO = mapEncodeIO show
   decodeIO = mapDecodeIO read

color_tyCon = mkTyCon "DisplayParms" "Color"
instance HasTyCon1 Color where
   tyCon1 _ = color_tyCon
instance Typeable value => HasCodedValue (Color value) where
   encodeIO = mapEncodeIO (\ (Color str) -> str)
   decodeIO = mapDecodeIO (\ str -> Color str)

edgePattern_tyCon = mkTyCon "DisplayParms" "EdgePattern"
instance HasTyCon1 EdgePattern where
   tyCon1 _ = edgePattern_tyCon
instance Typeable value => HasCodedValue (EdgePattern value) where
   encodeIO = mapEncodeIO show
   decodeIO = mapDecodeIO read

simpleNodeAttributes_tyCon = mkTyCon "DisplayParms" "SimpleNodeAttributes"
instance HasTyCon1 SimpleNodeAttributes where
   tyCon1 _ = simpleNodeAttributes_tyCon
instance Typeable value => HasCodedValue (SimpleNodeAttributes value) where
   encodeIO = mapEncodeIO (\ 
      (SimpleNodeAttributes {shape = shape,nodeColor = nodeColor}) ->
      (shape,nodeColor)
      )
   decodeIO = mapDecodeIO (\ (shape,nodeColor) ->
      (SimpleNodeAttributes {shape = shape,nodeColor = nodeColor})
      )

simpleArcAttributes_tyCon = mkTyCon "DisplayParms" "SimpleArcAttributes"
instance HasTyCon1 SimpleArcAttributes where
   tyCon1 _ = simpleArcAttributes_tyCon
instance Typeable value => HasCodedValue (SimpleArcAttributes value) where
   encodeIO = mapEncodeIO (\ 
      (SimpleArcAttributes {edgePattern = edgePattern,arcColor = arcColor}) 
         ->
      (edgePattern,arcColor)
      )
   decodeIO = mapDecodeIO (\ (edgePattern,arcColor) ->
      (SimpleArcAttributes {edgePattern = edgePattern,arcColor = arcColor})
      )

displayTypeFilter_tyCon = mkTyCon "DisplayParms" "DisplayTypeFilter"
instance HasTyCon DisplayTypeFilter where
   tyCon _ = displayTypeFilter_tyCon
instance HasCodedValue DisplayTypeFilter where
   encodeIO = mapEncodeIO (\ f ->
      case f of
         AllDisplays -> Nothing
         ThisDisplay wd -> Just wd
      )
   decodeIO = mapDecodeIO (\ j ->
      case j of
         Nothing -> AllDisplays
         Just wd -> ThisDisplay wd
      )

nodeTypes_tyCon = mkTyCon "DisplayParms" "NodeTypes"
instance HasTyCon1 NodeTypes where
   tyCon1 _ = nodeTypes_tyCon
instance Typeable value => HasCodedValue (NodeTypes value) where
   encodeIO = mapEncodeIO (\ (NodeTypes l) -> l)
   decodeIO = mapDecodeIO (\ l -> (NodeTypes l))

arcTypes_tyCon = mkTyCon "DisplayParms" "ArcTypes"
instance HasTyCon1 ArcTypes where
   tyCon1 _ = arcTypes_tyCon
instance Typeable value => HasCodedValue (ArcTypes value) where
   encodeIO = mapEncodeIO (\ (ArcTypes l) -> l)
   decodeIO = mapDecodeIO (\ l -> (ArcTypes l))

-- -----------------------------------------------------------------------
-- The external functions
-- -----------------------------------------------------------------------

---
-- Add a node rule, taking priority over previous rules.
addNodeRule :: DisplayTypeFilter -> SimpleNodeAttributes value 
   -> NodeTypes value -> NodeTypes value
addNodeRule displayTypeFilter simpleNodeAttributes (NodeTypes l) =
   NodeTypes ((displayTypeFilter,simpleNodeAttributes):l)

---
-- Add a arc rule, taking priority over previous rules.
addArcRule :: DisplayTypeFilter -> SimpleArcAttributes value 
   -> ArcTypes value -> ArcTypes value
addArcRule displayTypeFilter simpleArcAttributes (ArcTypes l) =
   ArcTypes ((displayTypeFilter,simpleArcAttributes):l)

---
-- getNodeTypeParms extracts the parameter for a particular display type.
getNodeTypeParms :: (Typeable value,HasNodeTypeConfigs nodeTypeParms) 
   => WrappedDisplayType -> NodeTypes value -> Maybe (nodeTypeParms value)
getNodeTypeParms wd1 (NodeTypes []) = Nothing
getNodeTypeParms wd1 (NodeTypes ((f1,atts1):rest)) =
   let
      proceed =
         case f1 of
            AllDisplays -> True
            ThisDisplay wd2 -> (wd1 == wd2)
   in
      if proceed 
         then
            Just (mkNodeTypeParms atts1)
         else
            getNodeTypeParms wd1 (NodeTypes rest)

---
-- getArcTypeParms extracts the parameter for a particular display type.
getArcTypeParms :: (Typeable value,HasArcTypeConfigs arcTypeParms) 
   => WrappedDisplayType -> ArcTypes value -> Maybe (arcTypeParms value)
getArcTypeParms wd1 (ArcTypes []) = Nothing
getArcTypeParms wd1 (ArcTypes ((f1,atts1):rest)) =
   let
      proceed =
         case f1 of
            AllDisplays -> True
            ThisDisplay wd2 -> (wd1 == wd2)
   in
      if proceed 
         then
            Just (mkArcTypeParms atts1)
         else
            getArcTypeParms wd1 (ArcTypes rest)

-- -----------------------------------------------------------------------
-- Converting SimpleNodeAttributes and SimpleArcAttributes to node
-- type parameters
-- -----------------------------------------------------------------------

mkNodeTypeParms :: (Typeable nodeLabel,HasNodeTypeConfigs nodeTypeParms) =>
   SimpleNodeAttributes nodeLabel -> nodeTypeParms nodeLabel
mkNodeTypeParms simpleNodeAttributes =
   (shape simpleNodeAttributes) $$$?
   (nodeColor simpleNodeAttributes) $$$?
   emptyNodeTypeParms
     

mkArcTypeParms :: (Typeable arcLabel,HasArcTypeConfigs arcTypeParms) =>
   SimpleArcAttributes arcLabel -> arcTypeParms arcLabel
mkArcTypeParms simpleArcAttributes =
   (edgePattern simpleArcAttributes) $$$?
   (arcColor simpleArcAttributes) $$$?
   emptyArcTypeParms

-- -----------------------------------------------------------------------
-- SimpleNodeAttributes and SimpleArcAttributes
-- -----------------------------------------------------------------------

---
-- Encodes those attributes (shape, color and so on) which
-- don't depend on the value of the node.
data SimpleNodeAttributes value = SimpleNodeAttributes {
   shape :: Maybe (Shape value),
   nodeColor :: Maybe (Color value)
   }

---
-- Encodes those attributes (edge pattern, color and so on) which
-- don't depend on the value of the arc.
data SimpleArcAttributes value = SimpleArcAttributes {
   edgePattern :: Maybe (EdgePattern value),
   arcColor :: Maybe (Color value)
   }