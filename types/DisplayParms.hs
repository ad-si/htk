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

   simpleNodeTypesForm,
   readDisplay,
   defaultNodeTypes,

   valueTitleSource,
   fontStyleSource,
   ) where

import Maybe
import Char

import Computation
import Dynamics
import BinaryAll(ReadShow(..))

import HTk
import SimpleForm

import GraphConfigure hiding (Shape(..))
import GraphConfigure (Shape)
import qualified GraphConfigure

import GraphDisp

import DisplayTypes
import CodedValue
import Link
import ObjectTypes
import View

-- -----------------------------------------------------------------------
-- The Types and Datatypes
-- -----------------------------------------------------------------------

-- | Selects particular display types
data DisplayTypeFilter =
      AllDisplays  -- ^ select every display
   |  ThisDisplay WrappedDisplayType  -- ^ select just this display


-- | NodeTypes is a list of display type filters with SimpleNodeAttributes
newtype NodeTypes value = 
   NodeTypes [(DisplayTypeFilter,SimpleNodeAttributes value)]

-- | ArcTypes is a list of display type filters with SimpleArcAttributes
newtype ArcTypes value = 
   ArcTypes [(DisplayTypeFilter,SimpleArcAttributes value)]

-- -----------------------------------------------------------------------
-- The Datatypes
-- -----------------------------------------------------------------------

-- | Returns \"Nothing\" for any display type
emptyNodeTypes :: NodeTypes value
emptyNodeTypes = NodeTypes []
   
-- | Returns \"Nothing\" for any display type
emptyArcTypes :: ArcTypes value
emptyArcTypes = ArcTypes []

-- -----------------------------------------------------------------------
-- Instances of HasBinary.
-- -----------------------------------------------------------------------

instance Monad m => HasBinary (Shape value) m where
   writeBin = mapWrite ReadShow
   readBin = mapRead (\ (ReadShow x) -> x)

instance Monad m => HasBinary (Color value) m where
   writeBin = mapWrite ReadShow
   readBin = mapRead (\ (ReadShow x) -> x)

instance Monad m => HasBinary (EdgePattern value) m where
   writeBin = mapWrite ReadShow
   readBin = mapRead (\ (ReadShow x) -> x)

instance Monad m => HasBinary (SimpleNodeAttributes value) m where
   writeBin = mapWrite (\ 
      (SimpleNodeAttributes {shape = shape,nodeColor = nodeColor}) ->
      (shape,nodeColor)
      )
   readBin = mapRead (\ (shape,nodeColor) ->
      (SimpleNodeAttributes {shape = shape,nodeColor = nodeColor})
      )

instance Monad m => HasBinary (SimpleArcAttributes value) m where
   writeBin = mapWrite (\ 
      (SimpleArcAttributes {edgePattern = edgePattern,arcColor = arcColor}) 
         ->
      (edgePattern,arcColor)
      )
   readBin = mapRead (\ (edgePattern,arcColor) ->
      (SimpleArcAttributes {edgePattern = edgePattern,arcColor = arcColor})
      )

instance HasBinary DisplayTypeFilter CodingMonad where
   writeBin = mapWrite (\ f ->
      case f of
         AllDisplays -> Nothing
         ThisDisplay wd -> Just wd
      )
   readBin = mapRead (\ j ->
      case j of
         Nothing -> AllDisplays
         Just wd -> ThisDisplay wd
      )

instance HasBinary (NodeTypes value) CodingMonad where
   writeBin = mapWrite (\ (NodeTypes l) -> l)
   readBin = mapRead (\ l -> (NodeTypes l))

instance HasBinary (ArcTypes value) CodingMonad where
   writeBin = mapWrite (\ (ArcTypes l) -> l)
   readBin = mapRead (\ l -> (ArcTypes l))

-- -----------------------------------------------------------------------
-- The external functions
-- -----------------------------------------------------------------------

-- | Add a node rule, taking priority over previous rules.
addNodeRule :: DisplayTypeFilter -> SimpleNodeAttributes value 
   -> NodeTypes value -> NodeTypes value
addNodeRule displayTypeFilter simpleNodeAttributes (NodeTypes l) =
   NodeTypes ((displayTypeFilter,simpleNodeAttributes):l)

-- | Add a arc rule, taking priority over previous rules.
addArcRule :: DisplayTypeFilter -> SimpleArcAttributes value 
   -> ArcTypes value -> ArcTypes value
addArcRule displayTypeFilter simpleArcAttributes (ArcTypes l) =
   ArcTypes ((displayTypeFilter,simpleArcAttributes):l)

-- | getNodeTypeParms extracts the parameter for a particular display type.
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

-- | getArcTypeParms extracts the parameter for a particular display type.
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

-- | Encodes those attributes (shape, color and so on) which
-- don\'t depend on the value of the node.
data SimpleNodeAttributes value = SimpleNodeAttributes {
   shape :: Maybe (Shape value),
   nodeColor :: Maybe (Color value)
   }

-- | Encodes those attributes (edge pattern, color and so on) which
-- don\'t depend on the value of the arc.
data SimpleArcAttributes value = SimpleArcAttributes {
   edgePattern :: Maybe (EdgePattern value),
   arcColor :: Maybe (Color value)
   }

-- -----------------------------------------------------------------------
-- Forms for attributes
-- -----------------------------------------------------------------------

----
-- Allows the user to select for shape and colour.
simpleNodeTypesForm :: Form (NodeTypes value)
simpleNodeTypesForm =
   fmap
      (\ (color,shape) -> 
         addNodeRule
            AllDisplays
            (SimpleNodeAttributes {nodeColor = Just color,shape = Just shape})
            emptyNodeTypes
         )
      (colorForm // shapeForm)

-- | Select a colour 
-- We steal the following trick from htk\/examples\/toolkit\/Mainsimpleform.hs
data OurColour = White | Black| Red | Orange | Yellow | Green | Blue | Violet 
               deriving (Bounded,Enum,Read,Show)

instance HasConfigRadioButton OurColour where
   configRadioButton colour = HTk.background (show colour)

colorForm :: Form (GraphConfigure.Color value)
colorForm = 
   mapForm
      (\ radColour ->
         case radColour of
            NoRadio -> hasError "No colour specified"
            Radio (col :: OurColour) 
               -> hasValue (convertColour col)
         )
      (newFormEntry EmptyLabel NoRadio)

convertColour :: OurColour -> GraphConfigure.Color a
convertColour Green = GraphConfigure.Color ("#98ecb2")
convertColour Blue  = GraphConfigure.Color ("#98ceda")
convertColour col = GraphConfigure.Color (show col)

-- 
-- Select a shape.  For this we need our own shape type, as we exclude
-- GraphConfigure's Icon option.
data OurShape = Box | Circle | Ellipse | Rhombus | Triangle 
   deriving (Read,Show,Bounded,Enum)

instance HasConfigRadioButton OurShape where
   configRadioButton value = text (show value)

convertShape :: OurShape -> Shape value 
convertShape Box = GraphConfigure.Box
convertShape Circle = GraphConfigure.Circle
convertShape Ellipse = GraphConfigure.Ellipse
convertShape Rhombus = GraphConfigure.Rhombus
convertShape Triangle = GraphConfigure.Triangle

shapeForm :: Form (Shape value)
shapeForm = 
   mapForm
      (\ radShape ->
         case radShape of
            NoRadio -> hasError "No shape specified"
            Radio (shape :: OurShape) -> hasValue (convertShape shape)
         )
      (newFormEntry EmptyLabel NoRadio)

-- -----------------------------------------------------------------------
-- A simple format for describing graphics instructions.
-- For the time being we simply regard this as a list of words, which
-- are either colours or shapes.
-- -----------------------------------------------------------------------

readDisplay :: String -> NodeTypes a
readDisplay str =
   let
      instructions = words str

      normalise (c:cs) = (toUpper c):(map toLower cs)
         -- uppercase first char, lowercase rest
         -- normalise "" shouldn't happen as "words" doesn't produce an
         -- empty string.

      doInstruction :: (OurColour,OurShape) -> String 
         -> (OurColour,OurShape)
      doInstruction (c,s) word0 =
         let
            word1 = normalise word0
         in
            case reads word1 of
               [(color,"")] -> (color,s)
               _ -> case reads word1 of
                  [(shape,"")] -> (c,shape)
                  _ -> error ("Graphics instruction "++word0++
                     " not understood")

      (c,s) = foldl doInstruction (White,Box) instructions
   in
      createSimpleNodeTypes c s

createSimpleNodeTypes :: OurColour -> OurShape -> NodeTypes a
createSimpleNodeTypes c s =
   addNodeRule
      AllDisplays
      (SimpleNodeAttributes {nodeColor = Just (convertColour c),
         shape = Just (convertShape s)})
      emptyNodeTypes


defaultNodeTypes :: NodeTypes a
defaultNodeTypes = createSimpleNodeTypes White Box

-- -----------------------------------------------------------------------
-- A function for getting a parameter which describes the title from the
-- object's nodeTitleSource.
-- -----------------------------------------------------------------------

valueTitleSource :: ObjectType objectType object 
   => View -> ValueTitleSource (Link object)
valueTitleSource view =
   let
      getNodeTitleSource link =
         do
            object <- readLink view link
            return (nodeTitleSourcePrim object)
   in
      ValueTitleSource getNodeTitleSource

-- -----------------------------------------------------------------------
-- Another function which describes whether the object in question is 
-- dirty or not by making the fontstyle italic when it is dirty.
-- -----------------------------------------------------------------------

fontStyleSource :: ObjectType objectType object
   => View -> FontStyleSource (Link object)
fontStyleSource view =
   let
      getFontStyleSource link =
         do
            versioned <- fetchLink view link
            let
               isDirtySource = getIsDirtySimpleSource versioned
               fontStyleSource =
                  fmap
                     (\ isDirty -> if isDirty 
                        then
                           BoldItalicFontStyle
                        else
                           BoldFontStyle
                        )
                     isDirtySource

            return fontStyleSource
   in
      FontStyleSource getFontStyleSource
