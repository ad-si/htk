{- GetAttributes is used by the GraphEditor to pop up HTk windows
   to get information from the user.
   -}
module GetAttributes(
   NodeTypeAttributes(..), -- instance of Typeable 
   getNodeTypeAttributes, -- :: IO (Maybe NodeTypeAttributes)
   NodeAttributes(..), -- instance of Typeable 
   getNodeAttributes, -- :: IO (Maybe NodeAttributes)
   ArcTypeAttributes(..), -- instance of Typeable
   getArcTypeAttributes, -- :: IO (Maybe ArcTypeAttributes)
   ArcAttributes(..), -- instance of Typeable 
   getArcAttributes, -- :: IO (Maybe ArcAttributes)
   displayError, -- :: String -> IO ()
   ) where

import Exception

import Computation (done)
import QuickReadShow
import Dynamics
import Registry

import HTk
import ModalDialog
import DialogWin
import InputWin
import InputForm

import qualified GraphConfigure

------------------------------------------------------------------------
-- NodeTypes
------------------------------------------------------------------------

data ShapeSort = Box | Circle | Ellipse | Rhombus | Triangle | Icon
   deriving (Enum,Read,Show)

instance GUIValue ShapeSort where
   cdefault = Box

data NodeTypeAttributes nodeLabel = NodeTypeAttributes {
   shape :: GraphConfigure.Shape nodeLabel,
   nodeTypeTitle :: String
   } deriving (Read,Show)

nodeTypeAttributes_tycon = mkTyCon "GetAttributes" "NodeTypeAttributes"

instance HasTyCon1 NodeTypeAttributes where
   tyCon1 _ = nodeTypeAttributes_tycon

data PreAttributes = PreAttributes {
   shapeSort :: ShapeSort,
   nodeTypeTitle' :: String
   }

getNodeTypeAttributes :: IO (Maybe(NodeTypeAttributes nodeLabel))
getNodeTypeAttributes =
   allowCancel (
      do
         PreAttributes {shapeSort=shapeSort,nodeTypeTitle'=nodeTypeTitle} <- 
            getNodeTypeAttributes1
         
         shape <- case shapeSort of
            Box -> return GraphConfigure.Box
            Circle -> return GraphConfigure.Circle
            Ellipse -> return GraphConfigure.Ellipse
            Rhombus -> return GraphConfigure.Rhombus
            Triangle -> return GraphConfigure.Triangle
            GetAttributes.Icon -> 
               do
                  fname <- getSingleString "Icon filename"
                  return (GraphConfigure.Icon fname)
   
         return NodeTypeAttributes {shape=shape,nodeTypeTitle=nodeTypeTitle}        )

getNodeTypeAttributes1 :: IO PreAttributes
-- This returns the sort of shape + the node type title.
getNodeTypeAttributes1 =
   do
      let def = PreAttributes {shapeSort=Box,nodeTypeTitle'=""}
      (iw, form) <- newInputWin "Node Type Attributes" 
                                (\p-> newInputForm p (Just def) []) []
      newEnumField form [Box .. GetAttributes.Icon] [
         -- text "Node Shape",
         selector shapeSort,
         modifier (\ old newShape -> old {shapeSort = newShape})
         ]
      newEntryField form [
         text "Node Type title",
         selector nodeTypeTitle',
         modifier (\ old newTitle -> old {nodeTypeTitle' = newTitle}),
         width 20
         ]
      result <- wait iw True
      case result of
         Just value -> return value
         Nothing -> cancelQuery

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

data NodeAttributes nodeType = NodeAttributes {
   nodeType :: nodeType,
   nodeTitle :: String
   } deriving (Read,Show)

nodeAttributes_tycon = mkTyCon "GetAttributes" "NodeAttributes"

instance HasTyCon1 NodeAttributes where
   tyCon1 _ = nodeAttributes_tycon

data NodePreAttributes = NodePreAttributes {
   preNodeType :: String,
   preNodeTitle :: String
   } deriving Show

getNodeAttributes :: (Registry String nodeType) -> 
   IO (Maybe (NodeAttributes nodeType))
-- getNodeAttributes gets the required attributes of a node given
-- its possible types (with their titles).
getNodeAttributes registry =
   allowCancel (
      do
         knownTypeNames <- listKeys registry
         case knownTypeNames of
            [] -> 
               do
                  displayError "You must first define some node types"
                  cancelQuery
            _ -> done
         let
            def = NodePreAttributes {
               preNodeType=head knownTypeNames,
               preNodeTitle=""
               }
            -- iform p = newInputForm p (Just def) []   
         (inputWin, form) <- newInputWin "Node Attributes" 
                                         (\p-> newInputForm p (Just def) []) []
         newEnumField form knownTypeNames [
            -- text "Node Type",
            selector preNodeType,
            modifier (\ old nodeTypeName -> 
               old {preNodeType = nodeTypeName})
            ]
         newEntryField form [
            text "Node title",
            selector preNodeTitle,
            modifier (\ old newTitle -> old {preNodeTitle = newTitle}),
            width 20
            ]
         result <- wait inputWin True
         case result of
            Just (NodePreAttributes {
               preNodeTitle = nodeTitle,
               preNodeType = nodeTypeName
               }) ->
                  do
                     nodeType <- Registry.getValue registry nodeTypeName 
                     return (NodeAttributes {
                        nodeTitle = nodeTitle,
                        nodeType = nodeType
                        })
            Nothing -> cancelQuery
      )

------------------------------------------------------------------------
-- Arc Types
------------------------------------------------------------------------

data ArcTypeAttributes = ArcTypeAttributes {
   arcTypeTitle :: String
   } deriving (Read,Show)

arcTypeAttributes_tycon = mkTyCon "GetAttributes" "arcTypeAttributes"

instance HasTyCon ArcTypeAttributes where
   tyCon _ = arcTypeAttributes_tycon

getArcTypeAttributes :: IO (Maybe ArcTypeAttributes) 
getArcTypeAttributes =
   do
      let def = ArcTypeAttributes {arcTypeTitle=""}
      (iw, form) <- newInputWin "Arc Type Attributes" 
                                (\p-> newInputForm p (Just def) []) []
      newEntryField form [
         text "Arc Type title",
         selector arcTypeTitle,
         modifier (\ old newTitle -> old {arcTypeTitle = newTitle}),
         width 20
         ]
      wait iw True
   
------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

data ArcAttributes arcType = ArcAttributes {
   arcType :: arcType
   } deriving (Read,Show)

arcAttributes_tycon = mkTyCon "GetAttributes" "ArcAttributes"

instance HasTyCon1 ArcAttributes where
   tyCon1 _ = arcAttributes_tycon

data ArcPreAttributes = ArcPreAttributes {
   preArcType :: String
   }

getArcAttributes :: (Registry String arcType) -> 
   IO (Maybe (ArcAttributes arcType))
-- getArcAttributes gets the required attributes of an arc given
-- its possible types (with their titles).
getArcAttributes registry =
   allowCancel (
      do
         knownTypeNames <- listKeys registry
         case knownTypeNames of
            [] -> 
               do
                  displayError "You must first define some arc types"
                  cancelQuery
            _ -> done
         let
            def = ArcPreAttributes {
               preArcType=head knownTypeNames
               }
         (iw, form) <- newInputWin "Arc Attributes" 
	                           (\p-> newInputForm p (Just def) []) []
         newEnumField form knownTypeNames [
            -- text "Arc Type",
            selector preArcType,
            modifier (\ old arcTypeName -> 
               old {preArcType = arcTypeName})
            ]
         result <- wait iw True
         case result of
            Just (ArcPreAttributes {
               preArcType = arcTypeName
               }) ->
                  do
                     arcType <- Registry.getValue registry arcTypeName 
                     return (ArcAttributes {
                        arcType = arcType
                        })
            Nothing -> cancelQuery
      )

------------------------------------------------------------------------
-- General Routines
------------------------------------------------------------------------

displayError :: String -> IO ()
-- This displays an error message until the user clicks "Try Again".
displayError message = newErrorWin message []
{-
   do
      frame <- newFrame []
      win <- window frame [text "Error"]
      newLabel [value message,parent frame]
      ackButton <- newButton [text "Try Again",parent frame,
         command (\ () -> destroy win)]
      interactor (\iact -> triggered ackButton >>> stop iact)
      sync(destroyed win)
 -} 


getSingleString :: String -> IO String
-- This gets a single string from the user, prompting with the argument
-- provided.
getSingleString query =
   do
      (inputWin, form) <- newInputWin "" (\p-> newInputForm p (Just "") []) []
      (entryField :: EntryField String String) <-
         newEntryField form [
            text query,
            selector id,
            modifier (\ oldValue newValue -> newValue),
            width 20
            ]
      result <- wait inputWin True
      case result of
         Just value -> return value
         Nothing -> cancelQuery

newtype CancelException = CancelException ()
cancelException_tag = mkTyCon "GetAttributes" "CancelException"

instance HasTyCon CancelException where
   tyCon _ = cancelException_tag

cancelQuery :: IO anything
cancelQuery = throwDyn (CancelException ())

allowCancel :: IO a -> IO (Maybe a)
allowCancel action = 
   catchDyn 
      (do
         result <- action
         return (Just result)
         )
      (\ (CancelException ()) -> return Nothing)


