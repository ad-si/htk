{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | GetAttributes is used by the GraphEditor to pop up HTk windows
-- to get information from the user.
module Graphs.GetAttributes(
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

import Control.Exception as Exception

import Util.Dynamics
import Util.Registry hiding (getValue)
import qualified Util.Registry as Registry (getValue)
import Util.Messages

import HTk.Toplevel.HTk hiding (Icon)
import HTk.Toolkit.InputWin
import HTk.Toolkit.InputForm

import qualified Graphs.GraphConfigure as GraphConfigure

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
   } deriving (Read,Show,Typeable)

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
            Icon ->
               do
                  fname <- getSingleString "Icon filename"
                  return (GraphConfigure.Icon fname)

         return NodeTypeAttributes {shape=shape,nodeTypeTitle=nodeTypeTitle}        )

getNodeTypeAttributes1 :: IO PreAttributes
-- This returns the sort of shape + the node type title.
getNodeTypeAttributes1 =
   do
      let def = PreAttributes {shapeSort=Box,nodeTypeTitle'=""}
      (iw, form) <- createInputWin "Node Type Attributes"
                                (\p-> newInputForm p (Just def) []) []
      newEnumField form [Box .. Icon] [
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
   } deriving (Read,Show,Typeable)

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
            _ -> return ()
         let
            def = NodePreAttributes {
               preNodeType=head knownTypeNames,
               preNodeTitle=""
               }
            -- iform p = newInputForm p (Just def) []
         (inputWin, form) <- createInputWin "Node Attributes"
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
   } deriving (Read,Show,Typeable)

getArcTypeAttributes :: IO (Maybe ArcTypeAttributes)
getArcTypeAttributes =
   do
      let def = ArcTypeAttributes {arcTypeTitle=""}
      (iw, form) <- createInputWin "Arc Type Attributes"
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
   } deriving (Read,Show,Typeable)

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
            _ -> return ()
         let
            def = ArcPreAttributes {
               preArcType=head knownTypeNames
               }
         (iw, form) <- createInputWin "Arc Attributes"
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
-- This displays an error message.
displayError = errorMess

getSingleString :: String -> IO String
-- This gets a single string from the user, prompting with the argument
-- provided.
getSingleString query =
   do
      (inputWin, form) <- createInputWin "" (\p-> newInputForm p (Just "") []) []
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

newtype CancelException = CancelException () deriving (Typeable)

cancelQuery :: IO anything
cancelQuery = throw $ toDyn (CancelException ())

allowCancel :: IO a -> IO (Maybe a)
allowCancel action =
   Exception.catchJust
      (\ e -> case fromDynamic e of
           Just (CancelException ()) -> return $ Just ()
           _ -> return Nothing)
      (do
         result <- action
         return (Just result)
         )
      (\ _ -> return Nothing)


