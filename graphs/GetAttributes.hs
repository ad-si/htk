{- GetAttributes is used by the GraphEditor to pop up HTk windows
   to get information from the user.
   -}
module GetAttributes(
   NodeTypeAttributes(..), 
   getNodeTypeAttributes, -- :: IO (Maybe NodeTypeAttributes)
   NodeAttributes(..),
   getNodeAttributes, -- :: IO (Maybe NodeAttributes)
   ) where

import Exception

import Computation (done)
import QuickReadShow
import Dynamics
import Registry

import HTk hiding (Icon(..))
import Button
import Frame
import Label
import DialogWin
import InputWin

import qualified GraphDisp

------------------------------------------------------------------------
-- NodeTypes
------------------------------------------------------------------------

data ShapeSort = Box | Circle | Ellipse | Rhombus | Triangle | Icon
   deriving (Enum,Read,Show)

instance GUIValue ShapeSort where
   cdefault = Box

data NodeTypeAttributes nodeLabel = NodeTypeAttributes {
   shape :: GraphDisp.Shape nodeLabel,
   nodeTypeTitle :: String
   } deriving Show

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
            Box -> return GraphDisp.Box
            Circle -> return GraphDisp.Circle
            Ellipse -> return GraphDisp.Ellipse
            Rhombus -> return GraphDisp.Rhombus
            Triangle -> return GraphDisp.Triangle
            Icon -> 
               do
                  fname <- getSingleString "Icon filename"
                  return (GraphDisp.Icon fname)
   
         return NodeTypeAttributes {shape=shape,nodeTypeTitle=nodeTypeTitle}        )

getNodeTypeAttributes1 :: IO PreAttributes
-- This returns the sort of shape + the node type title.
getNodeTypeAttributes1 =
   do
      let def = PreAttributes {shapeSort=Box,nodeTypeTitle'=""}
      form <- newInputForm [flexible,value def]
      newEnumField [Box .. Icon] [
         text "Node Shape",
         selector shapeSort,
         modifier (\ old newShape -> old {shapeSort = newShape}),
         parent form
         ]
      newEntryField [
         text "Node Type title",
         selector nodeTypeTitle',
         modifier (\ old newTitle -> old {nodeTypeTitle' = newTitle}),
         parent form,
         width 20
         ]
      inputWin <- newInputWin "Node Type Attributes" form Nothing [modal True]
      result <- sync(triggered inputWin)
      case result of
         Just value -> return value
         Nothing -> cancelQuery

------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------

data NodeAttributes nodeType = NodeAttributes {
   nodeType :: nodeType,
   nodeTitle :: String
   } deriving Show

data NodePreAttributes = NodePreAttributes {
   preNodeType :: String,
   preNodeTitle :: String
   }

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


         (form :: InputForm NodePreAttributes)
            <- newInputForm [flexible,value def]
         newEnumField knownTypeNames [
            text "Node Type",
            selector preNodeType,
            modifier (\ old nodeTypeName -> 
               old {preNodeType = nodeTypeName}),
            parent form
            ]
         newEntryField [
            text "Node title",
            selector preNodeTitle,
            modifier (\ old newTitle -> old {preNodeTitle = newTitle}),
            parent form,
            width 20
            ]
         inputWin <- newInputWin "Node Attributes" form Nothing [modal True]
         result <- sync(triggered inputWin)
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
   }

getArcTypeAttributes :: IO (Maybe ArcTypeAttributes) 
getArcTypeAttributes =
   do
      let def = ArcTypeAttributes {arcTypeTitle=""}
      form <- newInputForm [flexible,value def]
      newEntryField [
         text "Arc Type title",
         selector arcTypeTitle,
         modifier (\ old newTitle -> old {arcTypeTitle = newTitle}),
         parent form,
         width 20
         ]
      inputWin <- newInputWin "Arc Type Attributes" form Nothing [modal True]
      sync(triggered inputWin)
   
------------------------------------------------------------------------
-- Arcs
------------------------------------------------------------------------

------------------------------------------------------------------------
-- General Routines
------------------------------------------------------------------------

displayError :: String -> IO ()
-- This displays an error message until the user clicks "Try Again".
displayError message =
   do
      frame <- newFrame []
      win <- window frame [text "Error"]
      newLabel [value message,parent frame]
      ackButton <- newButton [text "Try Again",parent frame,
         command (\ () -> destroy win)]
      interactor (\iact -> triggered ackButton >>> stop iact)
      sync(destroyed win)

getSingleString :: String -> IO String
-- This gets a single string from the user, prompting with the argument
-- provided.
getSingleString query =
   do
      form <- newInputForm [flexible,value ""]
      (entryField :: EntryField String String) <-
         newEntryField [
            text query,
            selector id,
            modifier (\ oldValue newValue -> newValue),
            parent form,
            width 20
            ]
      inputWin <- newInputWin "" form Nothing [modal True]
      result <- sync(triggered inputWin)
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


