{- DaVinciActions maintains a DaVinci-wide register of actions to be performed
   when, for example, the user clicks a menu event.  As it is fairly
   low-level (to be called from DaVinciCore), it knows the data for
   these events by their primitive identifiers, EG  their MenuItemId.
   -}
module DaVinciActions(
   Register, 
      -- the register of actions.  There is one of these for
      -- each graph.
   newRegister, -- IO Register

   newGlobalEvent, -- Register -> IO () -> IO MenuItemId
   invokeGlobalEvent, -- Register -> MenuItemId -> IO ()

   newNodeEvent, -- Register -> (NodeId -> IO ()) -> IO MenuItemId
   invokeNodeEvent, -- Register -> NodeId -> MenuItemId -> IO ()

   newEdgeEvent, -- Register -> (EdgeId -> IO ()) -> IO MenuItemId
   invokeEdgeEvent, -- Register -> EdgeId -> MenuItemId -> IO ()
   ) where

import Concurrent

import FiniteMap

import DaVinciGraphTerm

type Register = MVar RegisterData

data RegisterData =
   RegisterData {
      nextEvent :: MenuItemId,
      globalActions :: FiniteMap MenuItemId (IO()),
      nodeActions :: FiniteMap MenuItemId (NodeId -> IO()),
      edgeActions :: FiniteMap MenuItemId (EdgeId -> IO())
      }

newRegister :: IO Register
newRegister =
   newMVar (RegisterData {
      nextEvent = MenuItemId 0,
      globalActions = emptyFM,
      nodeActions = emptyFM,
      edgeActions = emptyFM
      })

next :: MenuItemId -> MenuItemId
next (MenuItemId idNo) = MenuItemId (idNo + 1)

newGlobalEvent :: Register -> IO () -> IO MenuItemId
newGlobalEvent mVar action =
   do
      registerData <- takeMVar mVar
      let
         menuItemId = nextEvent registerData
         newRegisterData = registerData {
            nextEvent = next (menuItemId),
            globalActions = 
               addToFM (globalActions registerData) menuItemId action
            }
      putMVar mVar newRegisterData
      return menuItemId

invokeGlobalEvent :: Register -> MenuItemId -> IO ()
invokeGlobalEvent mVar menuItemId =
   do
      registerData <- readMVar mVar
      let
         Just action = lookupFM (globalActions registerData) menuItemId
      action

newNodeEvent :: Register -> (NodeId -> IO ()) -> IO MenuItemId
newNodeEvent mVar actionFn =
   do
      registerData <- takeMVar mVar
      let
         menuItemId = nextEvent registerData
         newRegisterData = registerData {
            nextEvent = next (menuItemId),
            nodeActions = 
               addToFM (nodeActions registerData) menuItemId actionFn
            }
      putMVar mVar newRegisterData
      return menuItemId

invokeNodeEvent :: Register -> NodeId -> MenuItemId -> IO ()
invokeNodeEvent register nodeId menuItemId =
   do
      registerData <- readMVar register
      let
         Just nodeActionFn = lookupFM (nodeActions registerData) menuItemId
      nodeActionFn nodeId

newEdgeEvent :: Register -> (EdgeId -> IO ()) -> IO MenuItemId
newEdgeEvent mVar actionFn =
   do
      registerData <- takeMVar mVar
      let
         menuItemId = nextEvent registerData
         newRegisterData = registerData {
            nextEvent = next (menuItemId),
            edgeActions = 
               addToFM (edgeActions registerData) menuItemId actionFn
            }
      putMVar mVar newRegisterData
      return menuItemId
         
invokeEdgeEvent :: Register -> EdgeId -> MenuItemId -> IO ()
invokeEdgeEvent register edgeId menuItemId =
   do
      registerData <- readMVar register
      let
         Just edgeActionFn = lookupFM (edgeActions registerData) menuItemId
      edgeActionFn edgeId




