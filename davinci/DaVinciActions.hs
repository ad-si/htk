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

import Registry

import DaVinciGraphTerm

data Register =
   Register {
      nextEvent :: MVar MenuItemId,
      globalActions ::  Registry MenuItemId (IO()),
      nodeActions :: Registry MenuItemId (NodeId -> IO()),
      edgeActions :: Registry MenuItemId (EdgeId -> IO())
      }

newRegister :: IO Register
newRegister =
   do
      nextEvent <- newMVar (MenuItemId 0)
      globalActions <- newRegistry
      nodeActions <- newRegistry
      edgeActions <- newRegistry
      return (Register {
         nextEvent = nextEvent,
         globalActions = globalActions,
         nodeActions = nodeActions,
         edgeActions = edgeActions
         })

next :: MVar MenuItemId -> IO MenuItemId
next mVar =
   do
      (menuItemId@(MenuItemId idNo)) <- takeMVar mVar
      putMVar mVar (MenuItemId (idNo+1))
      return menuItemId

newGlobalEvent :: Register -> IO () -> IO MenuItemId
newGlobalEvent register action =
   do
      menuItemId <- next (nextEvent register)
      setValue (globalActions register) menuItemId action
      return menuItemId

invokeGlobalEvent :: Register -> MenuItemId -> IO ()
invokeGlobalEvent register menuItemId =
   do
      action <- getValue (globalActions register) menuItemId
      action

newNodeEvent :: Register -> (NodeId -> IO ()) -> IO MenuItemId
newNodeEvent register actionFn =
   do
      menuItemId <- next (nextEvent register)
      setValue (nodeActions register) menuItemId actionFn
      return menuItemId

invokeNodeEvent :: Register -> NodeId -> MenuItemId -> IO ()
invokeNodeEvent register nodeId menuItemId =
   do
      actionFn <- getValue (nodeActions register) menuItemId
      actionFn nodeId

newEdgeEvent :: Register -> (EdgeId -> IO ()) -> IO MenuItemId
newEdgeEvent register actionFn =
   do
      menuItemId <- next (nextEvent register)
      setValue (edgeActions register) menuItemId actionFn
      return menuItemId
         
invokeEdgeEvent :: Register -> EdgeId -> MenuItemId -> IO ()
invokeEdgeEvent register edgeId menuItemId =
   do
      actionFn <- getValue (edgeActions register) menuItemId
      actionFn edgeId




