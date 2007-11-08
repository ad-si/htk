-- | This module contains code for generating the specialNodeActions of
-- the ObjectTypes module.
module SpecialNodeActions(
   NodeActionSource,
   newNodeActionSource,
   setArcsHidden,
   getNodeActions,
   emptyNodeActions,
   ) where

import KeyedChanges
import Sources
import Dynamics
import Computation(done)

import GraphConfigure

-- ------------------------------------------------------------------
-- The producer's interface
-- ------------------------------------------------------------------

data NodeAction =
      ArcsHidden NodeArcsHidden

newtype NodeActionSource = NodeActionSource (KeyedChanges Char NodeAction)

newNodeActionSource :: IO NodeActionSource
newNodeActionSource =
   do
      keyedChanges <- newKeyedChanges
      return (NodeActionSource keyedChanges)

setArcsHidden :: NodeActionSource -> NodeArcsHidden -> IO ()
setArcsHidden (NodeActionSource keyedChanges) nodeArcsHidden =
   sendOrDelete nodeArcsHidden 'A' (ArcsHidden nodeArcsHidden) keyedChanges

sendOrDelete :: NodeArcsHidden -> Char -> NodeAction
   -> (KeyedChanges Char NodeAction) -> IO ()
sendOrDelete modification =
   if isDef modification then deleteKeyedChange else sendKeyedChanges

-- ------------------------------------------------------------------
-- The consumer's interface
-- ------------------------------------------------------------------

emptyNodeActions :: (HasNodeModifies graph node,Typeable value)
   => SimpleSource (graph -> node value -> IO ())
emptyNodeActions = staticSimpleSource . const . const $ done

getNodeActions :: (HasNodeModifies graph node,Typeable value)
   => NodeActionSource -> SimpleSource (graph -> node value -> IO ())
getNodeActions (NodeActionSource keyedChanges) =
   let
      source = toSource keyedChanges

      apply :: (HasNodeModifies graph node,Typeable value)
         => NodeAction -> graph -> node value -> IO ()
      apply (ArcsHidden arcsHidden) = modify arcsHidden

      applyMultiple :: (HasNodeModifies graph node,Typeable value)
         => [NodeAction] -> graph -> node value -> IO ()
      applyMultiple list graph node =
         mapM_
            (\ action -> apply action graph node)
            list

      actionSimpleSource :: (HasNodeModifies graph node,Typeable value)
         => SimpleSource (graph -> node value -> IO ())
      actionSimpleSource = SimpleSource (
         (map1 applyMultiple) . (map2 apply) $ source
         )
   in
      actionSimpleSource
