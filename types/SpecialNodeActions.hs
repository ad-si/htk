{- This module contains code for generating the specialNodeActions of
   the ObjectTypes module. -}
module SpecialNodeActions(
   NodeActionSource,
   newNodeActionSource,
   setBorder,
   setFontStyle,
   setArcsHidden,
   getNodeActions,
   emptyNodeActions,
   ) where

import KeyedChanges
import Sources
import Broadcaster
import Dynamics
import Computation(done)

import GraphConfigure

-- ------------------------------------------------------------------
-- The producer's interface
-- ------------------------------------------------------------------

data NodeAction = 
      Border Border
   |  FontStyle FontStyle
   |  ArcsHidden NodeArcsHidden

newtype NodeActionSource = NodeActionSource (KeyedChanges Char NodeAction)

newNodeActionSource :: IO NodeActionSource
newNodeActionSource = 
   do
      keyedChanges <- newKeyedChanges
      return (NodeActionSource keyedChanges)

setBorder :: NodeActionSource -> Border -> IO ()
setBorder (NodeActionSource keyedChanges) border =
   sendOrDelete border 'B' (Border border) keyedChanges

setFontStyle :: NodeActionSource -> FontStyle -> IO ()
setFontStyle (NodeActionSource keyedChanges) fontStyle =
   sendOrDelete fontStyle 'F' (FontStyle fontStyle) keyedChanges

setArcsHidden :: NodeActionSource -> NodeArcsHidden -> IO ()
setArcsHidden (NodeActionSource keyedChanges) nodeArcsHidden =
   sendOrDelete nodeArcsHidden 'A' (ArcsHidden nodeArcsHidden) keyedChanges

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
      apply (Border border) = modify border
      apply (FontStyle fontStyle) = modify fontStyle
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