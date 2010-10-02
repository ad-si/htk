{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module describes an empty display graph sort.  In other words, it
-- displays nothing.  Not a lot of use you might think, but we use it for
-- the MMiSS API to get a version graph without invoking daVinci.
module Graphs.EmptyGraphSort(
   emptyGraphSort,
   ) where

import Data.IORef

import Util.Dynamics
import Util.Delayer
import Util.Computation
import Util.Object
import Util.ExtendedPrelude
import Util.VariableList

import Events.Events
import Events.Destructible
import Events.Channels

import Graphs.GraphDisp hiding (redraw)
import Graphs.GraphConfigure

-- ---------------------------------------------------------------------------
-- Datatypes
-- ---------------------------------------------------------------------------

data EmptyGraph = EmptyGraph {
   delayer :: Delayer,
   destructChan :: Channel (),
   oId :: ObjectID
   } deriving (Typeable)

data EmptyGraphParms = EmptyGraphParms

data EmptyNode value = EmptyNode {
   ioRefN :: IORef value,
   oIdN :: ObjectID
   } deriving (Typeable)

data EmptyNodeType value = EmptyNodeType deriving (Typeable)

data EmptyNodeTypeParms value = EmptyNodeTypeParms

data EmptyArc value = EmptyArc {
   ioRefE :: IORef (Maybe value),
   oIdE :: ObjectID
   } deriving (Typeable)

newtype EmptyArcType value = EmptyArcType {oIdET :: ObjectID}
   deriving (Typeable)

data EmptyArcTypeParms value = EmptyArcTypeParms

-- ---------------------------------------------------------------------------
-- The sort
-- ---------------------------------------------------------------------------

emptyGraphSort :: Graph EmptyGraph
   EmptyGraphParms EmptyNode EmptyNodeType EmptyNodeTypeParms
   EmptyArc EmptyArcType EmptyArcTypeParms
emptyGraphSort = displaySort

instance GraphAllConfig EmptyGraph EmptyGraphParms
   EmptyNode EmptyNodeType EmptyNodeTypeParms
   EmptyArc EmptyArcType EmptyArcTypeParms

-- ---------------------------------------------------------------------------
-- Instances for EmptyGraph/EmptyGraphParms
-- ---------------------------------------------------------------------------

instance Eq EmptyGraph where
   (==) = mapEq oId

instance Ord EmptyGraph where
   compare = mapOrd oId

instance Destroyable EmptyGraph where
   destroy graph = sync (noWait (send (destructChan graph) ()))

instance Destructible EmptyGraph where
   destroyed graph = receive (destructChan graph)

instance HasDelayer EmptyGraph where
   toDelayer = delayer

instance GraphClass EmptyGraph where
   redrawPrim _ = done

instance NewGraph EmptyGraph EmptyGraphParms where
   newGraphPrim _ =
      do
         delayer <- newDelayer
         destructChan <- newChannel
         oId <- newObject
         let
            graph = EmptyGraph {
               delayer = delayer,
               destructChan = destructChan,
               oId = oId
               }
         return graph

instance GraphParms EmptyGraphParms where
   emptyGraphParms = EmptyGraphParms

instance GraphConfig graphConfig
   => HasConfig graphConfig EmptyGraphParms where

   ($$) _ parms = parms

   configUsed _ _ = True

-- ---------------------------------------------------------------------------
-- Instances for EmptyNode, EmptyNodeType, EmptyNodeTypeParms
-- ---------------------------------------------------------------------------

instance Eq1 EmptyNode where
   eq1 = mapEq oIdN

instance Ord1 EmptyNode where
   compare1 = mapOrd oIdN

instance NodeClass EmptyNode

instance NodeTypeClass EmptyNodeType

instance NodeTypeParms EmptyNodeTypeParms where
   emptyNodeTypeParms = EmptyNodeTypeParms

   coMapNodeTypeParms _ _ = EmptyNodeTypeParms

instance NewNode EmptyGraph EmptyNode EmptyNodeType where
   newNodePrim _ _ value =
      do
         ioRef <- newIORef value
         oId <- newObject
         let
            node = EmptyNode {ioRefN = ioRef,oIdN = oId}
         return node
   setNodeTypePrim _ _ _ = done

instance DeleteNode EmptyGraph EmptyNode where
   deleteNodePrim _ _ = done
   getNodeValuePrim _ node = readIORef (ioRefN node)
   setNodeValuePrim _ node = writeIORef (ioRefN node)
   getMultipleNodesPrim _ getA =
      do
         a <- getA never
         return a

instance SetNodeFocus EmptyGraph EmptyNode where
   setNodeFocusPrim _ _ = done


instance NewNodeType EmptyGraph EmptyNodeType EmptyNodeTypeParms where
   newNodeTypePrim _ _ = return EmptyNodeType

instance NodeTypeConfig nodeTypeConfig
   => HasConfigValue nodeTypeConfig EmptyNodeTypeParms where

   ($$$) _ parms = parms

   configUsed' _ _ = True

instance HasModifyValue FontStyle EmptyGraph EmptyNode where
   modify _ _ _ = done

instance HasModifyValue Border EmptyGraph EmptyNode where
   modify _ _ _ = done

instance HasModifyValue NodeArcsHidden EmptyGraph EmptyNode where
   modify _ _ _ = done

-- ---------------------------------------------------------------------------
-- Instances for EmptyArc, EmptyArcType, EmptyArcTypeParms
-- ---------------------------------------------------------------------------

instance Eq1 EmptyArc where
   eq1 = mapEq oIdE

instance Ord1 EmptyArc where
   compare1 = mapOrd oIdE

instance Eq1 EmptyArcType where
   eq1 = mapEq oIdET

instance Ord1 EmptyArcType where
   compare1 = mapOrd oIdET

instance ArcClass EmptyArc

instance ArcTypeClass EmptyArcType where
   invisibleArcType = EmptyArcType {oIdET = staticObject 1}

instance ArcTypeParms EmptyArcTypeParms where
   emptyArcTypeParms = EmptyArcTypeParms

   invisibleArcTypeParms = EmptyArcTypeParms

   coMapArcTypeParms _ _ = EmptyArcTypeParms

instance NewArcType EmptyGraph EmptyArcType EmptyArcTypeParms where
   newArcTypePrim _ _ =
      do
         oId <- newObject
         return (EmptyArcType {oIdET = oId})

instance NewArc EmptyGraph EmptyNode EmptyNode EmptyArc EmptyArcType where
   newArcPrim _ _ value _ _ =
      do
         ioRef <- newIORef (Just value)
         oId <- newObject
         return (EmptyArc {ioRefE = ioRef,oIdE = oId})

   newArcListDrawerPrim _ _ = listDrawer

instance SetArcType EmptyGraph EmptyArc EmptyArcType where
   setArcTypePrim _ _ _ = done

listDrawer :: ListDrawer
   (EmptyArcType value,value,WrappedNode EmptyNode) (EmptyArc value)
listDrawer =
   let
      newPos _ endOpt =
         do
            ioRef <- newIORef (mapOpt endOpt)
            oId <- newObject
            return (EmptyArc {ioRefE = ioRef,oIdE = oId})
      setPos (EmptyArc {ioRefE = ioRef}) endOpt =
         writeIORef ioRef (mapOpt endOpt)
      delPos _ = done

      mapOpt = fmap (\ (_,value,_) -> value)
   in
      ListDrawer {
         newPos = newPos,
         setPos = setPos,
         delPos = delPos,
         redraw = done
         }

instance DeleteArc EmptyGraph EmptyArc where
   deleteArcPrim _ _ = done
   setArcValuePrim _ (EmptyArc {ioRefE = ioRef}) value =
      writeIORef ioRef (Just value)
   getArcValuePrim _ (EmptyArc {ioRefE = ioRef}) =
      do
         valueOpt <- readIORef ioRef
         case valueOpt of
            Just value -> return value


instance ArcTypeConfig arcTypeConfig
   => HasConfigValue arcTypeConfig EmptyArcTypeParms where

   ($$$) _ parms = parms

   configUsed' _ _ = True

