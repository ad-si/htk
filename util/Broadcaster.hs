{- A Broadcaster/SimpleBroadcaster is a variable Source/SimpleSource paired 
   with its update function
   -}
module Broadcaster(
   -- instances of HasSource (and so CanAddSinks)
   GeneralBroadcaster,
   Broadcaster,
   SimpleBroadcaster,

   newBroadcaster, -- :: x -> IO (Broadcaster x d)
   newSimpleBroadcaster, -- :: x -> IO (SimpleBroadcaster x)
   newGeneralBroadcaster, -- :: x -> IO (GeneralBroadcaster x d)

   BroadcasterClass(broadcast),
      -- sends an update to a broadcaster.

   applySimpleUpdate, -- :: SimpleBroadcaster x -> (x -> x) -> IO ()

   applyUpdate, -- :: Broadcaster x d -> (x -> (x,[d])) -> IO ()

   applyGeneralUpdate, -- :: GeneralBroadcaster x d -> (x -> (x,[d],extra)) -> IO extra

   switchOffSimpleSource, 
      -- :: SimpleSource a -> IO (SimpleSource a,IO (IO ()))
      -- Replace a SimpleSource by another which comes with a switch-off 
      -- function, which temporarily blocks further updates.
      -- The action returned by the switch-off function switches the source 
      -- again.

   mirrorSimpleSource,
      -- :: SimpleSource a -> IO (SimpleSource a,IO ())
      -- Replace a SimpleSource by another which mirrors it, but only copies
      -- from it once, hopefully saving CPU time.
      -- The IO action stops the mirroring.
      -- We use unsafeInterleaveIO to prevent the mirroring beginning before it
      -- is actually needed.  However we don't provide any means to stop the
      -- mirroring when it is no longer needed, yet.

   ) where

import qualified Control.Concurrent.MVar as MVar

import Sink
import Sources

-- -----------------------------------------------------------------
-- Datatypes
-- -----------------------------------------------------------------

data GeneralBroadcaster x d = GeneralBroadcaster {
   source' :: Source x d,
   updater :: Updater x d
   }

data Broadcaster x d = Broadcaster {
   source :: Source x d,
   updateAct :: (x -> (x,[d])) -> IO ()
   }

data SimpleBroadcaster x = SimpleBroadcaster {
   simpleSource :: SimpleSource x,
   updateAct2 :: (x -> x) -> IO ()
   }

-- -----------------------------------------------------------------
-- Creation
-- -----------------------------------------------------------------

newBroadcaster :: x -> IO (Broadcaster x d)
newBroadcaster x =
   do
      (source,updateAct) <- variableSource x
      return (Broadcaster {source = source,updateAct = updateAct})

newSimpleBroadcaster :: x -> IO (SimpleBroadcaster x)
newSimpleBroadcaster x =
   do
      (source,updateAct') <- variableSource x
      let
         apply updateFn oldX =
           let
              newX = updateFn oldX
           in
              (newX,[newX])

      return (SimpleBroadcaster {simpleSource = SimpleSource source,
         updateAct2 = updateAct' . apply})

newGeneralBroadcaster :: x -> IO (GeneralBroadcaster x d)
newGeneralBroadcaster x =
   do
      (source,updater) <- variableGeneralSource x
      return (GeneralBroadcaster {source' = source,updater = updater})

-- -----------------------------------------------------------------
-- Sending values
-- -----------------------------------------------------------------

class BroadcasterClass broadcaster value | broadcaster -> value where
   broadcast :: broadcaster -> value -> IO () 

instance BroadcasterClass (Broadcaster x d) (x,[d]) where
   broadcast (Broadcaster {updateAct = updateAct}) (x,ds) =
      updateAct (\ _ -> (x,ds))

instance BroadcasterClass (SimpleBroadcaster x) x where
   broadcast (SimpleBroadcaster {updateAct2 = updateAct2}) x = 
      updateAct2 (\ _ -> x)

applySimpleUpdate :: SimpleBroadcaster x -> (x -> x) -> IO ()
applySimpleUpdate simpleBroadcaster updateFn =
   updateAct2 simpleBroadcaster updateFn

applyUpdate :: Broadcaster x d -> (x -> (x,[d])) -> IO ()
applyUpdate (Broadcaster {updateAct = updateAct}) updateFn =
   updateAct updateFn

applyGeneralUpdate :: GeneralBroadcaster x d -> (x -> (x,[d],extra)) -> IO extra
applyGeneralUpdate (GeneralBroadcaster {updater = updater}) updateAct =
   applyToUpdater updater updateAct

-- -----------------------------------------------------------------
-- Instances of HasSource and HasSimpleSource
-- -----------------------------------------------------------------

instance HasSource (Broadcaster x d) x d where
   toSource broadcaster = source broadcaster

instance HasSource (SimpleBroadcaster x) x x where
   toSource broadcaster = toSource . toSimpleSource $ broadcaster

instance HasSource (GeneralBroadcaster x d) x d where
   toSource generalBroadcaster = source' generalBroadcaster

instance HasSimpleSource (SimpleBroadcaster x) x where
   toSimpleSource simpleBroadcaster = simpleSource simpleBroadcaster


-- -----------------------------------------------------------------
-- switchOffSimpleSource
-- -----------------------------------------------------------------

-- | Replace a SimpleSource by another which comes with a switch-off function,
-- which temporarily blocks further updates.
-- The action returned by the switch-off function switches the source back on
-- again.
switchOffSimpleSource :: SimpleSource a -> IO (SimpleSource a,IO (IO ()))
switchOffSimpleSource simpleSource =
   do
      broadcaster <- newSimpleBroadcaster simpleSource
      let
         switchOffSource = staticSimpleSourceIO (readContents simpleSource)

         switchOff =
            do
               broadcast broadcaster switchOffSource
               return (broadcast broadcaster simpleSource)

         newSource =
            do
               source <- toSimpleSource broadcaster
               source
      return (newSource,switchOff)

-- -----------------------------------------------------------------
-- mirrorSimpleSource
-- -----------------------------------------------------------------

-- | Replace a SimpleSource by another which mirrors it, but only copies
-- from it once, hopefully saving CPU time.
-- The IO action stops the mirroring.
mirrorSimpleSource :: SimpleSource a -> IO (SimpleSource a,IO ())
mirrorSimpleSource (simpleSource :: SimpleSource a) =
   do
      (sourceMVar :: MVar.MVar (Maybe (SimpleSource a)))  
         <- MVar.newMVar Nothing
      sinkId <- newSinkID

      let
         getSource :: IO (SimpleSource a)
         getSource = MVar.modifyMVar sourceMVar
            (\ sourceOpt -> case sourceOpt of
               Just source -> return (sourceOpt,source)
               Nothing ->
                  do
                     parallelX <- newParallelExec
                     broadcaster <- newSimpleBroadcaster 
                        (error "mirrorSimpleSource: 1")
                     initialised <- MVar.newEmptyMVar 

                     let
                        writeX a =
                           do
                              broadcast broadcaster a
                              MVar.putMVar initialised ()
                        writeD a =
                           do
                              broadcast broadcaster a

                     addNewSourceActions (toSource simpleSource) writeX writeD 
                        sinkId parallelX
                     MVar.takeMVar initialised
                     let 
                        source = toSimpleSource broadcaster
                     return (Just source,source)
               )

      source <- getSource

      return (source,invalidate sinkId)