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

   applyUpdate, -- :: Broadcaster x d -> (x -> (x,[d])) -> IO ()

   applyGeneralUpdate, -- :: GeneralBroadcaster x d -> (x -> (x,[d],extra)) -> IO extra
   ) where

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
