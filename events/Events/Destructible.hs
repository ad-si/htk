-- | Things which instance Destroyable and Destructible can be destroyed.
module Events.Destructible (
   Destroyable(..),
   Destructible(..),

   doOnce,
      -- :: IO () -> IO (IO ())
      -- doOnce can be used to produce an action which is identical
      -- to its argument, except that if it's already been called, it
      -- does nothing.
   ) where

import Events.Events

import Events.Toggle

class Destroyable o where
   -- | Destroys an object
   destroy :: o -> IO ()

class Destroyable o => Destructible o where
   -- | An event which occurs when the object is destroyed.
   destroyed       :: o -> Event ()

-- | doOnce can be used to produce an action which is identical
-- to its argument, except that if it\'s already been called, it
-- does nothing.
doOnce :: IO () -> IO (IO ())
doOnce action =
   do
      sToggle <- newSimpleToggle
      return (ifSimpleToggle sToggle action)
