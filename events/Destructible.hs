{- Things which instance Destroyable and Destructible can be destroyed. -}
module Destructible (
   Destroyable(..),
   Destructible(..),

   doOnce, 
      -- :: IO () -> IO (IO ())
      -- doOnce can be used to produce an action which is identical
      -- to its argument, except that if it's already been called, it
      -- does nothing.
   ) where

import Events

import Toggle

class Destroyable o where
   destroy :: o -> IO ()

class Destroyable o => Destructible o where
-- destroy destroys the object; the destroyed event should then
-- occur.  See EWK thesis 7.4.1.
   destroyed       :: o -> Event ()

doOnce :: IO () -> IO (IO ())
doOnce action =
   do
      sToggle <- newSimpleToggle
      return (ifSimpleToggle sToggle action)