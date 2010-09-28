module Events.Synchronized (

  Synchronized(..)

) where

class Synchronized a where
   -- | acquire lock on a, and while we\'ve got it do this action.
   synchronize :: a -> IO b -> IO b
