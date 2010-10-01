-- | Lock is an instance of a typical thing we synchronize with.
-- One instance is 'BSem'.
module Reactor.Lock (
        Lock(..),
        ) where

-- --------------------------------------------------------------------------
-- Class Lock
-- --------------------------------------------------------------------------

class Lock l where
   -- | release a lock
   release :: l -> IO ()
   -- | acquire a lock
   acquire :: l -> IO ()

   -- | acquire a lock and return True, if that can be done at once, otherwise
   -- return False.
   tryAcquire :: l -> IO Bool
