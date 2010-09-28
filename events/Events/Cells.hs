-- | A Cell is a container for a value.  It is created with the value in it.
-- The only change we can make is to remove the value, and we cannot put
-- it back again.
module Events.Cells(
   Cell, -- The Cell type
   newCell, -- :: a -> IO (Cell a)
   emptyCell, -- :: Cell a -> IO ()
      -- emptying an already empty cell does nothing.
   inspectCell, -- :: Cell a -> IO (Maybe a)
      -- returns the value, or Nothing if the Cell has been cleared.
   ) where

import Data.IORef

newtype Cell a = Cell (IORef (Maybe a))

newCell :: a -> IO (Cell a)
newCell val =
   do
      ioRef <- newIORef (Just val)
      return (Cell ioRef)

emptyCell :: Cell a -> IO ()
emptyCell (Cell ioRef) = writeIORef ioRef Nothing

inspectCell :: Cell a -> IO (Maybe a)
inspectCell (Cell ioRef) = readIORef ioRef

{-# INLINE newCell #-}
{-# INLINE emptyCell #-}
{-# INLINE inspectCell #-}
