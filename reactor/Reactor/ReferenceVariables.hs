-- |
-- - Reentrant, protected references: an IORef in an MVar, protected by
-- - a reentrant monitor.
-- -
-- - The operations which change the value (setRef, changeRef, withRef)
-- - are protected by the monitor, which additionally provides a reentrant
-- - synchronize method.


module Reactor.ReferenceVariables(
  Ref,       -- type Ref a
  newRef,    -- :: a -> IO (Ref a)
  setRef,    -- :: Ref a -> a -> IO ()
  changeRef, -- :: Ref a -> (a -> a) -> IO ()
  changeRefM, -- :: Ref a-> (a-> IO a) -> IO ()
  withRef,   -- :: Ref a -> (a -> b) -> IO b
  getRef     -- :: Ref a -> IO a

) where

import Data.IORef
import Control.Concurrent.MVar

import Events.Synchronized
import Reactor.MSem

data Ref a = Ref MSem (MVar (IORef a))

newRef :: a -> IO (Ref a)
newRef val =
  do
    ioref <- newIORef val
    mvar <- newMVar ioref
    mtx <- newMSem
    return (Ref mtx mvar)

setRef :: Ref a -> a -> IO ()
setRef (Ref mtx mvar) val =
  synchronize mtx $
  do ioref <- takeMVar mvar
     writeIORef ioref val
     putMVar mvar ioref

changeRef :: Ref a -> (a -> a) -> IO ()
changeRef (Ref mtx mvar) fn =
  synchronize mtx $
  do ioref <- takeMVar mvar
     val <- readIORef ioref
     writeIORef ioref (fn val)
     putMVar mvar ioref

changeRefM :: Ref a -> (a -> IO a) -> IO ()
changeRefM (Ref mtx mvar) act =
  synchronize mtx $
  do ioref <- takeMVar mvar
     val <- readIORef ioref
     val' <- act val
     writeIORef ioref val'
     putMVar mvar ioref

withRef :: Ref a -> (a -> b) -> IO b
withRef (Ref mtx mvar) fn =
  synchronize mtx $
  do
    ioref <- takeMVar mvar
    val <- readIORef ioref
    putMVar mvar ioref
    return (fn val)

getRef :: Ref a -> IO a
getRef (Ref _ mvar) =
  do
    ioref <- takeMVar mvar
    val <- readIORef ioref
    putMVar mvar ioref
    return val

instance Synchronized (Ref a) where
  synchronize (Ref mtx mvar) = synchronize mtx

