{- 
 - Reentrant, protected references: an IORef in an MVar, protected by
 - a reentrant monitor.
 -
 - The operations which change the value (setRef, changeRef, withRef) 
 - are protected by the monitor, which additionally provides a reentrant
 - synchronize method. 
 -}
 

module ReferenceVariables(
  Ref,       -- type Ref a
  newRef,    -- :: a -> IO (Ref a)
  setRef,    -- :: Ref a -> a -> IO ()
  changeRef, -- :: Ref a -> (a -> a) -> IO ()
  withRef,   -- :: Ref a -> (a -> b) -> IO b
--  updRef,
  getRef     -- :: Ref a -> IO a

) where

import Computation
import Synchronized
import Mutex
import IOExts(IORef,newIORef,readIORef,writeIORef)
import Concurrent

data Ref a = Ref Mutex (MVar (IORef a))

newRef :: a -> IO (Ref a)
newRef val = 
  do
    ioref <- newIORef val
    mvar <- newMVar ioref
    mtx <- newMutex
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

{-
updRef (Ref mvar) cmd =
  do
    ioref <- takeMVar mvar
    val <- readIORef ioref
    ans <- try (cmd val)
    case ans of
      Left e ->
        do
          putMVar mvar ioref
          raise e
      Right (val',res) ->
        do
          writeIORef ioref val'
          putMVar mvar ioref
          return res
-}