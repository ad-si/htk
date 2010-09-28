-- | Little functions connected with IO
module Util.IOExtras(
   catchEOF, -- :: IO a -> IO (Maybe a)
   -- If successful return result.
   -- if unsuccessful because of EOF return Nothing
   -- otherwise pass on error

   catchAlreadyExists, -- :: IO a -> IO (Maybe a)
   -- If successful return results,
   -- If unsuccessful because of an isAlreadyExists error return Nothing
   -- otherwise pass on error.

   catchDoesNotExist,
      -- :: IO a -> IO (Maybe a)

   catchErrorCalls, -- :: IO a -> IO (Either String a)
   -- Catch all calls to the error function.

   hGetLineR, -- :: Read a => Handle -> IO a
   -- hGetLine and then read.

   simpleModifyIORef,
      -- :: IORef a -> (a -> (a,b)) -> IO b
      -- carry out a pure modification of an IORef.
      -- From ghc5.05 onwards, we should be able to use atomicModifyIORef
      -- for this.

   ) where

import IO

import Data.IORef
import Control.Exception

catchEOF :: IO a -> IO (Maybe a)
catchEOF action = catchGeneral isEOFError action

catchAlreadyExists :: IO a -> IO (Maybe a)
catchAlreadyExists action = catchGeneral isAlreadyExistsError action

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist action = catchGeneral isDoesNotExistError action

catchGeneral :: (IOError -> Bool) -> IO a -> IO (Maybe a)
catchGeneral discriminator action =
   do
      result <- tryJust
         (\ excep ->
            case ioErrors excep of
               Nothing -> Nothing
               Just ioError ->
                  if discriminator ioError
                     then
                        Just ()
                     else
                        Nothing
            )
         action
      case result of
         Left () -> return Nothing
         Right success -> return (Just success)

catchErrorCalls :: IO a -> IO (Either String a)
catchErrorCalls action =  tryJust errorCalls action

hGetLineR :: Read a => Handle -> IO a
hGetLineR handle =
   do
      line <- hGetLine handle
      return (read line)

simpleModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
simpleModifyIORef = atomicModifyIORef

