{- Little functions connected with IO -}
module IOExtras(
   catchEOF, -- :: IO a -> IO (Maybe a)
   -- If successful return result.
   -- if unsuccessful because of EOF return Nothing
   -- otherwise pass on error

   catchAlreadyExists, -- :: IO a -> IO (Maybe a)
   -- If successful return results,
   -- If unsuccessful because of an isAlreadyExists error return Nothing
   -- otherwise pass on error.

   hGetLineR, -- :: Read a => Handle -> IO a
   -- hGetLine and then read. 
   ) where

import IO

import Exception
import Storable

catchEOF :: IO a -> IO (Maybe a)
catchEOF action = catchGeneral isEOFError action

catchAlreadyExists :: IO a -> IO (Maybe a)
catchAlreadyExists action = catchGeneral isAlreadyExistsError action

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

hGetLineR :: Read a => Handle -> IO a
hGetLineR handle =
   do
      line <- hGetLine handle
      return (read line)

