{- Little functions connected with IO -}
module IOExtras(
   catchEOF, -- :: IO a -> IO (Maybe a)
   -- If successful return result.
   -- if unsuccessful because of EOF return Nothing
   -- otherwise pass on error
   hGetLineR -- :: Read a => Handle -> IO a
   -- hGetLine and then read. 
   ) where

import IO

import Exception

catchEOF :: IO a -> IO (Maybe a)
catchEOF action =
   do
      result <- tryIO
         (\ excep -> 
            case excep of
               IOException ioErr ->
                  if isEOFError ioErr 
                     then
                        Just ()
                     else
                        Nothing
               _ -> Nothing
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
