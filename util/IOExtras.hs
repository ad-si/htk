{- Little functions connected with IO -}
module IOExtras(
   catchEOF, -- :: IO a -> IO (Maybe a)
   -- If successful return result.
   -- if unsuccessful because of EOF return Nothing
   -- otherwise pass on error
   hGetLineR, -- :: Read a => Handle -> IO a
   -- hGetLine and then read. 
   readFileInstant, -- :: FilePath -> IO String
   -- Like readFile, but closes the file before returning.
   ) where

import IO

import qualified IOExts
import qualified CString
import Exception
import Storable

catchEOF :: IO a -> IO (Maybe a)
catchEOF action =
   do
      result <- tryJust
         (\ excep -> 
            case ioErrors excep of
               Nothing -> Nothing
               Just ioError -> 
                  if isEOFError ioError
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

{-
readFileInstant :: FilePath -> IO String
readFileInstant file =
   do
      (addr,len) <- IOExts.slurpFile file
      str <- CString.unpackCStringLenIO addr len
      IOExts.freeHaskellFunctionPtr addr 
      -- unpackCStringLenIO is, according to Simon Marlow,
      -- supposed to unpack the string at once.           
      return str

   Another way
-}

readFileInstant :: String -> IO String
readFileInstant file =
   do
      handle <- openFile file ReadMode
      contents <- hGetContents handle
      seq (last contents) (hClose handle)
      -- The seq hopefully forces everything to be read.
      -- PS I've tried removing seq on the grounds that
      -- hClose should make it unnecessary, but it breaks
      -- the Versions test on Linux.
      return contents

      
