{- This module contains the ObjectSource functions.  An ObjectSource
   refers to either something like a String, or the contents of a file. -}
module ObjectSource(
   ObjectSource,
   -- type of data as retrieved from the repository.
   exportString, -- :: ObjectSource -> IO String
   -- exportString extracts the contents of the object as a String
   exportFile, -- :: ObjectSource -> FilePath -> IO ()
   -- exportFile writes the contents of the object as a file with the
   -- supplied name, overwriting whatever was there before.
   importString, -- :: String -> IO ObjectSource
   -- importString makes an object with the given contents.
   importFile, -- :: FilePath -> IO ObjectSource
   -- importFile makes an object from the given file.

   getICSL, -- :: ObjectSource -> IO ICStringLen
   fromICSL, -- :: ICStringLen -> ObjectSource
   ) where


import AtomString
import ICStringLen

import FdRead
import CopyFile

-- --------------------------------------------------------------
-- The ObjectSource type, and functions for it.
-- --------------------------------------------------------------

newtype ObjectSource = ObjectSource (IO ICStringLen)

getICSL :: ObjectSource -> IO ICStringLen
getICSL (ObjectSource act) = act

fromICSL :: ICStringLen -> ObjectSource
fromICSL icsl = ObjectSource (return icsl)

exportString :: ObjectSource -> IO String
exportString (ObjectSource act) =
   do
      icsl <- act
      return (toString icsl)

exportFile :: ObjectSource -> FilePath -> IO ()
exportFile (ObjectSource act) filePath =
   do
      icsl <- act
      withICStringLen icsl 
         (\ len cString -> copyCStringLenToFile (cString,len) filePath)

importString :: String -> IO ObjectSource
importString str = return (fromICSL (fromString str))

importFile :: FilePath -> IO ObjectSource
importFile file =
   do
      icsl <- copyFileToICStringLen file
      return (fromICSL icsl)
