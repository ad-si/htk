{- Here we provide access to the FileSystem of a view -}
module ViewFileSystem(
   ensureObject,
   ) where

import Maybes
import FileSystem

import Folders
import ObjectTypes
import BasicObjects
import Link
import ViewType
import CopyFile

---
-- ensureObject ensures that an up-to-date copy of the
-- object referred to by the given
-- [String] argument exists in the view's FileSystem.
-- The "object" argument is not looked at, but is a type argument specifying
-- the type of the object concerned; if the object does not have that type
-- an error will be raised.
ensureObject :: (ObjectType objectType object,HasFilePath object)
    => View -> [String] -> object -> IO ()
ensureObject view objectPath (_ :: object) =
   do
      wrappedLinkOpt <- lookupFileName view objectPath
      let 
         descript = "Object "++show objectPath
  
         wrappedLink = fromJustError (descript++" not found") wrappedLinkOpt
      let
         (linkOpt :: Maybe (Link object)) = unpackWrappedLink wrappedLink

         link = fromJustError (descript++" has wrong type") linkOpt

         viewUpdate _ target =
            do
               x <- readLink view link
               linkFile (toFilePath x) target

      ensureFile (fileSystem view) viewUpdate objectPath 


instance HasFileSystem View where
   getFileSystemLocation view = getFileSystemLocation (fileSystem view)
