{- The InsertionPoint denotes where to insert or overwrite an object. -}
module MMiSSInsertionPoint(
   InsertionPoint,
   getInsertionPoint,
      -- :: View -> EntityFullName -> IO (WithError InsertionPoint)
      -- Get the insertion point for an object with the given name.
   ) where

import Computation
import AtomString

import EntityNames

import LinkManager
import Link
import View
import Folders

import MMiSSSplitLink

type InsertionPoint = Either LinkedObject (Link Folder)
   -- Left LinkedObject means overwrite this object.
   -- Right (Link Folder) means insert in this folder.

getInsertionPoint :: View -> EntityFullName 
   -> IO (WithError (Either LinkedObject (Link Folder)))
getInsertionPoint view fullName =
   do
      linkedObjectOpt <- lookupLinkedObjectByFullName view fullName
      case linkedObjectOpt of
         Just linkedObject -> return (hasValue (Left linkedObject)) 
         Nothing -> case entityDir fullName of
            Nothing -> return (hasError 
               "Attempt to write to Root element, and no root element found")
            Just dirName ->
               do
                  folderOpt <- lookupLinkedObjectByFullName view dirName
                  case folderOpt of
                     Nothing -> return (hasError (
                        "Unable to find directory to write to "
                           ++ toString dirName))
                     Just linkedObject -> 
                        case splitLinkedObject linkedObject of
                           FolderC folderLink 
                             -> return (hasValue (Right folderLink))
                           _ -> return (hasError ("Attempt to insert new "
                              ++ "object in node not a folder"
                              ))
