-- | The InsertionPoint denotes where to insert or overwrite an object.
module MMiSS.InsertionPoint(
   InsertionPoint,
   getInsertionPoint,
      -- :: View -> EntityFullName -> IO (WithError InsertionPoint)
      -- Get the insertion point for an object with the given name.


   toInsertionPointName, -- :: InsertionPoint -> Maybe EntityName

   ) where

import Util.Computation
import Util.AtomString

import Imports.EntityNames

import Types.LinkManager
import Types.Link
import Types.View
import Types.Folders

import MMiSS.SplitLink

type InsertionPoint = Either LinkedObject (Link Folder,EntityName)
   -- Left LinkedObject means overwrite this object.
   -- Right (Link Folder) means insert in this folder.

getInsertionPoint :: View -> EntityFullName -> IO (WithError InsertionPoint)
getInsertionPoint view fullName =
   do
      linkedObjectOpt <- lookupLinkedObjectByFullName view fullName
      case linkedObjectOpt of
         Just linkedObject -> return (hasValue (Left linkedObject))
         Nothing -> case entityDirBase fullName of
            Nothing -> return (hasError
               "Attempt to write to Root element, and no root element found")
            Just (dirName,baseName) ->
               do
                  folderOpt <- lookupLinkedObjectByFullName view dirName
                  case folderOpt of
                     Nothing -> return (hasError (
                        "Unable to find directory to write to "
                           ++ toString dirName))
                     Just linkedObject ->
                        case splitLinkedObject linkedObject of
                           FolderC folderLink
                             -> return (hasValue (Right (folderLink,baseName)))
                           _ -> return (hasError ("Attempt to insert new "
                              ++ "object in node not a folder"
                              ))

-- -------------------------------------------------------------------------
-- Getting the name from an insertion point
-- -------------------------------------------------------------------------

toInsertionPointName :: InsertionPoint -> Maybe EntityName
toInsertionPointName (Left _) = Nothing
toInsertionPointName (Right (_,name)) = Just name

