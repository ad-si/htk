{- UniTypes maintains the database of UniForM types. 
   Types correspond exactly to Unix file extensions.  
   A file name is mapped uniquely to a pair (name + extension) as
   follows:  the period before the first period is the name;
   the remainder (excluding the period) is the extension.  Hence
   names may not include periods, but extensions may.  If the
   file name contains no periods, the "name" is the whole file name,
   and the extension is empty.
   -}
module UniTypes(
   UniTypeData(..), -- input data required to register a type
   UniTypeDataBase, -- Type information for a particular project
   UniType, -- represents a type
   newUniTypeDataBase, -- :: IO UniTypeDataBase
   registerType, -- :: UniTypeDataBase -> UniTypeData -> IO UniType
   lookupByExtension, -- :: UniTypeDataBase -> String -> IO UniType
   -- lookupByExtension raises an error if it doesn't recognise the
   -- type.
   getExtension, -- :: UniType -> String

   makeFileName, -- :: String -> UniType -> String
   -- makeFileName makes a file name from the base name and type
   unmakeFileName -- :: UniTypeDataBase -> String -> IO (String,UniType)
   -- unmakeFileName reverses makeFileName
   ) where

import Concurrent
import FiniteMap
import Debug(debug)

type Name = String -- text displayed on icons etcetera
type Extension = String
-- Unix file extension.  In the special case where the extension
-- is "", there is no preceding dot.  This type corresponds to
-- folders.
-- The extension must be unique, and this is checked

data UniTypeData =
   UniTypeData {
      name :: Name,  
      extension :: Extension 
      }

newtype UniTypeDataBase = UniTypeDataBase (MVar (FiniteMap Extension UniType))

data UniType = UniType {typeName::Name,typeExtension::Extension} 
   deriving (Eq,Ord,Show)

newUniTypeDataBase :: IO UniTypeDataBase
newUniTypeDataBase = 
   do
      mVar <- newMVar emptyFM
      return (UniTypeDataBase mVar)

registerType :: UniTypeDataBase -> UniTypeData -> IO UniType
registerType (UniTypeDataBase mVar) 
      (UniTypeData{name=name,extension=extension}) =
   do
      map <- takeMVar mVar
      let
         (newMap,endAction) =
            case lookupFM map extension of
               Nothing ->
                  let
                     uniType = UniType{typeName=name,typeExtension=extension}
                  in
                     (addToFM map extension uniType,return uniType)
               Just _ -> 
                  let
                     err = 
                        userError("UniTypes: "++extension++
                           " multiply defined")
                  in
                     (map,ioError err)         

      putMVar mVar newMap
      endAction

lookupByExtension :: UniTypeDataBase -> Extension -> IO UniType
lookupByExtension(UniTypeDataBase mVar) extension =
   do
      map <- readMVar mVar
      case lookupFM map extension of
         Just uniType -> return uniType
         Nothing -> ioError(userError(
            "lookupByExtension: unrecognised type "++extension
            ))

getExtension :: UniType -> String
getExtension = typeExtension

splitFileName :: String -> (Name,Extension)
splitFileName "" = ("","")
splitFileName ('.':rest) = ("",rest)
splitFileName (c:rest) = 
   let
      (name',extension) = splitFileName rest
   in
      (c:name',extension)

unsplitFileName :: Name -> Extension -> String
unsplitFileName name "" = name
unsplitFileName name extension = name ++ ('.':extension)

makeFileName :: Name -> UniType -> String
makeFileName name (UniType {typeExtension=extension}) = 
   unsplitFileName name extension

unmakeFileName :: UniTypeDataBase -> String -> IO (String,UniType)
unmakeFileName db fileName =
   do
      let
         (base,extension) = splitFileName fileName
      uniType <- lookupByExtension db extension
      return (base,uniType)  
