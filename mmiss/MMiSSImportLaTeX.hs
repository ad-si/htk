{- This module contains the code for importing an MMiSSObject as LaTeX. -}
module MMiSSImportLaTeX(
   importMMiSSLaTeX,
   ) where

import Maybe
import Monad
import Char (toLower)

import Computation
import ExtendedPrelude
import WBFiles
import FileNames
import Delayer
import Messages

import Events

import FileDialog

import CopyFile

import ViewType
import View
import Link
import LinkManager
import EntityNames

import Text.XML.HaXml.Types 
import Text.XML.HaXml.Parse (xmlParse)

import LaTeXParser

import MMiSSDTDAssumptions
import MMiSSObjectTypeType
import MMiSSObjectType
import MMiSSPreamble
import MMiSSFileType
import MMiSSContent
import MMiSSPreObjects
import MMiSSVariant(MMiSSVariantSpec)
import {-# SOURCE #-} MMiSSPackageFolder
import {-# SOURCE #-} MMiSSWriteObject


---
-- Import a new object from a LaTeX or XML file and attach it to the 
-- subdirectory of a given PackageFolder.
--
-- The preamble should be written to the link given by the first
-- argument.
--
-- The complicated EntityName -> .. argument constructs or retrieves the 
-- parent linked object, given the name of the package.
-- 
-- The Maybe FilePath argument provides the name of the file to be read.
-- If it is Nothing, the user is quizzed.
importMMiSSLaTeX :: Link MMiSSPreamble -> MMiSSObjectType -> View
   -> (EntityName -> IO (WithError MMiSSPackageFolder)) 
   -> Maybe FilePath
   -> IO (Maybe (Link MMiSSObject))
importMMiSSLaTeX preambleLink objectType view getPackageFolder filePathOpt0 =
   do
      result <- addFallOut (\ break ->
         do
            filePathOpt <- case filePathOpt0 of
               Just _ -> return filePathOpt0
               Nothing ->
                  do
                     top <- getTOP 
                     let
                        fullName = unbreakName [top,"mmiss","test","files"]
                     dialogEvent <- fileDialog "Import Sources" fullName
                     sync dialogEvent

            case filePathOpt of
               Nothing -> return Nothing
               Just filePath0 ->
                  do
                     let 
                        filePath = trimDir filePath0

                        dirPath = case splitName filePath of
                           Nothing -> thisDir
                           Just (dirPath,_) -> dirPath

	             inputStringWE <- copyFileToStringCheck filePath
                     inputString 
                        <- coerceWithErrorOrBreakIO break inputStringWE
                     parseResultWE <- 
                        if not (looksLikeXml filePath) 
                           then
                              do 
                                 putStrLn "We have LaTeX."
                                 return (parseMMiSSLatex inputString)
                           else
                              parseXml filePath inputString

                     (element,preambleOpt) 
                        <- coerceWithErrorOrBreakIO break parseResultWE

                     (fullLabelSearch :: EntitySearchName)
                        <- coerceWithErrorOrBreakIO break (getLabel element)

                     (fullLabel :: EntityFullName)
                        <- case toFullName fullLabelSearch of
                           Just fullLabel -> return fullLabel
                           Nothing -> (break 
                              "Element label is not within current directory")

                     let
                        baseLabel = fromMaybe
                           (break "Element label has no name!")
                           (entityBase fullLabel)

                     seq baseLabel done

                     packageFolderWE <- getPackageFolder baseLabel
                     packageFolder 
                        <- coerceWithErrorOrBreakIO break packageFolderWE

                     preamble <- case preambleOpt of
                        Just preamble -> return preamble
                        Nothing -> break "Object has no preamble!"

                     writePreamble preambleLink view preamble

                     resultWE <- writeToMMiSSObject objectType view
                        packageFolder Nothing element True

                     (link,_,preObjects) 
                        <- coerceWithErrorOrBreakIO break resultWE
                     
                     -- Now add all referenced files belonging to this
                     -- package, getting them from preObjects.
                     -- From here on we treat all errors softly,
                     -- not breaking but simply reporting them and moving on.
                     -- This is because the package import itself has
                     -- succeeded.
                     let
                        toLinkedObject' = toMMiSSPackageFolderLinkedObject

                        insertions1 :: [(ObjectLoc,[StructuredContent])]
                        insertions1 = listPreObjects preObjects

                        -- filter all insertions out which aren't into
                        -- this package folder and get the StructuredContent
                        -- for the rest
                        insertions2 :: [StructuredContent]
                        insertions2 = concat (
                           map 
                              (\ (objectLoc,contents) ->
                                 if toLinkedObject' (package objectLoc) 
                                       == toLinkedObject' packageFolder
                                    then
                                       contents
                                    else
                                       []
                                 )
                              insertions1
                              )

                        -- Now get the files for all the objects
                        insertions3 :: [(MMiSSVariantSpec,String)]
                        insertions3 = concat (
                           map
                              (\ content -> 
                                 pairList (variantSpec content) 
                                    (getAllFiles content)
                              )
                              insertions2
                           )

                        -- Remove duplicates
                        insertions4 :: [(MMiSSVariantSpec,String)]
                        insertions4 = uniqOrd insertions3

                     -- Now find all files of appropriate file type within
                     -- the directory, as (name,extension) pairs.
                     -- Also remember which files we did not find, so we can
                     -- complain.
                     (insertions5 :: [(MMiSSVariantSpec,String,String)],
                           notFound :: [String])
                        <- foldM
                           (\ (found0,notFound0) (variantSpec,toFind) ->
                              do
                                 foundPairs <- findMMiSSFilesInDirectory
                                    dirPath toFind
                                 return (case foundPairs of
                                    [] -> (found0,toFind : notFound0)
                                    _ -> 
                                       let
                                          foundList :: [(MMiSSVariantSpec,
                                             String,String)]
                                          foundList = map
                                             (\ (name,ext) 
                                                -> (variantSpec,name,ext))
                                             foundPairs
                                       in
                                          (foundList ++ found0,notFound0)
                                    )
                              )
                           ([],[])
                           insertions4

                     -- Complain about missing files if necessary
                     case notFound of
                        [] -> done
                        _ -> errorMess
                           ("The following referenced files were not found or "
                              ++ "have an unknown file type:"
                              ++ (concat (map (\ nf -> "\n   " ++ nf) 
                                 notFound))
                              )

                     -- Remove duplicates again
                     let
                        insertions6 :: [(MMiSSVariantSpec,String,String)]
                        insertions6 = uniqOrd insertions5

                     -- Now do the insertions.  
                     resultWEs <- mapM
                        (\ (variantSpec,name,ext) ->
                           importMMiSSFile view (toLinkedObject' packageFolder)
                              dirPath name ext variantSpec
                           )
                        insertions6  
                     
                     case fromWithError (listWithError resultWEs) of
                        Right _ -> done
                        Left mess -> errorMess mess
                     
                     return (Just link)
         )
      case result of
         Left str ->
            do
               messageMess str
               return Nothing
         Right (linkOpt@(Just link)) -> 
            do
               messageMess "Import successful!" 
               return linkOpt
         Right Nothing -> return Nothing 
            -- message has already been displayed by fileDialog.


looksLikeXml :: String-> Bool
looksLikeXml str =
  case splitName str of
   Just (dir, fname) ->
     case splitExtension fname of 
       Just (_, ext) -> let lext = map toLower ext in lext == "xml"  
                                                      || lext == "omdoc"
       _             -> False
   _ -> False

parseXml :: String-> String-> IO (WithError (Element, Maybe MMiSSLatexPreamble))
parseXml fname src = 
  do putStrLn "Parsing XML..."
     catch (do let Document _ _ el = xmlParse fname src 
               putStrLn "Successfully parsed XML"
               return (hasValue (el, Just emptyMMiSSLatexPreamble)))
            (\err-> return (hasError (show err)))

