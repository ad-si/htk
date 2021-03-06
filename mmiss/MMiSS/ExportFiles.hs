-- | This module contains operations on the list of MMiSS files (so, excluding
-- MMiSS objects such as elements and so on) to be exported to some directory.
module MMiSS.ExportFiles(
   ExportFiles,
   exportFiles, -- :: View -> FilePath -> ExportFiles -> IO ()

   ) where

import Control.Monad

import qualified Data.Map as Map

import Util.FileNames
import Util.Computation
import Util.ExtendedPrelude
import Util.Messages
import Util.AtomString(toString)

import Imports.EntityNames

import Types.View(View)
import Types.Link

import MMiSS.PackageFolder
import MMiSS.FileType
import MMiSS.Variant


type ExportFiles = [(MMiSSPackageFolder,EntityFullName,MMiSSVariantSearch)]

-- | Export the given ExportFiles to the directory by the given filePath.
-- NB.  We do not abort this operation, instead complaining from time to
-- time via errorMess
exportFiles :: View -> FilePath -> ExportFiles -> IO ()
exportFiles view dir0 exportFiles0 =
   do
      -- Remove duplicate elements
      let
         exportFiles1 :: ExportFiles
         exportFiles1 = uniqOrd exportFiles0

      -- Get the list of all matching files to export, and a
      (exportFiles2 ::
           [((EntityFullName,String),
              (Link MMiSSFile,MMiSSVariantSearch))],
         notFound1 :: [EntityFullName])
         <- foldM
            (\ (found0,notFound0) (packageFolder,name0,variantSearch) ->
               do
                  files0 <- findMMiSSFilesInRepository packageFolder name0
                  return (case files0 of
                     [] -> (found0,name0 : notFound0)
                     _ ->
                        let
                           files1 :: [(
                              (EntityFullName,String),
                              (Link MMiSSFile,MMiSSVariantSearch)
                              )]
                           files1 =
                              map
                                 (\ (link,name1,ext) ->
                                    ((name1,ext),(link,variantSearch))
                                    )
                                 files0
                        in
                            (files1 ++ found0,notFound0)
                     )
               )
            ([],[])
            exportFiles1

      case notFound1 of
         [] -> done
         _ -> errorMess
            ("Unable to find files in repository: "
               ++ (concat (map (\ file -> "\n   " ++ toString file) notFound1))
               )

      -- Check for duplicates and construct map
      let
         (exportFiles3 :: Map.Map (EntityFullName,String)
                 (Link MMiSSFile,MMiSSVariantSearch),
               duplicates :: [(EntityFullName,String)])
            = foldl
               (\ (exportFiles0,duplicates0) (nameExt,linkVariant) ->
                  case Map.lookup nameExt exportFiles0 of
                     Just linkVariant1 | linkVariant1 /= linkVariant
                        -> (exportFiles0,nameExt : duplicates0)
                     _ ->
                        (Map.insert nameExt linkVariant exportFiles0,duplicates0)
                  )
               (Map.empty,[])
               exportFiles2

      case duplicates of
         [] -> done
         _ -> errorMess
            ("Multiple variants of the same file cannot be printed; sorry:"
               ++ (concat (map (\ (name,ext) -> "\n   "
                  ++ unsplitExtension (show name) ext) duplicates))
               )

      -- Now do the export
      let
         dir1 = trimDir dir0
      unitWEs <- mapM
         (\ ((name,ext),(link,variantSearch)) ->
            exportMMiSSFile view dir1 link variantSearch
            )
         (Map.toList exportFiles3)

      case fromWithError (listWithError unitWEs) of
         Left mess -> errorMess mess
         Right _ -> done

