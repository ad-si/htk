{- This module contains operations on the list of MMiSS files (so, excluding
   MMiSS objects such as elements and so on) to be exported to some directory.
   -}
module MMiSSExportFiles(
   ExportFiles,
   exportFiles, -- :: View -> FilePath -> ExportFiles -> IO () 

   ) where

import Monad

import Data.FiniteMap

import FileNames
import Computation
import ExtendedPrelude
import Messages

import View(View)
import Link

import MMiSSPackageFolder
import MMiSSFileType
import MMiSSVariant


type ExportFiles = [(MMiSSPackageFolder,String,MMiSSVariantSearch)]

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
      (exportFiles2 :: [((String,String),(Link MMiSSFile,MMiSSVariantSearch))],
            notFound1 :: [String])
         <- foldM
            (\ (found0,notFound0) (packageFolder,str,variantSearch) ->
               do
                  files0 <- findMMiSSFilesInRepository packageFolder str
                  return (case files0 of
                     [] -> (found0,str : notFound0)
                     _ ->
                        let
                           files1 :: [(
                              (String,String),
                              (Link MMiSSFile,MMiSSVariantSearch)
                              )]
                           files1 =
                              map
                                 (\ (link,name,ext) -> 
                                    ((name,ext),(link,variantSearch))
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
               ++ (concat (map (\ file -> "\n   " ++ file) notFound1))
               )

      -- Check for duplicates and construct map
      let
         (exportFiles3 :: FiniteMap (String,String) 
                 (Link MMiSSFile,MMiSSVariantSearch),
               duplicates :: [(String,String)])
            = foldl
               (\ (exportFiles0,duplicates0) (nameExt,linkVariant) ->
                  case lookupFM exportFiles0 nameExt of
                     Just linkVariant1 | linkVariant1 /= linkVariant
                        -> (exportFiles0,nameExt : duplicates0)
                     _ -> 
                        (addToFM exportFiles0 nameExt linkVariant,duplicates0)
                  ) 
               (emptyFM,[])
               exportFiles2

      case duplicates of
         [] -> done
         _ -> errorMess 
            ("Multiple variants of the same file cannot be printed; sorry:"
               ++ (concat (map (\ (name,ext) -> "\n   "
                  ++ unsplitExtension name ext) duplicates))
               )

      -- Now do the export
      let
         dir1 = trimDir dir0
      unitWEs <- mapM
         (\ ((name,ext),(link,variantSearch)) ->
            exportMMiSSFile view dir1 link variantSearch
            )
         (fmToList exportFiles3)

      case fromWithError (listWithError unitWEs) of
         Left mess -> errorMess mess
         Right _ -> done
 