{- This module is called from VersionGraph when the user asks to
   copy versions from one repository to another. -}
module CopyVersions (
   copyVersions, -- :: VersionGraph -> IO ()
   ) where

import Maybe

import Computation
import ExtendedPrelude
import Registry

import HTk(text,photo)
import SimpleForm
import DialogWin

import Graph
import SimpleGraph
import FindCommonParents


import HostsPorts(HostPort)

import VersionInfo

import VersionGraphClient
import VersionGraph
import VersionGraphList
import CopyVersion



data HowToSelect =
      All
   |  AllMarked
   |  Selected
   |  Cancel

copyVersions :: VersionGraph -> IO ()
copyVersions versionGraphFrom =
   do
      addFallOut (\ break ->
         do
            -- Select what repository to copy to.
            (versionGraphs :: [(Maybe HostPort,VersionGraph)]) 
               <- getCurrentVersionGraphs

            let
               openMenu :: Form VersionGraph
               openMenu = newFormOptionMenu2
                  (map
                     (\ (hostPortOpt,versionGraph) ->
                           (showHostPortOpt hostPortOpt,versionGraph)
                        )
                     versionGraphs
                     )

            versionGraphToOpt <- doForm "Select Repository to Copy To"
               openMenu

            versionGraphTo <- case versionGraphToOpt of
               Nothing -> break "1"
               Just versionGraphTo -> return versionGraphTo

            -- Get the versions to copy.
            select <-
               createDialogWin 
                  [("All",All),("Marked",AllMarked),("Selected",Selected),
                     ("Cancel",Cancel)]
                  (Just 0)
                  [text "Which new versions should be copied",
                     photo questionImg]
                  [text "Method of Selection"]

            (versions :: [ObjectVersion]) <- case select of
               Cancel -> return []
               All ->
                  do
                     let
                        graph = toVersionGraphGraph versionGraphFrom
                     nodes <- getNodes graph
                     let
                        versions = mapMaybe nodeToVersion nodes
                     return versions
               AllMarked ->
                  do
                      let
                         graph = toVersionGraphGraph versionGraphFrom
                      (nodes1 :: [Node]) <- getNodes graph
                      let
                         versionNodes :: [Node]
                         versionNodes = filter
                            (\ node -> isJust (nodeToVersion node))
                            nodes1
                      (versionInfos1 :: [VersionInfo]) <- mapM
                         (\ node -> getNodeLabel graph node)
                         versionNodes

                      let
                         versions = mapMaybe
                            (\ versionInfo 
                               -> if private (user versionInfo)
                                  then
                                     Nothing
                                  else 
                                     Just (version (user versionInfo))
                               )   
                            versionInfos1   
                      return versions
               Selected ->
                  do
                     versionsOpt <- selectCheckedInVersions versionGraphFrom
                        "Versions to copy"
                     return (fromMaybe [] versionsOpt)

            case versions of 
               [] -> 
                  do
                     createMessageWin "Cancelling copy operation" []
                     break "2"
               _ -> done

            -- now ready to go. 
            let
               graphFrom = toVersionGraphGraph versionGraphFrom
               graphTo = toVersionGraphGraph versionGraphTo

               -- Construct GraphBack for purposes of findCommonParents.

               mkGraphBack :: VersionTypes SimpleGraph ->
                  GraphBack ObjectVersion VersionInfoKey
               mkGraphBack graph = GraphBack {
                  getAllNodes = 
                     (do
                        nodes <- getNodes graph
                        return (mapMaybe nodeToVersion nodes)
                     ),
                  getKey = (\ version ->
                     do
                        let
                           node = versionToNode version
                        versionInfo <- getNodeLabel graph node
                        return (Just (mapVersionInfo versionInfo))
                     ),
                  getParents = (\ version ->
                     do
                        let
                           node = versionToNode version
                        versionInfo <- getNodeLabel graph node
                        return (Just (parents (user versionInfo)))
                     )
                  }

            (commonParents 
               :: [(ObjectVersion,[(ObjectVersion,Maybe ObjectVersion)])])
               <- findCommonParents (mkGraphBack graphFrom) 
                  (mkGraphBack graphTo) versions

               -- List of versions to be copied, with their new parent
               -- versions either in the from repository (Left) or
               -- to repository (Right).  They are given in order, so
               -- we don't copy a version before we've copied its parent.

            (oldToNew :: Registry ObjectVersion ObjectVersion) <- newRegistry
               -- contains map from versions in the old repository to
               -- versions in the new one.
            let
               getParent :: (ObjectVersion,Maybe ObjectVersion) 
                  -> IO (FromTo ObjectVersion)
               getParent (fromVersion,toVersionOpt) =
                  do
                     toVersion <- case toVersionOpt of
                        Just toVersion -> return toVersion
                        Nothing ->
                           do
                              Just toVersion <- getValueOpt oldToNew fromVersion
                              return toVersion
                     let
                        fromTo = FromTo {from = fromVersion,to = toVersion}

                     return fromTo

            -- Now copy.
            let
               repositoryFrom = toVersionGraphRepository versionGraphFrom
               repositoryTo = toVersionGraphRepository versionGraphTo

               repositories = FromTo {from = repositoryFrom,to = repositoryTo}

            mapM_ 
               (\ (versionFrom,parentDatas) ->
                  do
                     parents <- mapM getParent parentDatas
                     versionTo <- copyVersion repositories parents versionFrom
                     setValue oldToNew versionFrom versionTo
                   )
               commonParents
 
         )
      done