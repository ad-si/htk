{- This module is called from VersionGraph when the user asks to
   copy versions from one repository to another. -}
module CopyVersions (
   copyVersions, -- :: VersionGraph -> IO ()
   ) where

import Maybe

import Computation
import ExtendedPrelude
import Registry
import Thread(mapMConcurrent_)

import HTk(text,photo)
import SimpleForm
import DialogWin

import Graph
import SimpleGraph
import FindCommonParents
import GetAncestors


import HostsPorts(HostPort)

import VersionInfo

import VersionGraphClient
import VersionGraph
import VersionGraphList
import CopyVersion
import CopyVersionInfos



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
               versionGraphFromTo = FromTo {
                  from = versionGraphFrom,
                  to = versionGraphTo
                  } 

               graphFrom = toVersionGraphGraph versionGraphFrom
               graphTo = toVersionGraphGraph versionGraphTo

               -- Construct GraphBack for purposes of findCommonParents.
               -- To make sure we only pick up on nodes whose views have
               -- actually been committed to the repository, we use 
               -- getAncestors so that getParents misses out on the others.

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

                        ancestors <- getAncestors
                           graph
                           (\ versionInfo -> return (isPresent versionInfo))
                           node

                        let
                           versions = map
                              (\ node -> case nodeToVersion node of
                                 Just version -> version
                                 -- Nothing is impossible since no version
                                 -- may have a view as parent.
                                 )
                              ancestors

                        return (Just versions)
                     )
                  }

            (commonParents 
               :: [(ObjectVersion,[(ObjectVersion,Maybe ObjectVersion)])])
               <- findCommonParents (mkGraphBack graphFrom) 
                  (mkGraphBack graphTo) versions

            -- Now copy the VersionInfos
            (toNewVersionInfo :: ObjectVersion -> IO VersionInfo)
               <- copyVersionInfos versionGraphFromTo versions

            let
               -- Do one element of commonParents
               copyOne :: (ObjectVersion,[(ObjectVersion,Maybe ObjectVersion)])
                  -> IO ()
               copyOne (fromVersion,parents) =
                   do
                      parentsFromTo <- mapM
                         (\ (parentFrom,parentToOpt) ->
                            do
                               parentTo <- case parentToOpt of
                                  Just parentTo -> return parentTo
                                  Nothing -> 
                                     do
                                        versionInfo 
                                           <- toNewVersionInfo parentFrom
                                        return (version (user versionInfo))
                               return 
                                  (FromTo {from = parentFrom,to = parentTo})
                            )
                         parents

                      toVersionInfo <- toNewVersionInfo fromVersion
                      copyVersion versionGraphFromTo fromVersion toVersionInfo
                         parentsFromTo toNewVersionInfo

            -- Copy simultaneously all the versions.  This will mean that
            -- if either server is a long way away and we have a lot of
            -- versions to copy, the commands will get bunched together 
            -- using the MultiCommand mechanism.
            mapMConcurrent_ copyOne commonParents
 
         )
      done