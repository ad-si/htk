-- | This module is called from VersionGraph when the user asks to
-- copy versions from one repository to another. 
module CopyVersions (
   copyVersions, -- :: VersionGraph -> IO ()
   ) where

import Maybe

import Computation
import ExtendedPrelude
import Registry
import Thread(mapMConcurrentExcep)
import Messages

import HTk(text,photo)
import SimpleForm
import DialogWin

import Graph
import SimpleGraph
import FindCommonParents

import HostsPorts(HostPort)

import VersionInfo
import VersionDB

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
                     messageMess "Cancelling copy operation"
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

               repositoryTo = toVersionGraphRepository versionGraphTo

               -- Construct GraphBack for purposes of findCommonParents.

               -- We need to assume all nodes which have isPresent = False
               -- are not equal to any node in the other graph, since they
               -- are useless for copying.
               -- We do this by giving mkGraphBack an extra Bool whichGraph
               -- and giving nodes an extra key which is
               --    0 if isPresent = True
               --    1 if isPresent = False and mkGraphBack = False
               --    2 if isPresent = False and mkGraphBack = True,
               -- so ensuring that two nodes from different graphs will only 
               -- compare equal if they both have isPresent = True 
               mkGraphBack :: Bool -> VersionTypes SimpleGraph ->
                  GraphBack ObjectVersion (VersionInfoKey,Int)
               mkGraphBack whichGraph graph = GraphBack {
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
                        let
                           extraKey = 
                              if isPresent versionInfo
                                 then
                                    0
                                 else
                                    if whichGraph then 2 else 1
                        return (Just (mapVersionInfo versionInfo,extraKey))
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
               <- findCommonParents (mkGraphBack False graphFrom) 
                  (mkGraphBack True graphTo) versions

            -- Now copy the VersionInfos
            (toNewVersionInfo :: ObjectVersion -> IO VersionInfo)
               <- copyVersionInfos versionGraphFromTo versions

            let
               -- Function for getting command for committing one element of 
               -- commonParents
               copyOne :: (ObjectVersion,[(ObjectVersion,Maybe ObjectVersion)])
                  -> IO SimpleDBCommand
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

            -- Copy simultaneously all the versions.  We bunch them up in
            -- a single command. 
            --
            -- NB - this is good, because when we send a version to the 
            -- destination repository, we don't want to send a version before
            -- its parents.  
            --
            -- When computing the commands to send, we use 
            -- mapMConcurrentExcep.  This will hopefully have the effect
            -- of bunching up requests to the source repository into single
            -- MultiCommands, if it is a long way away.
            commands <- mapMConcurrentExcep copyOne commonParents

            (MultiResponse responses) <- queryRepository repositoryTo 
               (MultiCommand commands)
            mapM_ 
               (\ response -> case response of
                  IsOK -> done
                  IsObjectVersion _ -> done 
                     -- means this version already committed.  Can happen
                     -- with a race condition.
                  IsError mess -> error ("Server error: " ++ mess)
                  _ -> error ("Mysterious server error")
                  ) 
               responses
         )
      done