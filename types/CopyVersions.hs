-- | This module is called from VersionGraph when the user asks to
-- copy versions from one repository to another. 
module CopyVersions (
   copyVersions, -- :: VersionGraph -> IO ()
   ) where

import Maybe
import List

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
import VersionInfoFilter
import VersionDB

import VersionGraphClient
import VersionGraph
import VersionGraphList
import CopyVersion
import CopyVersionInfos



data HowToSelect =
      All
   |  ByFilter
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
                  [("All",All),("By Filter",ByFilter),("Selected",Selected),
                     ("Cancel",Cancel)]
                  (Just 0)
                  [text "Which new versions should be copied",
                     photo questionImg]
                  [text "Method of Selection"]

            let
               versionGraphClientFrom = toVersionGraphClient versionGraphFrom

            (versions :: [ObjectVersion]) <- case select of
               Cancel -> return []
               All ->
                  do
                     versionInfos <- VersionGraphClient.getVersionInfos 
                        versionGraphClientFrom
                     return (map (version . user . toVersionInfo) versionInfos)
               ByFilter ->
                  do
                     filterOpt <- readVersionInfoFilter
                     case filterOpt of
                        Nothing -> return []
                        Just filter ->
                           do
                              versionInfos0 
                                 <- VersionGraphClient.getVersionInfos 
                                    versionGraphClientFrom
                              let
                                 versionInfos1 
                                    = map toVersionInfo versionInfos0

                                 versionInfos2 = List.filter 
                                    (filterVersionInfo filter) 
                                    versionInfos1
                              return (map (version . user) versionInfos2)
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

               versionGraphClientTo = toVersionGraphClient versionGraphTo

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
               graphBackArg :: Bool 
                  -> VersionGraphNode -> VersionInfo1 
                  -> (VersionInfoKey,Int)
               graphBackArg whichGraph versionGraphNode versionInfo1 =
                  let
                     versionInfo = toVersionInfo versionInfo1
                     extraKey 
                        = if isPresent versionInfo
                           then
                              0
                           else
                              if whichGraph then 2 else 1
                  in
                     (mapVersionInfo versionInfo,extraKey)

            graphBackFrom <- getInputGraphBack versionGraphClientFrom
               (graphBackArg False)

            graphBackTo <- getInputGraphBack versionGraphClientTo
               (graphBackArg True)

            let
               versionGraphNodes :: [VersionGraphNode]
               versionGraphNodes = map CheckedInNode versions

               commonParentsAsNodes 
                  :: [(VersionGraphNode,
                     [(VersionGraphNode,Maybe VersionGraphNode)])]
               commonParentsAsNodes = findCommonParents 
                  graphBackFrom graphBackTo versionGraphNodes

               commonParents :: [(ObjectVersion,
                  [(ObjectVersion,Maybe ObjectVersion)])]
               commonParents = map
                  (\ (vgn1,parentData) ->
                     (toVersion vgn1,
                        map
                           (\ (vgn2,vgn3Opt) 
                              -> (toVersion vgn2,fmap toVersion vgn3Opt)
                              )
                           parentData
                        )
                     )
                  commonParentsAsNodes

               toVersion :: VersionGraphNode -> ObjectVersion
               toVersion vgn = case vgn of
                  CheckedInNode ov -> ov
                  _ -> error ("CopyVersions.toVersion: unexpected view found "
                     ++ "in version graph")

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