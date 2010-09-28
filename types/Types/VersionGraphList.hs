-- | Module which maintains the list of external VersionGraphs.
module Types.VersionGraphList(
   addVersionGraph,
      -- :: Maybe HostPort -> IO ()
      -- attempt to open a new version graph by connecting to a server.
   getCurrentVersionGraphs,
      -- :: IO [(Maybe HostPort,Repository)]
      --
      -- Nothing means "the internal server".

   showHostPortOpt, -- :: Maybe HostPort -> String
   ) where

import Maybe
import Monad

import System.IO.Unsafe
import Control.Concurrent
import Control.Exception

import Util.Registry
import Util.ExtendedPrelude(ourExcepToMess)
import Util.Messages

import Events.Events
import Events.Destructible

import Graphs.GraphDisp as Disp
import Graphs.GraphConfigure

import Server.HostsPorts
import Server.CallServer (tryConnect)

import SimpleDB.VersionState

import Types.Initialisation
import Types.VersionGraph

-- -------------------------------------------------------------------------
-- The current version graphs
-- -------------------------------------------------------------------------

currentVersionGraphs :: LockedRegistry (Maybe HostPort) VersionGraph
   -- Here "Nothing" denotes the internal server.
currentVersionGraphs = unsafePerformIO newRegistry
{-# NOINLINE currentVersionGraphs #-}

-- -------------------------------------------------------------------------
-- The functions
-- -------------------------------------------------------------------------

addVersionGraph ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (Disp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> Maybe HostPort -> IO ()
addVersionGraph displaySort hostPortOpt =
   transformValue currentVersionGraphs hostPortOpt
      (\ versionGraphOpt -> case versionGraphOpt of
         Just _ ->
            do
               errorMess ("You are already connected to "
                  ++ showHostPortOpt hostPortOpt)
               return (versionGraphOpt,())
         Nothing ->
            case hostPortOpt of
               Nothing ->
                  do
                     versionState <- mkVersionState True
                     repository
                        <- openRepositoryInternal versionState
                     versionGraph <- newVersionGraphInternal
                        displaySort repository versionState
                     forkIO (
                        do
                           sync (destroyed versionGraph)
                           deleteFromRegistry currentVersionGraphs
                              hostPortOpt
                        )
                     return (Just versionGraph,())
               Just hostPort ->
                  do

                     repositoryOrCancelOrError <- Control.Exception.try (
                        let
                           ?server = hostPort
                        in
                           tryConnect openRepository
                        )
                     case repositoryOrCancelOrError of
                        Left excep ->
                           do
                              let
                                 excepStr = fromMaybe (show excep)
                                    (ourExcepToMess excep)
                              errorMess (
                                 "Failed to connect to " ++ show hostPort ++
                                 " with error " ++ excepStr
                                 )
                              return (versionGraphOpt,())
                        Right (Left mess) ->
                           do
                              messageMess mess
                              return (versionGraphOpt,())
                        Right (Right repository) ->
                           do
                              versionGraph <-
                                 let
                                    ?server = hostPort
                                 in
                                    newVersionGraph displaySort repository
                              forkIO (
                                 do
                                    sync (destroyed versionGraph)
                                    deleteFromRegistry currentVersionGraphs
                                       hostPortOpt
                                 )
                              return (Just versionGraph,())
         )

getCurrentVersionGraphs :: IO [(Maybe HostPort,VersionGraph)]
getCurrentVersionGraphs =
   do
      hostPorts <- listKeys currentVersionGraphs
      versionGraphOpts <- mapM
         -- allow for the possibility that someone else simultaneously
         -- deletes a versionGraph before we get around to looking at it.
         (\ hostPortOpt ->
             do
                versionGraphOpt <- getValueOpt currentVersionGraphs hostPortOpt
                return (fmap
                   (\ versionGraph -> (hostPortOpt,versionGraph))
                      versionGraphOpt
                   )
             )
         hostPorts
      return (catMaybes versionGraphOpts)

showHostPortOpt :: Maybe HostPort -> String
showHostPortOpt Nothing = "the internal server"
showHostPortOpt (Just hostPort) = show hostPort
