{- Module which maintains the list of VersionGraphs. -}
module VersionGraphList(
   addVersionGraph,
      -- :: HostPort -> IO ()
      -- attempt to open a new version graph by connecting to a server.
   getCurrentVersionGraphs, 
      -- :: IO [(HostPort,Repository)]
   ) where

import Maybe
import Monad

import System.IO.Unsafe
import Data.FiniteMap
import Control.Concurrent
import Control.Exception

import Registry
import Computation(done)

import Events
import Destructible

import DialogWin

import GraphDisp
import GraphConfigure

import HostsPorts
import CallServer (tryConnect)

import VersionDB
import Initialisation
import VersionGraph

-- -------------------------------------------------------------------------
-- The current version graphs
-- -------------------------------------------------------------------------

currentVersionGraphs :: LockedRegistry HostPort VersionGraph
currentVersionGraphs = unsafePerformIO newRegistry
{-# NOINLINE currentVersionGraphs #-}

-- -------------------------------------------------------------------------
-- The functions
-- -------------------------------------------------------------------------

addVersionGraph ::
   (GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms)
   => (GraphDisp.Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms)
   -> HostPort -> IO ()
addVersionGraph displaySort hostPort =
   transformValue currentVersionGraphs hostPort
      (\ versionGraphOpt -> case versionGraphOpt of
         Just _ ->
            do
               createErrorWin ("You are already connected to "
                  ++ show hostPort) []
               return (versionGraphOpt,())
         Nothing ->
            do
               repositoryOrCancelOrError <- Control.Exception.try (
                  let
                     ?server = hostPort
                  in
                     tryConnect (Initialisation.openRepository)
                  )
               case repositoryOrCancelOrError of
                  Left excep ->
                     do
                        createErrorWin (
                           "Failed to connect to " ++ show hostPort ++
                           " with error " ++ show excep
                           ) []
                        return (versionGraphOpt,())
                  Right (Left mess) ->
                     do
                        createMessageWin mess []
                        return (versionGraphOpt,())
                  Right (Right repository) -> 
                     do
                        versionGraph <- newVersionGraph displaySort repository
                        forkIO (
                           do
                              sync (destroyed versionGraph)
                              deleteFromRegistry currentVersionGraphs hostPort
                           )
                        return (Just versionGraph,())
         )

getCurrentVersionGraphs :: IO [(HostPort,VersionGraph)]
getCurrentVersionGraphs =
   do
      hostPorts <- listKeys currentVersionGraphs
      repositoryOpts <- mapM
         -- allow for the possibility that someone else simultaneously
         -- deletes a repository before we get around to looking at it.
         (\ hostPort -> 
             do
                repositoryOpt <- getValueOpt currentVersionGraphs hostPort
                return (fmap 
                   (\ repository -> (hostPort,repository)) repositoryOpt
                   )
             )
         hostPorts
      return (catMaybes repositoryOpts)
      