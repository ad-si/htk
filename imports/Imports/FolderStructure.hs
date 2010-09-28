-- | This module defines the basic abstract folder structure which will be
-- defined in the types stuff and passed back to the functions in this
-- module for when they process import commands.

module Imports.FolderStructure(
   FolderStructure(..),
   lookupSearchName,
   lookupFullName,
   getName,
   ) where

import Util.DeprecatedFiniteMap

import Util.Sources

import Imports.EntityNames

-- -----------------------------------------------------------------------
-- The datatype
-- -----------------------------------------------------------------------

-- The node will also need to instance Ord,Eq, though that's not required
-- here.
data FolderStructure node = FolderStructure {
   root :: node, -- the top node
   getContentsSource :: node -> IO (SimpleSource (FiniteMap EntityName node)),
   getImportCommands :: node -> IO (Maybe (SimpleSource ImportCommands)),
      -- when Nothing means that this node is of inappropriate type for import.
   getParent :: node -> IO (SimpleSource (Maybe (node,EntityName)))
      -- returns Nothing for the parent, but also if for some reason the
      -- node has been detached.
   }



-- -----------------------------------------------------------------------
-- Various functions
-- -----------------------------------------------------------------------

lookupSearchName :: FolderStructure node -> node -> EntitySearchName
   -> IO (SimpleSource (Maybe node))
lookupSearchName folderStructure (node :: node) entitySearchName =
      lookupSearchName1 node entitySearchName
   where
      lookupSearchName1 :: node -> EntitySearchName
         -> IO (SimpleSource (Maybe node))
      lookupSearchName1 node (FromHere fullName) =
         lookupFullName folderStructure node fullName
      lookupSearchName1 node (FromCurrent fullName) =
         lookupFullName folderStructure node fullName
      lookupSearchName1 node (FromRoot fullName) =
         lookupFullName folderStructure (root folderStructure) fullName
      lookupSearchName1 node0 (FromParent searchName1) =
         do
            parentSource <- getParent folderStructure node0
            return (mapIOSeq
               parentSource
               (\ parentOpt -> case parentOpt of
                  Nothing -> return (staticSimpleSource Nothing)
                  Just (node1,_) -> lookupSearchName1 node1 searchName1
                  )
               )

lookupFullName :: FolderStructure node -> node -> EntityFullName
   -> IO (SimpleSource (Maybe node))
lookupFullName  folderStructure (node :: node) (EntityFullName names) =
      lookupFullName1 node names
   where
      lookupFullName1 :: node -> [EntityName] -> IO (SimpleSource (Maybe node))
      lookupFullName1 node0 [] = return (staticSimpleSource (Just node0))
      lookupFullName1 node0 (name1 : names) =
         do
             (contentsSource :: SimpleSource (FiniteMap EntityName node))
                <- getContentsSource folderStructure node0
             return (mapIOSeq
                contentsSource
                (\ map -> case lookupFM map name1 of
                   Nothing -> return (staticSimpleSource Nothing)
                   Just node1 -> lookupFullName1 node1 names
                   )
                )

getName :: Ord node => FolderStructure node -> node -> IO EntityFullName
-- For detached nodes we generate
-- an (illegal) name of the form #DETACHED.[blah]
-- Current uses (22/9/2004) are (1) error messages; (2) package ids;
-- (3) displaying permissions.
getName (FolderStructure {getParent = getParent1,root = root1}) (node :: node) =
   do
      names <- getName1 [] node
      return (EntityFullName names)
   where
      getName1 :: [EntityName] -> node -> IO [EntityName]
      getName1 names1 node0 =
         do
            parentSource <- getParent1 node0
            parentOpt <- readContents parentSource
            case parentOpt of
               Nothing ->
                  return (
                     if node0 == root1
                        then
                           names1
                        else
                           EntityName "#DETACHED" : names1
                     )
               Just (node1,name) -> getName1 (name : names1) node1
