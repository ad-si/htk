{- This module defines the basic abstract folder structure which will be 
   defined in the types/ stuff and passed back to the functions in this
   module for when they process import commands. -}

module FolderStructure(
   FolderStructure(..),
   lookupSearchName,
   lookupFullName,
   getName,
   ) where

import Data.FiniteMap

import Sources
import VariableMap

import EntityNames

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
   parent :: node -> IO (SimpleSource (Maybe (node,EntityName)))
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
            parentSource <- parent folderStructure node0
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

getName :: FolderStructure node -> node -> IO EntityFullName
-- mostly used for error messagse
getName (FolderStructure {parent = parent1}) (node :: node) =
   do
      names <- getName1 [] node
      return (EntityFullName (reverse names))
   where
      getName1 :: [EntityName] -> node -> IO [EntityName]
      getName1 names1 node0 =
         do
            parentSource <- parent1 node0
            parentOpt <- readContents parentSource
            case parentOpt of
               Nothing -> return names1
               Just (node1,name) -> getName1 (name : names1) node1