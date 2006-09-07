{------------------------------------
MODULE        : UDraw.Util
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2006
VERSION       : 1.0
DESCRIPTION   : Provides utility functions for uDraw (Ontology).
				At the Moment a datastrukture for selectet relations 
				and a function to get the uDraw (Graph) home.
-------------------------------------}

module UDraw.Util (
  Relations,
--emptyRelations :: Relations
  emptyRelations,
  setUpRelations,
  setSelectedRelations,
  getRelationName,
  getRelationNames,
  getRelationIndex,
  getSelectedRelations,
  getSelectedRelationNames,
  getUnSelectedRelations,
  getUnSelectedRelationNames,
  isRelationSelected,
  getUDGHOME
  )where

import qualified List

import qualified Data.Map               as Map
import qualified Debug.Trace            as Debug

data Relations = Relations {
  relationSelected :: Map.Map Int Bool,
  relationName     :: Map.Map Int String,
  relationIndex    :: Map.Map String Int
  }

--------------------------------------------
-- create an empty map of relations
--------------------------------------------
emptyRelations :: Relations
emptyRelations =
  Relations {
    relationSelected = Map.empty,
    relationName = Map.empty,
    relationIndex = Map.empty
    }

--------------------------------------------
-- set which relations are selected
--------------------------------------------
setSelectedRelations :: Relations -> [Int] -> Relations
setSelectedRelations rels indexs =
  let relSel = Map.mapWithKey (selectRels indexs) (relationSelected rels)
  in Relations {
            relationSelected = relSel,
            relationName = (relationName rels),
            relationIndex = (relationIndex rels)
            }
  where
    selectRels :: [Int] -> Int -> a -> Bool
    selectRels indexs pos _ = if (elem pos indexs) then True else False

--------------------------------------------
-- setUpRelations
--------------------------------------------
setUpRelations :: [String] -> Relations
setUpRelations relationNames =
  let rels = initRels (List.sort relationNames) emptyRelations
  in rels
  where
    initRels :: [String] -> Relations -> Relations
    initRels [] rels = rels
    initRels (name:rest) rels =
      let ind = (Map.size(relationIndex rels))
          relIndex = Map.insert name ind (relationIndex rels)
          relName  = Map.insert ind name (relationName rels)
          relSelected = Map.insert ind True (relationSelected rels)
          newRelations = Relations {
            relationSelected = relSelected,
            relationName = relName,
            relationIndex = relIndex
            }
      in initRels rest newRelations

--------------------------------------------
-- give index of a relation and you will
-- recieve it's name
--------------------------------------------
getRelationName :: Relations -> Int -> (Maybe String)
getRelationName rels ind = Map.lookup ind (relationName rels)

--------------------------------------------
-- get a list of all relations
--------------------------------------------
getRelationNames :: Relations -> [String]
getRelationNames rels = Map.keys (relationIndex rels)

--------------------------------------------
-- give index of a relation and you will
-- recieve if it's selected
--------------------------------------------
isRelationSelected :: Relations -> Int -> (Maybe Bool)
isRelationSelected rels ind = Map.lookup ind (relationSelected rels)

--------------------------------------------
-- give index of a relation and you will
-- recieve it's name
--------------------------------------------
getRelationIndex :: Relations -> String -> (Maybe Int)
getRelationIndex rels name = Map.lookup name (relationIndex rels)

--------------------------------------------
-- get a list of all indexs in the relation-
-- list
--------------------------------------------
getSelectedRelations :: Relations -> [Int]
getSelectedRelations rels =
  Map.keys (Map.filter selectRels (relationSelected rels))
  where
    selectRels :: Bool -> Bool
    selectRels isSelected = isSelected

--------------------------------------------
-- get a list of all indexs in the relation-
-- list
--------------------------------------------
getSelectedRelationNames :: Relations -> [String]
getSelectedRelationNames rels =
  map getNames (Map.keys (Map.filter selectRels (relationSelected rels)))
  where
    selectRels :: Bool -> Bool
    selectRels isSelected = isSelected
    getNames :: Int -> String
    getNames i = removeMaybes (getRelationName rels i)
    removeMaybes :: (Maybe String) -> String
    removeMaybes x =
      case x of
      Nothing -> "panic! this should never happen"
      Just y -> y

--------------------------------------------
-- get a list of all unselected relations
--------------------------------------------
getUnSelectedRelations :: Relations -> [Int]
getUnSelectedRelations rels =
  Map.keys (Map.filter selectRels (relationSelected rels))
  where
    selectRels :: Bool -> Bool
    selectRels isSelected = (not isSelected)

--------------------------------------------
-- get a list of all unselected relations
--------------------------------------------
getUnSelectedRelationNames :: Relations -> [String]
getUnSelectedRelationNames rels =
  map removeMaybes (map (getRelationName rels) (Map.keys (Map.filter selectRels (relationSelected rels))))
  where
    selectRels :: Bool -> Bool
    selectRels isSelected = (not isSelected)
    removeMaybes :: (Maybe String) -> String
    removeMaybes x =
      case x of
      Nothing -> "panic! this should never happen"
      Just y -> y

--------------------------------------------
-- retrieve UDGHOME
--------------------------------------------
getUDGHOME :: [(String, String)] -> IO(String)
getUDGHOME [] = return ("")
getUDGHOME ((var, value):rest)
  | var == "UDG_HOME" = return (value)
  | otherwise = getUDGHOME rest