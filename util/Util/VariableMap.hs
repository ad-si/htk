{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | VariableMap is analagous to VariableSet and provides a mutable map ordered
-- by key whose changes can be tracked.
module Util.VariableMap(
   VariableMapData,
   VariableMapUpdate(..),
   VariableMap,
   newEmptyVariableMap,
   newVariableMap,
   newVariableMapFromFM,
   updateMap,
   lookupMap,
   lookupWithDefaultMap,
   mapToList,
   mapToFM,
   mapToVariableSetSource,

   addToVariableMap,
   delFromVariableMap,
   variableMapToList,
   lookupVariableMap,
   getVariableMapByKey,
   ) where

import Data.Maybe

import qualified Data.Map as Map

import Util.Dynamics
import Util.Broadcaster
import Util.VariableSet
import Util.Sources

-- --------------------------------------------------------------------
-- The datatype
-- --------------------------------------------------------------------

-- | Describes a map update.  For DelUpdate, the second parameter (the one
-- of type elt) is irrelevant and may be undefined.
newtype VariableMapData key elt = VariableMapData (Map.Map key elt)

-- | We recycle the VariableSetUpdate type for this.
newtype VariableMapUpdate key elt =
   VariableMapUpdate (VariableSetUpdate (key,elt))

-- | The Bool indicates whether the operation was successfully carried out.
-- We block updating a value which is already in the map, or
-- deleting one that isn\'t.
update :: Ord key
   => VariableMapUpdate key elt -> VariableMapData key elt
   -> (VariableMapData key elt,[VariableMapUpdate key elt],Bool)
update (variableUpdate @ (VariableMapUpdate update))
       (variableMap @ (VariableMapData map)) =
   case update of
      AddElement (key,elt) ->
         if member key
            then
               (variableMap,[],False)
            else
               (VariableMapData (Map.insert key elt map),[variableUpdate],True)
      DelElement (key,_) ->
         -- we ignore the element, allowing delFromVariable map to put an
         -- error there.
         case Map.lookup key map of
            Just elt ->
               (VariableMapData (Map.delete key map),
                  [VariableMapUpdate (DelElement (key,elt))],True)
            Nothing -> (variableMap,[],False)
      BeginGroup -> (variableMap,[variableUpdate],True)
      EndGroup -> (variableMap,[variableUpdate],True)
   where
      member key = isJust (Map.lookup key map)

newtype VariableMap key elt =
   VariableMap (GeneralBroadcaster (VariableMapData key elt)
      (VariableMapUpdate key elt))
   deriving (Typeable)

-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------

-- | Create a new empty variable map.
newEmptyVariableMap :: Ord key => IO (VariableMap key elt)
newEmptyVariableMap =
   do
      broadcaster <- newGeneralBroadcaster (VariableMapData Map.empty)
      return (VariableMap broadcaster)

-- | Create a new variable map with given contents
newVariableMap :: Ord key => [(key,elt)] -> IO (VariableMap key elt)
newVariableMap contents = newVariableMapFromFM (Map.fromList contents)

newVariableMapFromFM :: Ord key
   => Map.Map key elt -> IO (VariableMap key elt)
newVariableMapFromFM fmap =
   do
      broadcaster <- newGeneralBroadcaster (VariableMapData fmap)
      return (VariableMap broadcaster)


-- | Update a variable map in some way.  Returns True if the update was
-- sucessful (so for insertions, the object is not already there; for
-- deletions the object is not there).
updateMap :: Ord key => VariableMap key elt -> VariableMapUpdate key elt
   -> IO Bool
updateMap (VariableMap broadcaster) mapUpdate =
   applyGeneralUpdate broadcaster (update mapUpdate)


-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------


-- | Unlike VariableSet, the contents of a variable map are not returned in
-- concrete form but as the abstract data type VariableMapData.  We provide
-- functions for querying this.
instance Ord key => HasSource (VariableMap key elt)
      (VariableMapData key elt) (VariableMapUpdate key elt)
      where
   toSource (VariableMap broadcaster) = toSource broadcaster

lookupMap :: Ord key => VariableMapData key elt -> key -> Maybe elt
lookupMap (VariableMapData map) key = Map.lookup key map

lookupWithDefaultMap :: Ord key => VariableMapData key elt -> elt -> key -> elt
lookupWithDefaultMap (VariableMapData map) def key
   = Map.findWithDefault def key map

mapToList :: Ord key => VariableMapData key elt -> [(key,elt)]
mapToList = Map.toList . mapToFM

mapToFM :: Ord key => VariableMapData key elt -> Map.Map key elt
mapToFM (VariableMapData map) = map

-- --------------------------------------------------------------------
-- An interface to a VariableMap which makes it look like a variable
-- set source.
-- --------------------------------------------------------------------

data VariableMapSet key elt element = VariableMapSet {
   variableMap :: VariableMap key elt,
   mkElement :: key -> elt -> element
   }

-- | Given a variable map and conversion function, produce a VariableSetSource
mapToVariableSetSource :: Ord key => (key -> elt -> element)
   -> VariableMap key elt -> VariableSetSource element
mapToVariableSetSource mkElement variableMap = toSource (VariableMapSet
      {variableMap = variableMap,mkElement = mkElement})

instance Ord key => HasSource (VariableMapSet key elt element) [element]
     (VariableSetUpdate element)
   where
      toSource (VariableMapSet
         {variableMap = variableMap,mkElement = mkElement}) =
            (map1
               (\ (VariableMapData contents) ->
                  map (uncurry mkElement) (Map.toList contents)
                  )
               )
            .
            (map2
               (\ (VariableMapUpdate update) ->
                  fmap (\ (key,elt) -> mkElement key elt) update
                  )
               )
            $
            (toSource variableMap)

-- --------------------------------------------------------------------
-- A couple of simple access functions
-- NB.  We don't follow the Registry interface because, without altering
-- the design, it would be difficult to implement some Registry functions.
-- --------------------------------------------------------------------

addToVariableMap :: Ord key => VariableMap key elt -> key -> elt -> IO Bool
addToVariableMap variableMap key elt =
   updateMap variableMap (VariableMapUpdate (AddElement (key,elt)))

delFromVariableMap :: Ord key => VariableMap key elt -> key -> IO Bool
delFromVariableMap variableMap key =
   updateMap variableMap (VariableMapUpdate (DelElement (key,
      error ("VariableMap.delFromVariableMap"))))

variableMapToList :: Ord key => VariableMap key elt -> IO [(key,elt)]
variableMapToList (VariableMap broadcaster) =
   do
      contents <- readContents broadcaster
      return (mapToList contents)

lookupVariableMap :: Ord key => VariableMap key elt -> key -> IO (Maybe elt)
lookupVariableMap (VariableMap broadcaster) key =
   do
      (VariableMapData finiteMap) <- readContents broadcaster
      return (Map.lookup key finiteMap)

-- --------------------------------------------------------------------
-- Returns current value of key (if any) in variable map
-- NB.  This implementation is very inefficient and it is in an inner loop
-- in types/LinkManager.  However it could be made much better by changing
-- the type.
-- --------------------------------------------------------------------

getVariableMapByKey :: Ord key => VariableMap key elt -> key
   -> SimpleSource (Maybe elt)
getVariableMapByKey variableMap key =
   let
      source1 = toSource variableMap
      source2 =
         (map1
            (\ (VariableMapData fmap) -> Map.lookup key fmap)
            )
         .
         (filter2
            (\ (VariableMapUpdate update) -> case update of
               AddElement (key2,elt)
                  | key2 == key -> Just (Just elt)
               DelElement (key2,elt)
                  | key2 == key -> Just Nothing
               _ -> Nothing
               )
            )
         $
         source1
   in
      SimpleSource source2
