{- VariableMap is analagous to VariableSet and provides a mutable map ordered
   by key whose changes can be tracked. -}
module VariableMap(
   VariableMapData,
   VariableMapUpdate(..),
   VariableMap,
   newEmptyVariableMap,
   newVariableMap,
   updateMap,
   lookupMap,
   lookupWithDefaultMap,
   mapToList,
   mapToVariableSetSource,

   addToVariableMap,
   variableMapToList,
   lookupVariableMap,
   ) where

import FiniteMap
import Concurrent

import Dynamics
import Sink
import Broadcaster
import VariableSet
import Sources

-- --------------------------------------------------------------------
-- The datatype
-- --------------------------------------------------------------------

newtype VariableMapData key elt = VariableMapData (FiniteMap key elt)

---
-- We recycle the VariableSetUpdate type for this.
newtype VariableMapUpdate key elt = 
   VariableMapUpdate (VariableSetUpdate (key,elt))

update :: Ord key 
   => VariableMapUpdate key elt -> VariableMapData key elt 
   -> (VariableMapData key elt,[VariableMapUpdate key elt])
update (variableUpdate @ (VariableMapUpdate update)) (VariableMapData map) =
   (case update of
      AddElement (key,elt) -> VariableMapData (addToFM map key elt)
      DelElement (key,elt) -> VariableMapData (delFromFM map key)
   , [variableUpdate]
   )

newtype VariableMap key elt = 
   VariableMap (Broadcaster (VariableMapData key elt) 
      (VariableMapUpdate key elt))

-- --------------------------------------------------------------------
-- The provider's interface
-- --------------------------------------------------------------------

---
-- Create a new empty variable map.
newEmptyVariableMap :: Ord key => IO (VariableMap key elt)
newEmptyVariableMap = 
   do
      broadcaster <- newBroadcaster (VariableMapData emptyFM)
      return (VariableMap broadcaster)

---
-- Create a new variable map with given contents
newVariableMap :: Ord key => [(key,elt)] -> IO (VariableMap key elt)
newVariableMap contents =
   do
      broadcaster <- newBroadcaster (VariableMapData (listToFM contents))
      return (VariableMap broadcaster)

---
-- Update a variable map in some way.
updateMap :: Ord key => VariableMap key elt -> VariableMapUpdate key elt 
   -> IO ()
updateMap (VariableMap broadcaster) mapUpdate = 
   applyUpdate broadcaster (update mapUpdate)


-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------


---
-- Unlike VariableSet, the contents of a variable map are not returned in
-- concrete form but as the abstract data type VariableMapData.  We provide
-- functions for querying this.
instance Ord key => HasSource (VariableMap key elt) 
      (VariableMapData key elt) (VariableMapUpdate key elt)
      where
   toSource (VariableMap broadcaster) = toSource broadcaster

lookupMap :: Ord key => VariableMapData key elt -> key -> Maybe elt
lookupMap (VariableMapData map) key = lookupFM map key

lookupWithDefaultMap :: Ord key => VariableMapData key elt -> elt -> key -> elt
lookupWithDefaultMap (VariableMapData map) def key 
   = lookupWithDefaultFM map def key

mapToList :: Ord key => VariableMapData key elt -> [(key,elt)]
mapToList (VariableMapData map) = fmToList map

-- --------------------------------------------------------------------
-- An interface to a VariableMap which makes it look like a variable
-- set source.
-- --------------------------------------------------------------------

data VariableMapSet key elt element = VariableMapSet {
   variableMap :: VariableMap key elt,
   mkElement :: key -> elt -> element
   }

---
-- Given a variable map and conversion function, produce a VariableSetSource
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
                  map (uncurry mkElement) (fmToList contents)
                  )
               )
            .
            (map2
               (\ (VariableMapUpdate update) ->
                  case update of
                     AddElement (key,elt) -> AddElement (mkElement key elt)
                     DelElement (key,elt) -> DelElement (mkElement key elt)
                  )
               )
            $
            (toSource variableMap)   

-- --------------------------------------------------------------------
-- A couple of simple access functions
-- NB.  We don't follow the Registry interface because, without altering
-- the design, it would be difficult to implement some Registry functions.
-- --------------------------------------------------------------------

addToVariableMap :: Ord key => VariableMap key elt -> key -> elt -> IO ()
addToVariableMap variableMap key elt = 
   updateMap variableMap (VariableMapUpdate (AddElement (key,elt)))

variableMapToList :: Ord key => VariableMap key elt -> IO [(key,elt)]
variableMapToList (VariableMap broadcaster) =
   do
      contents <- readContents broadcaster
      return (mapToList contents)

lookupVariableMap :: Ord key => VariableMap key elt -> key -> IO (Maybe elt)
lookupVariableMap (VariableMap broadcaster) key =
   do
      (VariableMapData finiteMap) <- readContents broadcaster
      return (lookupFM finiteMap key)

-- --------------------------------------------------------------------
-- Make VariableMap Typeable
-- --------------------------------------------------------------------

variableMap_tyRep = mkTyRep "VariableMap" "VariableMap"
instance HasTyRep2 VariableMap where
   tyRep2 _ = variableMap_tyRep
