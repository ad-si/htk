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
   mapToSinkSource,
   ) where

import FiniteMap
import Concurrent

import Dynamics
import Sink
import Broadcaster
import VariableSet

-- --------------------------------------------------------------------
-- The datatype
-- --------------------------------------------------------------------

newtype VariableMapData key elt = VariableMapData (FiniteMap key elt)

---
-- We recycle the VariableSetUpdate type for this.
newtype VariableMapUpdate key elt = 
   VariableMapUpdate (VariableSetUpdate (key,elt))

update :: Ord key 
   => VariableMapData key elt -> VariableMapUpdate key elt 
   -> VariableMapData key elt
update (VariableMapData map) (VariableMapUpdate update) =
   case update of
      AddElement (key,elt) -> VariableMapData (addToFM map key elt)
      DelElement (key,elt) -> VariableMapData (delFromFM map key)

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
      broadcaster <- newBroadcaster update (VariableMapData emptyFM)
      return (VariableMap broadcaster)

---
-- Create a new variable map with given contents
newVariableMap :: Ord key => [(key,elt)] -> IO (VariableMap key elt)
newVariableMap contents =
   do
      broadcaster <- 
         newBroadcaster update (VariableMapData (listToFM contents))
      return (VariableMap broadcaster)

---
-- Update a variable map in some way.
updateMap :: Ord key => VariableMap key elt -> VariableMapUpdate key elt 
   -> IO ()
updateMap (VariableMap broadcaster) update = 
   updateBroadcaster broadcaster update


-- --------------------------------------------------------------------
-- The client's interface
-- --------------------------------------------------------------------


---
-- Unlike VariableSet, the contents of a variable map are not returned in
-- concrete form but as the abstract data type VariableMapData.  We provide
-- functions for querying this.
instance Ord key 
   => CanAddSinks (VariableMap key elt) (VariableMapData key elt) 
         (VariableMapUpdate key elt) where
   addOldSink (VariableMap broadcaster) sink = addOldSink broadcaster sink

   readContents (VariableMap broadcaster) = readContents broadcaster


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
mapToSinkSource :: Ord key => (key -> elt -> element) -> VariableMap key elt 
   -> VariableSetSource element
mapToSinkSource mkElement variableMap = SinkSource (VariableMapSet 
      {variableMap = variableMap,mkElement = mkElement})

instance Ord key => CanAddSinks (VariableMapSet key elt element) [element] 
      (VariableSetUpdate element) where
   addOldSink 
         (VariableMapSet {variableMap = variableMap,
            mkElement = mkElement}) sink =
      do
         let
            sink2 = coMapSink
               (\ (VariableMapUpdate update) ->
                  case update of
                     AddElement (key,elt) -> AddElement (mkElement key elt)
                     DelElement (key,elt) -> DelElement (mkElement key elt)
                  )
               sink
         VariableMapData contents <- addOldSink variableMap sink2
         let elements = map (uncurry mkElement) (fmToList contents)
         return elements
   readContents 
         (VariableMapSet {variableMap = variableMap,
            mkElement = mkElement}) =
      do
         VariableMapData contents <- readContents variableMap
         let elements = map (uncurry mkElement) (fmToList contents)
         return elements

   

-- --------------------------------------------------------------------
-- Make VariableMap Typeable
-- --------------------------------------------------------------------

variableMap_tyRep = mkTyRep "VariableMap" "VariableMap"
instance HasTyRep2 VariableMap where
   tyRep2 _ = variableMap_tyRep
