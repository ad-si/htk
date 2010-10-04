-- | Encapsulation of Event parameters used in TkCommands.
module HTk.Kernel.EventInfo(
   EventParameter(..), -- Type of wish event information
   -- epToChar/epFromChar convert to and from Wish's 1-character
   -- names for this information.
   epToChar, -- :: EventParameter -> Char
   epFromChar, -- :: Char -> EventParameter


   EventInfoSet, -- Describes what event information we are interested in.
   emptyEventInfoSet, -- :: EventInfoSet
   mkEventInfoSet, -- :: [EventParameter] -> EventInfoSet
   listEventInfoSet, -- :: EventInfoSet -> [EventParameter]
   addEventInfoSet, -- :: EventInfoSet -> [EventParameter] -> EventInfoSet
   delEventInfoSet, -- :: EventInfoSet -> [EventParameter] -> EventInfoSet

   EventInfo(..), -- Information for a particular event.
   mkEventInfo, -- :: [(EventParameter,String)] -> EventInfo
   -- getEventPar, -- :: EventInfo -> EventParameter -> String

   -- restrict, -- :: EventInfo -> EventInfoSet -> Maybe EventInfo
   -- Checks that all the information in the specified set
   -- is present and restricts the EventInfo to that.

   defaultEventInfoSet
   ) where

import qualified Data.Set as Set

import HTk.Kernel.Geometry(Distance)


-- --------------------------------------------------------------
-- Datatypes
-- --------------------------------------------------------------

newtype EventInfoSet = EventInfoSet (Set.Set EventParameter)

data EventInfo = EventInfo { x :: Distance,
                             y :: Distance,
                             xRoot :: Distance,
                             yRoot :: Distance,
                             button :: Int
                             -- more to come!
                           }

defaultEventInfoSet :: EventInfoSet
defaultEventInfoSet = mkEventInfoSet [Px, Py, PX, PY, Pb]


-- --------------------------------------------------------------
-- Event Parameters
-- --------------------------------------------------------------

-- Types of information that come with Events.
-- (page 298)
-- The names of these constructors all begin with P followed by
-- the %keyword required, except for # which is done by HASH
-- EventParameter needs to instance Ord for WishBasics.
data EventParameter =
   HASH | Pa | Pb | Pc | Pd | Pf | Ph | Pk | Pm | Po | Pp |
   Ps | Pt | Pv | Pw | Px | Py | PA | PB | PE | PK | PN |
   PR | PS | PT | PW | PX | PY deriving (Eq,Ord,Show,Read)

epToChar :: EventParameter -> Char
epToChar ep =
   -- avert your eyes, if of sensitive disposition
   case show ep of
      ['P',c] -> c
      "HASH" -> '#'

epFromChar :: Char -> EventParameter
epFromChar ch =
   -- avert your eyes again please!
   case ch of
      '#' -> HASH
      other -> read ['P',other]


-- --------------------------------------------------------------
-- Functions
-- --------------------------------------------------------------

listEventInfoSet :: EventInfoSet -> [EventParameter]
listEventInfoSet (EventInfoSet set) = Set.toList set

mkEventInfoSet :: [EventParameter] -> EventInfoSet
mkEventInfoSet eventPars = EventInfoSet (Set.fromList eventPars)

emptyEventInfoSet :: EventInfoSet
emptyEventInfoSet = mkEventInfoSet []

addEventInfoSet :: EventInfoSet -> [EventParameter] -> EventInfoSet
addEventInfoSet (EventInfoSet set) eventPars =
   EventInfoSet(Set.union set (Set.fromList eventPars))

delEventInfoSet :: EventInfoSet -> [EventParameter] -> EventInfoSet
delEventInfoSet (EventInfoSet set) eventPars =
   EventInfoSet(Set.difference set (Set.fromList eventPars))

mkEventInfo :: [(EventParameter,String)] -> EventInfo
mkEventInfo settings =
  foldl getEvPar (EventInfo 0 0 0 0 0) settings
  where getEvPar i (Px, val) = i {x= read val}
        getEvPar i (Py, val) = i {y= read val}
        getEvPar i (Pb, val) = i {button= read val}
        getEvPar i (PX, val) = i {xRoot = read val}
        getEvPar i (PY, val) = i {yRoot = read val}

