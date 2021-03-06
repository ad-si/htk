-- | This is the module which handles path aliases in imports
module Imports.Aliases(
   Aliases,
      -- set of path aliases in a package

   mkAliases,
      -- :: ImportCommands -> WithError Aliases
      -- Make a set of aliases given a set of import commands, checking for
      -- circularities.

   expandAliases,
      -- :: Aliases -> EntitySearchName -> EntitySearchName
      -- Expand all aliases in the given EntitySearchName
   ) where

import qualified Data.Map as Map

import Util.Computation
import Util.AtomString
import Util.ExtendedPrelude

import Graphs.FindCycle

import Imports.EntityNames

-- ------------------------------------------------------------------------
-- Datatype
-- ------------------------------------------------------------------------

data Aliases = Aliases {
   begin :: Map.Map EntityName EntitySearchName,
      -- path aliases which begin with Current., Root. or Parent.
   general :: Map.Map EntityName [EntityName]
      -- others
   }

-- ------------------------------------------------------------------------
-- Functions
-- ------------------------------------------------------------------------

mkAliases :: ImportCommands -> WithError Aliases
mkAliases (ImportCommands importCommands) =
   let
      -- transform the aliases, checking for repetitions.
      readAliases :: [ImportCommand] -> Aliases -> WithError Aliases
      readAliases [] aliases = hasValue aliases
      readAliases (Import _ _ : importCommands) aliases =
         readAliases importCommands aliases
      readAliases (PathAlias from esn : importCommands)
            (aliases0 @ (Aliases {begin = begin0,general = general0})) =
         if Map.member from begin0 || Map.member from general0
            then
               hasError ("Alias " ++ toString from ++ " is multiply defined")
            else
               readAliases importCommands (
                  case esn of
                     FromHere (EntityFullName to) ->
                        aliases0 {general = Map.insert from to general0}
                     _ -> aliases0 {begin = Map.insert from esn begin0}
                  )

      cycleCheckAliases :: Aliases -> WithError Aliases
      cycleCheckAliases
            (aliases @ (Aliases {begin = begin0,general = general0})) =
         -- Only aliases in the general0, namely those where the RHS does not
         -- begin with Root, Parent or Current, can cause problems.  For
         -- aliases which begin Root, Parent or Current can only be expanded
         -- at the start of a search name, and so cannot be expanded again.
         --
         -- So we include the begin0 names in the graph, but do not check
         -- their expansions.

         let
            nodes :: [EntityName]
            nodes = Map.keys begin0 ++ Map.keys general0

            components :: EntityName -> [EntityName]
            components entityName =
               Map.findWithDefault [] entityName general0
         in
            case findCycle nodes components of
               Nothing -> hasValue aliases
               Just cycle ->
                  hasError ("Cycle detected in aliases " ++
                     unsplitByChar '-' (map toString cycle))
   in
      mapWithError'
         cycleCheckAliases
         (readAliases importCommands
            (Aliases {begin = Map.empty,general = Map.empty}))

expandAliases :: Aliases -> EntitySearchName -> EntitySearchName
expandAliases (aliases @ (Aliases {begin = begin,general = general}))
   esn =
      case esn of
         FromHere (EntityFullName [])
            -> esn
         FromHere (EntityFullName (name1 : names))
            -> expandHere name1 names
         FromCurrent (EntityFullName names)
            -> FromCurrent (EntityFullName (expandNames names))
         FromRoot (EntityFullName names)
            -> FromRoot (EntityFullName (expandNames names))
         FromParent (FromHere (EntityFullName names))
            -> FromParent (FromHere (EntityFullName (expandNames names)))
         FromParent esn2
            ->  FromParent (expandAliases aliases esn2)
   where
      expandHere :: EntityName -> [EntityName] -> EntitySearchName
      expandHere name1 names =
         case Map.lookup name1 begin of
            Nothing -> FromHere (EntityFullName (expandNames (name1 : names)))
            Just esn -> expandAliases aliases (searchPlusNames esn names)

      expandNames :: [EntityName] -> [EntityName]
      expandNames names = concat (map expandName names)

      expandName :: EntityName -> [EntityName]
      expandName name = case Map.lookup name general of
         Nothing -> [name]
         Just names -> expandNames names

      searchPlusNames :: EntitySearchName -> [EntityName] -> EntitySearchName
      searchPlusNames esn names2 = case esn of
         FromHere fn1 -> FromHere (namePlusNames fn1 names2)
         FromCurrent fn1 -> FromCurrent (namePlusNames fn1 names2)
         FromRoot fn1 -> FromRoot (namePlusNames fn1 names2)
         FromParent esn2 -> FromParent (searchPlusNames esn2 names2)

      namePlusNames :: EntityFullName -> [EntityName] -> EntityFullName
      namePlusNames (EntityFullName names1) names2
         = EntityFullName (names1 ++ names2)
