{- This file differs from the Einar original (itself automatically 
   produced by decommenting an obsolete GHC source file, apparently) with
   nearly all the functions removed and one (compMaybe) added. -}
module Maybes (
   firstJust, -- :: [Maybe a] -> Maybe a
   maybeToBool, -- :: Maybe a -> Bool
                -- same as isJust
   compMaybe -- :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
   ) where

import Maybe -- renamer will tell us if there are any conflicts

import Debug(debug)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : _) = Just x
firstJust (Nothing : ms) = firstJust ms

maybeToBool :: Maybe a -> Bool
maybeToBool = isJust

compMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
compMaybe second first avalue =
   case first avalue of
      Nothing -> Nothing
      Just bvalue -> second bvalue

