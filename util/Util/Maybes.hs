-- | This file differs from the Einar original (itself automatically
-- produced by decommenting an obsolete GHC source file, apparently) with
-- nearly all the functions removed and one (compMaybe) added.
module Util.Maybes (
   firstJust, -- :: [Maybe a] -> Maybe a
   maybeToBool, -- :: Maybe a -> Bool
                -- same as isJust
   fromJustError, -- :: String -> Maybe a -> a
      -- from a Maybe, with an error message if it fails.

   fromMaybes, -- :: [Maybe a] -> Maybe [a]
      -- check that all the Maybes are really Just's.
   ) where

import Maybe -- renamer will tell us if there are any conflicts

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : _) = Just x
firstJust (Nothing : ms) = firstJust ms

maybeToBool :: Maybe a -> Bool
maybeToBool = isJust

fromJustError :: String -> Maybe a -> a
fromJustError _ (Just x) = x
fromJustError mess Nothing = error mess

fromMaybes :: [Maybe a] -> Maybe [a]
fromMaybes [] = Just []
fromMaybes (Nothing : _) = Nothing
fromMaybes (Just a : rest) = fmap (a :) (fromMaybes rest)
