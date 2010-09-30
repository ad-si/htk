-- | This file differs from the Einar original (itself automatically
-- produced by decommenting an obsolete GHC source file, apparently) with
-- nearly all the functions removed.
module Util.Maybes (
   fromMaybes, -- :: [Maybe a] -> Maybe [a]
      -- check that all the Maybes are really Just's.
   ) where

fromMaybes :: [Maybe a] -> Maybe [a]
fromMaybes [] = Just []
fromMaybes (Nothing : _) = Nothing
fromMaybes (Just a : rest) = fmap (a :) (fromMaybes rest)
