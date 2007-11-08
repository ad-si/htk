-- | This module contains various generic functions to be attached to object
-- menus.
module LocalMenus(
   deleteObject,
   ) where

import Computation
import Messages

import Link
import ObjectTypes
import LinkManager
import View

deleteObject
   :: (ObjectType objectType object,HasLinkedObject object)
   => View -> Link object -> IO ()
deleteObject view link =
   do
      object <- readLink view link
      title <- nodeTitleIOPrim object

      goAhead <- confirmMess ("Really delete " ++ show title ++ "?")

      if goAhead
         then
            do
               resultWE <- moveObject (toLinkedObject object) Nothing
               case fromWithError resultWE of
                  Right () ->
                     done
-- don't do this as it creates problems for committing link environments.
-- some time we will need a way of pruning views to remove dead links ...
--                   deleteLink view link
                  Left mess -> warningMess ("Deletion failed: "++mess)
         else
            done
