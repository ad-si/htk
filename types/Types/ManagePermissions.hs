-- | This module manages permissions from the client side.
module Types.ManagePermissions(
   editGlobalPermissions,
   permissionsMenu,
   editObjectPermissions,
   showAllPermissions,
   ) where

import Util.VariableSet(toKey)

import Util.Computation
import Util.Messages
import Util.Registry

import HTk.Toolkit.MenuType

import SimpleDB.Permissions
import Types.VersionDB

import Types.Link
import Types.ViewType
import Types.ObjectTypes
import Types.LinkManager


-- -----------------------------------------------------------------------
-- Editing and setting the global permissions
-- -----------------------------------------------------------------------

editGlobalPermissions :: Repository -> IO ()
editGlobalPermissions repository =
   do
      permissions0 <- getPermissions repository Nothing
      permissions1Opt <- editPermissions permissions0
      case permissions1Opt of
         Nothing -> done
         Just permissions1 -> setPermissions repository Nothing permissions1

-- -----------------------------------------------------------------------
-- Editing the permissions for an object.
-- -----------------------------------------------------------------------

permissionsMenu :: (ObjectType objectType object,HasLinkedObject object)
   => View -> [MenuPrim (Maybe String) (Link object -> IO ())]
permissionsMenu view = [
   Button "Edit Permissions" (\ link ->
      editObjectPermissions view link
      ),
   Button "View All Permissions" (\ link ->
      showAllPermissions view link
      )
   ]


editObjectPermissions
   :: (ObjectType objectType object,HasLinkedObject object)
   => View
   -> Link object
   -> IO ()
editObjectPermissions view thisLink =
   do
      version <- getViewVersion view
      let
         thisLocation = toKey thisLink

         thisOVOpt = Just (version,thisLocation)

      permissions0 <- getPermissions (repository view) thisOVOpt
      permissions1Opt <- editPermissions permissions0
      case permissions1Opt of
         Nothing -> done
         Just permissions1 -> setPermissions (repository view) thisOVOpt
            permissions1


-- -----------------------------------------------------------------------
-- Getting the parent LinkedObject (with respect to permissions).
-- -----------------------------------------------------------------------

-- | Get all permissions affecting an object.
showAllPermissions
   :: (ObjectType objectType object,HasLinkedObject object)
   => View
   -> Link object
   -> IO ()
showAllPermissions view link =
   do
      object <- readLink view link
      let
         thisLinkedObject = toLinkedObject object
      allPermissions <- getAllPermissions view thisLinkedObject

      (names1 :: [(String,Permissions)]) <- mapM
            (\ (object,permissions) ->
               do
                  name <- getFullName view object
                  return (name,permissions)
               )
            allPermissions

      globalPermissions <- getPermissions (repository view) Nothing
      let
         names = names1 ++ [("Global",globalPermissions)]

      let
         output = unlines (
            map
               (\ (name,permissions) ->
                  name ++ ": " ++ unparsePermissions permissions
                  )
               names
            )

      messageMess output




-- | Get all permissions for this object and the parent objects.
getAllPermissions :: View -> LinkedObject
   -> IO [(LinkedObject,Permissions)]
      -- ^ list of linked objects and permissions, from the given object
      -- upwards.
getAllPermissions view thisLinkedObject =
   do
      parentVersion <- getViewVersion view
      let
         thisLocation = toLocation thisLinkedObject

      thisPermissions <- getPermissions (repository view)
         (Just (parentVersion,thisLocation))
      remainingList <-
         do
            parentLinkedObjectOpt <- getPermissionsParentLinkedObject
               view thisLinkedObject
            case parentLinkedObjectOpt of
               Just parentLinkedObject ->
                  getAllPermissions view parentLinkedObject
               Nothing -> return []
      return ((thisLinkedObject,thisPermissions) : remainingList)

-- | Get the parent linked object (with respect to permissions).
--
-- NB.  For now at least this function is not capable of deducing the
-- parent linked object if that it not the parent linked object in the view,
-- since the repository can only tell us its location, not its type.
-- This *should* however be sufficient.
getPermissionsParentLinkedObject
   :: View -> LinkedObject -> IO (Maybe LinkedObject)
getPermissionsParentLinkedObject view thisLinkedObject =
   do
      let
         thisLocation :: Location
         thisLocation = toLocation thisLinkedObject

      parentLocationOpt <-
         do
            parentLocationOpt <- getValueOpt (parentChanges view) thisLocation
            case parentLocationOpt of
               Just parentLocation -> return parentLocationOpt
                  -- object has been moved since last commit
               Nothing ->
                  do
                     version <- getViewVersion view
                     getParentLocation (repository view) version thisLocation
      case parentLocationOpt of
         Nothing -> return Nothing
         Just parentLocation ->
            do
               let
                  failure =
                     do
                        alertMess "Unable to determine further ancestors"
                        return Nothing
               thisInsertionOpt
                  <- getCurrentInsertion thisLinkedObject
               case thisInsertionOpt of
                  Nothing -> failure
                  Just insertion ->
                     do
                        let
                           (parentLinkedObject,_) = unmkInsertion insertion
                        if toLocation parentLinkedObject == parentLocation
                           then
                              return (Just parentLinkedObject)
                           else
                              failure

-- -----------------------------------------------------------------------
-- Utility functions
-- -----------------------------------------------------------------------

getViewVersion :: View -> IO ObjectVersion
getViewVersion view =
   do
      parentVersionOpt <- getParentVersion view
      case parentVersionOpt of
         Just parentVersion -> return parentVersion
         Nothing ->
            error ("Attempt to get permissions for a view with no parent "
               ++ " version")

toLocation :: LinkedObject -> Location
toLocation = toKey . toWrappedLink

