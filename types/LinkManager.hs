module LinkManager(
   LinkManager, -- Manages all links in a view
   LinkedObject, -- An object.  Objects may contain further objects
      -- (like folders)
   LinkSource, -- A source of links to items to be searched for.

   newLinkManager, -- :: WrappedLink -> IO LinkManager
      -- Create a new Link Manager, given a WrappedLink corresponding to
      -- the top folder.

   toTopObject, -- :: LinkManager -> LinkedObject,
      -- Get the top folder for the link manager (which is fixed)
   toWrappedLink, -- :: LinkedObject -> WrappedLink
      -- Extract the WrappedLink for a LinkedObject.

   Insertion, -- specifies a position for LinkedObject
   InsertionSource, -- = SimpleSource (Maybe Insertion)
      -- Nothing causes the object in question to be forgotten about by the
      -- LinkManager.

   newLinkedObject, 
      -- :: LinkManager -> WrappedLink -> InsertionSource -> IO LinkedObject
      -- create a new LinkedObject, likewise specifying the location.

   objectContents, 
      -- :: LinkManager -> LinkedObject -> VariableSetSource WrappedLink
      -- keeps track of those objects inserted into a parent object.

   mkLinkSource, -- LinkManager -> SimpleSource EntityPath -> LinkedObject 
                 -- -> SimpleSource [EntityFullName] -> IO LinkSource
      -- Create a new LinkSource, mapping EntityFullName's to lists of
      -- LinkedObjects.  The EntitySearchNames are interpreted according to the
      -- EntityPath; this may in turn include relative paths which are 
      -- interpreted according to the LinkedObject.

   listFromLinkSource, -- :: LinkSource -> SimpleSource [LinkedObject]
      -- This extracts the currently valid links for a LinkSource.
   verifyLinkSource, -- LinkSource -> IO [EntityFullName]
      -- returns a list of those EntityFullName's which cannot at this time
      -- be matched.
   ) where

import Maybe

import IOExts

import Debug
import Computation
import Sources
import Broadcaster
import Sink
import VariableMap
import VariableSet

import ObjectTypes
import EntityNames

data Insertion = Insertion {
   parent :: LinkedObject, -- folder to contain this object
   name :: EntityName, -- name it shall have
   tellResult :: String -> IO () 
      -- action to be performed after insertion was attempted.  If successful
      -- "" is given as an argument; otherwise (for example if an object
      -- of this name already exists in the folder) a non-null String is
      -- applied and nothing is done.
   }

type InsertionSource = SimpleSource (Maybe Insertion)

-- ----------------------------------------------------------------------
-- The types
-- ----------------------------------------------------------------------

data LinkManager = LinkManager {
   topObject :: LinkedObject
   }

data LinkedObject = LinkedObject {
   wrappedLink :: WrappedLink,
   insertion :: InsertionSource,
   contents :: VariableMap EntityName LinkedObject
   }

data LinkSource = LinkSource {
   object :: LinkedObject,
   pathSource :: SimpleSource EntityPath,      
   linksSource :: SimpleSource [EntityFullName],
   targetsSource :: SimpleSource [Maybe LinkedObject]
   }

-- ----------------------------------------------------------------------
-- Creating things
-- ----------------------------------------------------------------------

newLinkManager :: WrappedLink -> IO LinkManager
newLinkManager wrappedLink =
   do
      let
         insertion = staticSimpleSource Nothing
      contents <- newEmptyVariableMap
      -- Create a new Link Manager, given a WrappedLink corresponding to
      -- the top folder.
      let
         topObject = LinkedObject {
            wrappedLink = wrappedLink,
            insertion = insertion,
            contents = contents
            }

         linkManager = LinkManager {
            topObject = topObject
            }

      return linkManager

toTopObject :: LinkManager -> LinkedObject
toTopObject linkManager = topObject linkManager

toWrappedLink :: LinkedObject -> WrappedLink
toWrappedLink linkedObject = wrappedLink linkedObject

newLinkedObject :: LinkManager -> WrappedLink -> InsertionSource 
   -> IO LinkedObject
newLinkedObject linkManager wrappedLink insertion =
   do
      newContents <- newEmptyVariableMap
      let
         linkedObject = LinkedObject {
            wrappedLink = wrappedLink,
            insertion = insertion,
            contents = newContents
            }

      -- Set up machinery so that the contents of enclosing folders are
      -- kept up to date.

      -- We use an IORef (horrible I know) to record the last successful
      -- insertion
      (insertionIORef :: IORef (Maybe Insertion)) <- newIORef Nothing

      let
         -- action to be run each time the object is moved.
         insertionAction :: Maybe Insertion -> IO ()
         insertionAction this =
            do
               previous <- readIORef insertionIORef
               let
                  -- we only do this after the object has been successfully
                  -- inserted in its new location.
                  removePrevious =
                     case previous of
                        Nothing -> -- this is presumably the first insertion
                           done
                        Just oldInsertion ->
                           do
                              success <- delFromVariableMap 
                                 (contents (parent oldInsertion)) 
                                 (name oldInsertion)
                              debugTest success "LinkManager.1"

               case this of
                  Nothing -> -- this must be a deletion
                     do
                        writeIORef insertionIORef this
                        removePrevious
                  Just newInsertion ->
                     do
                        success <- addToVariableMap
                           (contents (parent newInsertion)) (name newInsertion)
                              linkedObject
                        if success 
                           then
                              do
                                 writeIORef insertionIORef this
                                 removePrevious
                                 tellResult newInsertion ""
                           else
                              tellResult newInsertion (
                                 "There is already an object "++
                                 show (name newInsertion)++" in the folder")
                       
      addNewQuickSink insertion insertionAction 
      return linkedObject

objectContents :: LinkManager -> LinkedObject -> VariableSetSource WrappedLink
objectContents _ linkedObject =
   mapToVariableSetSource (\ _ linkedObject -> wrappedLink linkedObject)
      (contents linkedObject) 

-- ----------------------------------------------------------------------
-- Looking up names
-- ----------------------------------------------------------------------

mkLinkSource :: LinkManager -> SimpleSource EntityPath -> LinkedObject 
   -> SimpleSource [EntityFullName] -> IO LinkSource
mkLinkSource _ pathSource object linksSource =
   return (
      let
         targetsSource =
            do
               (path,links) <- pairSimpleSources pathSource linksSource
               sequenceSimpleSource (map (mapOneName object path) links)
      in
         LinkSource {
            object = object,
            pathSource = pathSource,
            linksSource = linksSource,
            targetsSource = targetsSource
            }
      )

listFromLinkSource :: LinkSource -> SimpleSource [LinkedObject]
listFromLinkSource linkSource = 
   fmap catMaybes (targetsSource linkSource)

verifyLinkSource linkSource =
   do
      targets <- readContents (targetsSource linkSource)
      if any isNothing targets
         then
            do
               -- since targets doesn't tell us what the names are, we go
               -- through them one by one.   
               names <- readContents (linksSource linkSource)
               path <- readContents (pathSource linkSource)
               namesOpts <- mapM
                  (\ name ->
                     do
                        linkedOpt <- readContents 
                           (mapOneName (object linkSource) path name)
                        return (fmap (\ _ -> name) linkedOpt)
                     ) 
                  names
               return (catMaybes namesOpts)
         else
            return []

mapOneName :: LinkedObject -> EntityPath -> EntityFullName 
   -> SimpleSource (Maybe LinkedObject)
mapOneName linkedObject (EntityPath []) fullName = return Nothing
mapOneName linkedObject (EntityPath (firstSearch:rest)) fullName =
   do
      linkedObjectOpt <- mapSearchName linkedObject firstSearch fullName
      case linkedObjectOpt of
         Nothing -> mapOneName linkedObject (EntityPath rest) fullName
         _ -> return linkedObjectOpt

mapSearchName :: LinkedObject -> EntitySearchName -> EntityFullName 
   -> SimpleSource (Maybe LinkedObject)
mapSearchName linkedObject (FromHere entityFullName) toSearch =
   mapFullNameFullName linkedObject entityFullName toSearch
mapSearchName linkedObject (FromParent entitySearchName) toSearch =
   do
      insertionOpt <- insertion linkedObject
      case insertionOpt of
         Nothing -> return Nothing
         Just insertion ->
            mapSearchName (parent insertion) entitySearchName toSearch      

---
-- Search for the second EntityFullName down the path given by the first
-- EntityFullName
mapFullNameFullName :: LinkedObject -> EntityFullName -> EntityFullName 
   -> SimpleSource (Maybe LinkedObject)
mapFullNameFullName linkedObject (EntityFullName n1) (EntityFullName n2)
   = mapFullName linkedObject (EntityFullName (n1 ++ n2))

---
-- Search for the EntityFullName searching from the given object
mapFullName :: LinkedObject -> EntityFullName 
   -> SimpleSource (Maybe LinkedObject)
mapFullName linkedObject (EntityFullName []) = return (Just linkedObject)
mapFullName linkedObject (EntityFullName (first:rest)) =
   do
      linkedObjectOpt <- getVariableMapByKey (contents linkedObject) first
      case linkedObjectOpt of
         Nothing -> return Nothing
         Just linkedObject 
            -> mapFullName linkedObject (EntityFullName rest)

