-- |
-- Description: What objects need to be mergeable.
--
-- This describes the various types relevant to the interface between the
-- repository and object implementations and merging.
module Types.MergeTypes(
   ObjectLinks(..),
   concatObjectLinks, -- :: [ObjectLinks key] -> ObjectLinks key

   MergeLinks(..),
   emptyMergeLinks, -- :: MergeLinks object
   singletonMergeLinks,
      -- :: HasCodedValue object
      -- => (object -> WrappedMergeLink) -> MergeLinks object

   pairMergeLinks,
      -- :: MergeLinks object -> MergeLinks object -> MergeLinks object

   LinkReAssigner(..),
   HasMerging(..),
   WrappedMergeLink(..), -- instance HasKey _ Location
   unpackWrappedMergeLink,
      -- :: HasMerging object => WrappedMergeLink -> Maybe (Link object)


   mapLink,
      -- :: HasMerging object => LinkReAssigner -> View -> Link object
      -- -> Link object

   PostMerge,
   newPostMerge, -- :: IO () -> PostMerge

   doPostMerge, -- :: PostMerge -> IO ()
   )
 where

import qualified Data.Map as Map

import Util.Dynamics
import Util.Computation
import Util.VariableSet(HasKey(..))

import SimpleDB.Interface(Location)
import SimpleDB.VersionInfo(ObjectVersion,VersionInfo)

import Types.CodedValue
import Types.Link
import Types.ViewType

-- | This describes all the links which leave an object and which need to
-- be preserved by the merge.
data ObjectLinks key = ObjectLinks [(WrappedMergeLink,key)]

instance Functor ObjectLinks where
   fmap fn (ObjectLinks l) =
      ObjectLinks (map (\ (wrappedLink,key) -> (wrappedLink,fn key)) l)

concatObjectLinks :: [ObjectLinks key] -> ObjectLinks key
concatObjectLinks l
   = ObjectLinks (concat (map (\ (ObjectLinks links) -> links) l))


-- | This is the function objects need to provide.
--
-- The \"Show\" instance is used in error messages.  The Typeable instance
-- is needed to get the key out and compare it with other keys for the
-- same object type in other views.
data MergeLinks object = forall key . (Ord key,Show key,Typeable key)
   => MergeLinks (View -> Link object -> IO (ObjectLinks key))


emptyMergeLinks :: MergeLinks object
emptyMergeLinks
   = MergeLinks (\ _ _ -> return ((ObjectLinks []) :: ObjectLinks ()))

singletonMergeLinks
   :: HasCodedValue object => (object -> WrappedMergeLink) -> MergeLinks object
singletonMergeLinks toWrappedMergeLink =
   let
      fn view link =
         do
            object <- readLink view link
            return (ObjectLinks [(toWrappedMergeLink object,())])
   in
      MergeLinks fn


-- pairMergeLinks
pairMergeLinks :: MergeLinks object -> MergeLinks object -> MergeLinks object
pairMergeLinks (MergeLinks fn1) (MergeLinks fn2) =
   let
      fn view link =
         do
            (objectLinks1 @ (ObjectLinks list1)) <- fn1 view link
            (objectLinks2 @ (ObjectLinks list2)) <- fn2 view link
            let
               list =
                     (map (\ (wrappedLink,key1) -> (wrappedLink,Left key1))
                        list1)
                  ++ (map (\ (wrappedLink,key2) -> (wrappedLink,Right key2))
                        list2)
            return (ObjectLinks list)
   in
      MergeLinks fn

-- | This contains the reassignments made in merging, mapping each link to
-- its corresponding link in the final merge.
data LinkReAssigner = LinkReAssigner {
   linkMap :: Map.Map (ViewId,WrappedMergeLink) WrappedMergeLink,
      -- ^ maps each link to its corresponding link in the final merge

   allMergesMap :: Map.Map WrappedMergeLink [(View,WrappedMergeLink)]
      -- ^ allMergesMap is the inverse map, mapping each link in the
      -- final merge to the corresponding original links.
   }


class HasCodedValue object => HasMerging object where
   getMergeLinks :: MergeLinks object
      -- Retuns those links which need to be preserved by merging.

   attemptMerge :: LinkReAssigner -> View -> Link object
      -> [(View,Link object,object)] -> IO (WithError ())
      -- Attempt to merge the links supplied in the last argument to produce
      -- a single object in (View,Link object), or return an error message.

   attemptMergeWithPostMerge :: LinkReAssigner -> View -> Link object
      -> [(View,Link object,object)] -> IO (WithError PostMerge)
      -- a more general version of attemptMerge, where the merge may return
      -- a PostMerge object, containing additional things (currently just an
      -- action) to be done after all the objects in the view have been merged.

   attemptMergeWithPostMerge linkReAssigner view link vlos =
      do
         unitWE <- attemptMerge linkReAssigner view link vlos
         return (mapWithError
            (\ () -> newPostMerge (return ()))
            unitWE
            )

   copyObject :: Maybe (View -> object -> View
      -> (ObjectVersion -> IO VersionInfo) -> IO object)
      -- Operation performed during session management, when we need to
      -- recreate an object.  We take the original view, old object,
      -- new view as arguments and lookup function, and return a new version
      -- of the object.
      --
      -- The lookup function passed in maps from an object version in the
      -- source repository to the corresponding VersionInfo (which may not
      -- yet have actually been committed) in the new repository.
      --
      -- When Nothing this means that the new copy can just be exactly the
      -- same (byte-identical) as the old one, which saves the
      -- session-management code a bit of work.  However we envisage this
      -- as being necessary for versioned objects which include an
      -- ObjectVersion, since the ObjectVersion needs to be renumbered.
   copyObject = Nothing


newtype PostMerge = PostMerge (IO ()) -- abstract type

newPostMerge :: IO () -> PostMerge
newPostMerge action = PostMerge action


doPostMerge :: PostMerge -> IO ()
doPostMerge (PostMerge action) = action

data WrappedMergeLink = forall object .
   (HasCodedValue object,HasMerging object) => WrappedMergeLink (Link object)

-- | Returns Nothing if the types don\'t match.
unpackWrappedMergeLink :: HasMerging object =>
    WrappedMergeLink -> Maybe (Link object)
unpackWrappedMergeLink (WrappedMergeLink link) = fromDynamic (toDyn link)


instance Eq WrappedMergeLink where
   (==) (WrappedMergeLink link1) (WrappedMergeLink link2) = eqLink link1 link2

-- | NB NB.  MergeComputeParents uses a hack which assumes that the
-- ordering only depends on the link location.
instance Ord WrappedMergeLink where
   compare (WrappedMergeLink link1) (WrappedMergeLink link2)
      = compareLink link1 link2

instance HasKey WrappedMergeLink Location where
   toKey (WrappedMergeLink link) = toKey link

mapLink :: HasMerging object => LinkReAssigner -> View -> Link object
   -> Link object
mapLink linkReAssigner oldView oldLink =
   case Map.lookup (viewId oldView,WrappedMergeLink oldLink)
         (linkMap linkReAssigner) of
      Nothing -> error "MergeTypes.mapLink - unmapped type"
      Just wrappedMergeLink -> case unpackWrappedMergeLink wrappedMergeLink of
         Just newLink -> newLink
         Nothing -> error "MergeTypes.mapLink - unexpected type error"
