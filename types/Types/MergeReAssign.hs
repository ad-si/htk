-- | This module computes the MergeType.LinkReAssigner value for merging a
-- list of view.
--
-- At the moment we use a fairly simplistic algorithm, assuming that no
-- object occurs more than once as a fixed link in a view.  That is because
-- we expect this to in fact be the case; the only fixed links for now will
-- be top folder links and their children, and the folder structure is a tree.
-- We may in future need to make this more sophisticated by allowing fixed
-- links to form a more general structure.  This could be handled in this
-- module by some sort of union-find based algorithm.  But the penalty of
-- doing that would be that merges would potentially fail for reasons the user
-- might find hard to cope with.
module Types.MergeReAssign(
   mkLinkReAssigner,
   ) where

import Data.Maybe
import Control.Monad

import qualified Data.Map as Map
import Data.IORef
import qualified Data.Set as Set

import Util.ExtendedPrelude
import Util.Registry
import Util.Dynamics
import Util.VariableSet (toKey)
import Util.Debug(debugString)
import Util.Sources(readContents)

import Util.VisitedSet
import Util.Thread
import Util.UnionFind as Union

import Types.MergeTypes
import Types.ObjectTypes
import Types.Link
import Types.GlobalRegistry
import Types.ViewType


-- -----------------------------------------------------------------
-- The ObjectNode type.
-- -----------------------------------------------------------------

-- | There is one ObjectNode for each link we need to put in the final
-- view.
data ObjectNode object key =
   ObjectNode {
      references :: IORef [(View,Link object)],
         -- existing links which have to correspond to this object.
      links :: IORef (Map.Map key WrappedObjectNode),
      pathHere :: [String] -- path to this node
      } deriving (Typeable)


data WrappedObjectNode = forall object key .
   (HasMerging object,Ord key,Typeable key)
   => WrappedObjectNode (ObjectNode object key)
   deriving (Typeable)

toObjectNode :: forall object key . (HasMerging object,Typeable key,Ord key)
   => WrappedObjectNode -> WithError (ObjectNode object key)
toObjectNode (WrappedObjectNode (objectNode0 :: ObjectNode fromObject key1)) =
   case dynCastOpt objectNode0 of
      Just objectNode -> hasValue objectNode
      (Nothing :: Maybe (ObjectNode object key)) ->
         let
            (objFrom :: fromObject) = error "MergeReAssign.1"
            (objTo :: object) = error "MergeReAssign.2"
         in
            hasError ("Merge reassignment - mismatched types; cannot unify " ++
               "an object of type "++show (typeOf objFrom)++" with an " ++
               "object of type "++show (typeOf objTo))

-- -----------------------------------------------------------------
-- The on-going state.
-- -----------------------------------------------------------------

data State
   = State {
   registry
      :: Registry (WrappedObjectTypeTypeData,GlobalKey) [WrappedObjectNode],
      -- This lists the top nodes for the object type given, as an
      -- object-type-type and global key.
   allNodes :: IORef [WrappedObjectNode]
      -- Absolutely all WrappedObjectNodes.
   }

-- -----------------------------------------------------------------
-- The functions
-- -----------------------------------------------------------------

type NodeData = (ViewId,WrappedMergeLink,[String])
   -- type abbreviation we use during mkLinkReAssigner.

mkLinkReAssigner ::
   [View]
   -> [(WrappedObjectTypeTypeData,[(GlobalKey,[(View,WrappedObjectType)])])]
   -> IO (WithError LinkReAssigner)
mkLinkReAssigner views allRelevantObjectTypes =
   addFallOutWE (\ break ->
      do
         -- (1) create the state
         state <- newState

         -- (2) merge all views
         mapM_
            (\ view ->
               do
                  unitWE <- assignView view state allRelevantObjectTypes
                  coerceWithErrorOrBreakIO break unitWE
               )
            views

         allNodesList <- readIORef (allNodes state)

         -- (3) Get the list of all the identifications we need to make
         let
            addObjectNode :: [WrappedObjectNode]
               -> [[NodeData]] -> IO [[NodeData]]
            addObjectNode [] identifications = return identifications
            addObjectNode
                  ((WrappedObjectNode ((ObjectNode {references = referencesA,
                        pathHere = pathHereA})
                     :: ObjectNode object key))
                     : restObjectNodes)
                  identifications0 =
               do
                  (references0 :: [(View,Link object)])
                     <- readIORef referencesA

                  -- Determine what link to map the references to.  If some are
                  -- already mapped, these must already be mapped to the same
                  -- thing (otherwise error), and that will be what we use.
                  -- Otherwise we simply take the first link, unless that
                  -- is already in use, in which case we generate a totally
                  -- new link.

                  -- In the long term we should probably use the union-find
                  -- algorithm here.
                  let
                     references1 :: [(ViewId,WrappedMergeLink,[String])]
                     references1 = fmap
                        (\ (view,link)
                           -> (viewId view,WrappedMergeLink link,pathHereA)
                           )
                        references0

                     identifications1 = references1 : identifications0

                  addObjectNode restObjectNodes identifications1

         (identifications0 :: [[NodeData]]) <- addObjectNode allNodesList []
         let
            allNodes :: [NodeData]
            allNodes = concat identifications0

            toKey :: NodeData -> (ViewId,WrappedMergeLink)
            toKey (viewId,wml,_) = (viewId,wml)

         (unionFinds :: Map.Map (ViewId,WrappedMergeLink)
            (UnionFind NodeData)) <- foldM
               (\ fm0 nodeData ->
                  do
                     let
                        key = toKey nodeData
                     if Map.member key fm0
                        then
                           return fm0 -- already done
                        else
                           do
                              unionFind <- newElement nodeData
                              return (Map.insert key unionFind fm0)
                  )
               Map.empty
               allNodes

         -- apply identifications
         mapM_
            (\ nds ->
               do
                  let
                     (uf : ufs) = fmap
                        (\ nd -> Map.findWithDefault
                           (error "MergeReassign.1") (toKey nd) unionFinds
                           )
                        nds
                  mapM_ (\ uf1 -> Union.union uf uf1) ufs
               )
            identifications0

         -- construct list of all identifications, using a set to keep track
         -- of what has already been included.
         let
            mkIdentifications :: [[NodeData]]
               -> Set.Set (ViewId,WrappedMergeLink)
               -> [[NodeData]] -> IO [[NodeData]]
            mkIdentifications oldIdentifications visited newIdentifications =
               case oldIdentifications of
                  [] -> return newIdentifications
                  (nd:_):oldIdentifications1 ->
                     let
                        key = toKey nd
                     in
                        if Set.member key visited
                           then -- done this one
                              mkIdentifications oldIdentifications1 visited
                                 newIdentifications
                           else
                              do
                                 let
                                    (Just uf) = Map.lookup key unionFinds
                                    visited1 = Set.insert key visited
                                 sameElements1 <- sameElements uf
                                 let
                                    newIdentifications1 =
                                       (fmap toValue sameElements1)
                                          : newIdentifications
                                 mkIdentifications oldIdentifications1
                                    visited1 newIdentifications1

         identifications1 <- mkIdentifications identifications0 Set.empty []

         -- Check for link clashes, IE two links in the same view being
         -- identified.
         let
            -- check a single list
            clashWE :: [NodeData] -> IO (WithError ())
            clashWE nodes = clash nodes Map.empty
               where
                  clash :: [NodeData] -> Map.Map ViewId [String]
                     -> IO (WithError ())
                  clash [] _ = return (hasValue ())
                  clash ((nd@(viewId,_,path)):nds) map0 =
                     do
                        case Map.lookup viewId map0 of
                           Just path2 ->
                              do
                                 viewString <- viewIdToString viewId
                                 return (hasError (
                                    "Merging would force identification of "
                                       ++ show path ++ " with " ++ show path2
                                       ++ " in the view " ++ viewString))
                           Nothing ->
                              let
                                 map1 = Map.insert viewId path map0
                              in
                                 clash nds map1


                  viewIdToString :: ViewId -> IO String
                  viewIdToString viewId0 =
                     readContents (getViewTitleSource (toView viewId0))

            -- Function for getting a View from a ViewId, which we need
            -- now (and also later)
            toView :: ViewId -> View
            toView viewId0 =
               case findJust
                  (\ view ->
                     if viewId view == viewId0
                        then
                           Just view
                        else
                           Nothing
                     ) views of
                  Just view -> view
                  Nothing -> break "MergeReAssign.UNKNOWN VIEW"

         (clashes :: [WithError ()]) <- mapM clashWE identifications1

         coerceWithErrorOrBreakIO break (concatWithError clashes)

         --
         -- We have now constructed the necessary identifications, as a list
         -- of disjoint lists of nodes.  We now have to assign to each element
         -- of this list an appropriate new link.
         --
         -- We do this in two passes over identifications1.  We thread
         -- the LinkReAssigner being constructed as we do this.
         let
            (headView : _) = views
            headViewId = viewId headView
            -- headView is special, because it will be the parent version
            -- in the merged view.  This means that all links to
            -- headView must map to themselves.

            linkReAssigner0 =
               LinkReAssigner {linkMap = Map.empty,allMergesMap = Map.empty}

            -- Add an assignment of one identified list to a particular
            -- wrapped merge link.
            addIdentifications ::  WrappedMergeLink -> [NodeData]
               -> LinkReAssigner -> LinkReAssigner
            addIdentifications newLink [] linkReAssigner = linkReAssigner
            addIdentifications newLink (nd : nds)
               (linkReAssigner0 @ (LinkReAssigner {
                     linkMap = linkMap0,allMergesMap = allMergesMap0})) =
                  let
                     (viewId,wml,_) = nd

                     ndPair1 = (viewId,wml)
                     ndPair2 = (toView viewId,wml)

                     linkMap1 = Map.insert ndPair1 newLink linkMap0

                     ndPairList0 = Map.findWithDefault [] newLink allMergesMap0
                     allMergesMap1 = Map.insert newLink
                        (ndPair2 : ndPairList0) allMergesMap0

                     linkReAssigner1 = LinkReAssigner {
                        linkMap = linkMap1,
                        allMergesMap = allMergesMap1}
                  in
                     addIdentifications newLink nds linkReAssigner1

            addHeadIdentifications
               :: [[NodeData]] -> LinkReAssigner -> LinkReAssigner
            addHeadIdentifications [] linkReAssigner = linkReAssigner
            addHeadIdentifications (nds : ndss) linkReAssigner0 =
               let
                  linkReAssigner1 =
                     case findJust
                        (\ (viewId,wml,_) ->
                           if viewId == headViewId
                              then
                                 Just wml
                              else
                                 Nothing
                           )
                        nds of

                        Just headWML ->
                           addIdentifications headWML nds linkReAssigner0
                        Nothing -> linkReAssigner0
               in
                  addHeadIdentifications ndss linkReAssigner1


            addRemainingIdentifications :: [[NodeData]] -> LinkReAssigner
               -> IO LinkReAssigner
            addRemainingIdentifications [] linkReAssigner0
               = return linkReAssigner0
            addRemainingIdentifications (nds:ndss) linkReAssigner0 =
               do
                  let
                     (nd @ (_,wml0,_) : _) = nds
                  linkReAssigner1 <-
                        if Map.member (toKey nd) (linkMap linkReAssigner0)
                     then
                        -- we've done this one
                        return linkReAssigner0
                     else
                        do
                           -- generate a link to use, of appropriate type.
                           let
                              -- this takes the first link as argument,
                              -- and uses it if possible.
                              genLink :: HasMerging object => Link object
                                 -> IO (Link object)
                              genLink link0 = if
                                    Map.member (WrappedMergeLink link0)
                                       (allMergesMap linkReAssigner0)
                                 then
                                    -- can't use link0 as it's already
                                    -- taken.
                                    absolutelyNewLink repository
                                 else
                                    return link0

                              genWrappedLink :: WrappedMergeLink
                                 -> IO WrappedMergeLink
                              genWrappedLink (WrappedMergeLink link0) =
                                 do
                                    link1 <- genLink link0
                                    return (WrappedMergeLink link1)
                           wml1 <- genWrappedLink wml0
                           return (addIdentifications wml1 nds linkReAssigner0)
                  addRemainingIdentifications ndss linkReAssigner1

         let
            linkReAssigner1
               = addHeadIdentifications identifications1 linkReAssigner0
         linkReAssigner2
            <- addRemainingIdentifications identifications1 linkReAssigner1

         debugString (debugLinkReAssigner linkReAssigner2)
         return linkReAssigner2
      )
   where
      (View {repository = repository} : _ ) = views


newState :: IO State
newState =
   do
      registry <- newRegistry
      allNodes <- newIORef []

      return (State {registry = registry,allNodes = allNodes})

assignView :: View -> State
   -> [(WrappedObjectTypeTypeData,[(GlobalKey,[(View,WrappedObjectType)])])]
   -> IO (WithError ())
assignView view (State {registry = registry,allNodes = allNodes})
      allObjectTypes =
 do
   -- visitedSet keeps track of what nodes in this view have already
   -- been visited.
   (visitedSet :: VisitedSet WrappedMergeLink) <- newVisitedSet


   addFallOutWE (\ break ->
      let
         allRelevantObjectTypes
            :: [(WrappedObjectTypeTypeData,GlobalKey,WrappedObjectType)]
         allRelevantObjectTypes = concat
            (fmap
               (\ (wrappedObjectTypeTypeData,globalKeys) ->
                  mapMaybe
                     (\ (globalKey,uses) ->
                        findJust
                           (\ (view2,wrappedObjectType) ->
                              if viewId view2 == viewId view
                                 then
                                    Just (wrappedObjectTypeTypeData,
                                       globalKey,wrappedObjectType)
                                 else
                                    Nothing
                              )
                           uses
                        )
                     globalKeys
                  )
               allObjectTypes
               )


         -- The following function has three jobs to do.
         -- (1) create an ObjectNode corresponding to this reference,
         --     if none was supplied in the second argument.
         -- (2) add this reference (View+WrappedMergeLink) to the references
         --     Field of the ObjectNode.
         -- (3) if not already done for this WrappedMergeLink in this view,
         --     (the third argument keeps track of that), expand the links,
         --     and repeat.
         -- We return the ObjectNode.

         -- Thanks to GHC's restrictions on unpacking existential types,
         -- we have to split visitNode into 3 functions, innerVisitNode and
         -- innerInnerVisitNode.
         --
         -- All these functions take a first argument of type [String].
         -- This is a description of how we got here, using the Show
         -- representations of keys.

         visitNode :: [String] -> WrappedMergeLink -> Maybe WrappedObjectNode
            -> IO WrappedObjectNode
         visitNode pathHere0 (WrappedMergeLink (link0 :: Link object))
               wrappedObjectNodeOpt =
            do
               wrappedObjectNode1
                  <- innerVisitNode pathHere0 () link0 wrappedObjectNodeOpt
                     getMergeLinks
               return wrappedObjectNode1

         innerVisitNode ::
            (Typeable key,Ord key,Show key,HasMerging object)
            => [String] -> key -> Link object -> Maybe WrappedObjectNode
            -> MergeLinks object
            -> IO WrappedObjectNode
         innerVisitNode pathHere0 key (link1 :: Link object)
               wrappedObjectNodeOpt
               (MergeLinks (fn :: View -> Link object
                  -> IO (ObjectLinks key))) =
            do
               -- (1) create an ObjectNode corresponding to this reference,
               --     if none was supplied in the second argument.
               (objectNode :: ObjectNode object key)
                  <- case wrappedObjectNodeOpt of
                     Nothing ->
                        do
                           objectNode <- createObjectNode pathHere0
                           modifyIORef allNodes
                              ((WrappedObjectNode objectNode) :)
                           return objectNode
                     Just wrappedObjectNode ->
                        do
                           let
                              objectNodeWE = toObjectNode wrappedObjectNode
                           objectNode <- coerceWithErrorOrBreakIO break
                              objectNodeWE
                           return objectNode

               objectNode <- innerInnerVisitNode
                  pathHere0 link1 fn objectNode
               return (WrappedObjectNode objectNode)

         innerInnerVisitNode ::
            (Typeable key,Ord key,Show key,HasMerging object)
            => [String] -> Link object
            -> (View -> Link object -> IO (ObjectLinks key))
            -> ObjectNode object key
            -> IO (ObjectNode object key)
         innerInnerVisitNode pathHere0 (link1 :: Link object)
               (fn :: View -> Link object -> IO (ObjectLinks key))
               (objectNode @ (
                  ObjectNode {references = references0,links =links0}))
               =
            do
               -- (2) add this reference (View+WrappedMergeLink) to the
               --     references Field of the ObjectNode.
               modifyIORef references0 ( (view,link1) : )

               -- (3) if not already done for this WrappedMergeLink in this
               --     view (the third argument keeps track of that), expand the
               --     links, and repeat.
               visited <- isVisited visitedSet (WrappedMergeLink link1)
               if visited
                  then
                     return objectNode
                  else
                     do
                        (ObjectLinks  (linksOut :: [(WrappedMergeLink,key)]))
                           <- fn view link1

                        let
                           links1
                              :: IORef (Map.Map key WrappedObjectNode)
                           links1 = dynCast
                              ("MergeReAssign: object type has "
                                 ++ "inconsistent keys!")
                              links0

                        (fMap0 :: Map.Map key WrappedObjectNode)
                           <- readIORef links1

                        let
                           -- This function processes a single link, given
                           -- the existing Map.Map of outgoing arcs.
                           --
                           -- It returns a possible (key,WrappedObjectNode)
                           -- to add to the map.
                           doLink :: (WrappedMergeLink,key)
                              -> IO (Maybe (key,WrappedObjectNode))
                           doLink (WrappedMergeLink link,key) =
                              do
                                 let
                                    wrappedObjectNodeOpt
                                       = Map.lookup key fMap0

                                    pathHere1 = show key : pathHere0
                                 wrappedObjectNode
                                    <- innerVisitNode pathHere1 () link
                                       wrappedObjectNodeOpt getMergeLinks
                                 return (case wrappedObjectNodeOpt of
                                    Nothing -> Just (key,wrappedObjectNode)
                                    Just _ -> Nothing
                                    )
                        (newLinks :: [Maybe (key,WrappedObjectNode)])
                           <- mapMConcurrent doLink linksOut
                        let
                           fMap1 = foldr (uncurry Map.insert)
                              fMap0 (catMaybes newLinks)

                        writeIORef links1 fMap1
                        return objectNode

         -- Create an empty ObjectNode.
         createObjectNode :: [String] -> IO (ObjectNode object key)
         createObjectNode pathHere0 =
            do
               references1 <- newIORef []
               links1 <- newIORef Map.empty
               let
                  newObjectNode = ObjectNode {
                     references = references1,links =links1,
                        pathHere = pathHere0
                        }
               return newObjectNode

         -- Function for processing the relevant object types, visiting
         -- their fixed links.
         doFixedLinks
            :: [(WrappedObjectTypeTypeData,GlobalKey,WrappedObjectType)]
            -> IO ()
         doFixedLinks [] = done
         doFixedLinks
               ((wrappedObjectTypeTypeData,key :: GlobalKey
                  ,wrappedObjectType) : rest)
               =
            do
               -- (1) get fixed links
               fixedLinks0 <- fixedLinks view wrappedObjectType

               -- (2) get existing WrappedObjectNode's, if any.
               let
                  stateKey = (wrappedObjectTypeTypeData,key)

               (oldNodesOpt :: Maybe [WrappedObjectNode])
                  <- getValueOpt registry stateKey

               -- (3) pair the two together
               let
                  linksNodes :: [(WrappedMergeLink,Maybe WrappedObjectNode)]
                  linksNodes = case oldNodesOpt of
                     Nothing -> [ (fixedLink,Nothing) |
                        fixedLink <- fixedLinks0 ]
                     Just oldNodes ->
                        if length oldNodes /= length fixedLinks0
                           then
                              break ("MergeReAssign: fixedLinks for the "
                                 ++ "same type have inconsistent lengths")
                           else
                              zipWith
                                 (\ fixedLink oldNode
                                    -> (fixedLink,Just oldNode))
                                 fixedLinks0 oldNodes
               seq linksNodes done

               let
                  -- This is the function which processes linksNodes
                  -- It returns the corresponding object nodes, and the
                  -- new visited set.  The former is constructed in
                  -- reversed order in the second argument.
                  doLinksNodes :: [(WrappedMergeLink,Maybe WrappedObjectNode)]
                     -> [WrappedObjectNode]
                     -> IO [WrappedObjectNode]
                  doLinksNodes [] wrappedObjectNodes =
                     return (reverse wrappedObjectNodes)
                  doLinksNodes ((wrappedLink,wrappedObjectNodeOpt):rest)
                        wrappedObjectNodes =
                     do
                        wrappedObjectNode <- visitNode []
                           wrappedLink wrappedObjectNodeOpt
                        doLinksNodes rest
                           (wrappedObjectNode:wrappedObjectNodes)

               wrappedObjectNodes1 <- doLinksNodes linksNodes []

               -- Insert wrappedObjectNodes1 into state, if necessary.
               case oldNodesOpt of
                  Nothing -> setValue registry stateKey wrappedObjectNodes1
                  Just _ -> done

               doFixedLinks rest

      in
         -- Now do the business!
         doFixedLinks allRelevantObjectTypes
      )

-- ------------------------------------------------------------------
-- Functions for debugging
-- ------------------------------------------------------------------

debugLinkReAssigner :: LinkReAssigner -> String
debugLinkReAssigner linkReAssign =
   let
      linkMap1 :: Map.Map (ViewId,WrappedMergeLink) WrappedMergeLink
      linkMap1 = linkMap linkReAssign

      linkMap2 :: [((ViewId,WrappedMergeLink),WrappedMergeLink)]
      linkMap2 = Map.toList linkMap1

      linkMap3 :: [((ViewId,String),String)]
      linkMap3 = fmap
         (\ ((vi,wml1),wml2) -> ((vi,debugWML wml1),debugWML wml2))
         linkMap2

      allMergesMap1 :: Map.Map WrappedMergeLink [(View,WrappedMergeLink)]
      allMergesMap1 = allMergesMap linkReAssign

      allMergesMap2 :: [(WrappedMergeLink,[(View,WrappedMergeLink)])]
      allMergesMap2 = Map.toList allMergesMap1

      allMergesMap3 :: [(String,[(ViewId,String)])]
      allMergesMap3 = fmap
         (\ (wml,vwmls) -> (debugWML wml,
            (fmap (\ (v,wml2) -> (viewId v,debugWML wml2)) vwmls)))
         allMergesMap2
   in
      show (linkMap3,allMergesMap3)


debugWML :: WrappedMergeLink -> String
debugWML (WrappedMergeLink link) = debugLink link

debugLink :: Link x -> String
debugLink = show . toKey

debugView :: View -> String
debugView = show . viewId
