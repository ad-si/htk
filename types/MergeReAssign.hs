{- This module computes the MergeType.LinkReAssigner value for merging a
   list of view. 

   At the moment we use a fairly simplistic algorithm, assuming that no
   object occurs more than once as a fixed link in a view.  That is because
   we expect this to in fact be the case; the only fixed links for now will
   be top folder links and their children, and the folder structure is a tree.
   We may in future need to make this more sophisticated by allowing fixed
   links to form a more general structure.  This could be handled in this 
   module by some sort of union-find based algorithm.  But the penalty of 
   doing that would be that merges would potentially fail for reasons the user
   might find hard to cope with.
   -}
module MergeReAssign(
   mkLinkReAssigner,
   ) where

import Maybe

import Data.Set
import Data.FiniteMap
import Data.IORef

import Computation
import ExtendedPrelude
import Registry
import Dynamics


import MergeTypes
import ObjectTypes
import Link
import GlobalRegistry
import ViewType


-- -----------------------------------------------------------------
-- The ObjectNode type.
-- -----------------------------------------------------------------

---
-- There is one ObjectNode for each link we need to put in the final
-- view.
data ObjectNode object key =
   ObjectNode {
      references :: IORef [(View,Link object)],
         -- existing links which have to correspond to this object.
      links :: IORef (FiniteMap key WrappedObjectNode)
      }


data WrappedObjectNode = forall object key .
   (HasMerging object,Ord key,Typeable key)
   => WrappedObjectNode (ObjectNode object key)

objectNode_tyRep = mkTyRep "MergeReAssign" "ObjectNode"

instance HasTyRep2 ObjectNode where
   tyRep2 _ = objectNode_tyRep

wrappedObjectNode_tyRep = mkTyRep "MergeReAssign" "WrappedObjectNode"
instance HasTyRep WrappedObjectNode where
   tyRep _ = wrappedObjectNode_tyRep

toObjectNode :: (HasMerging object,Typeable key,Ord key) 
   => WrappedObjectNode -> WithError (ObjectNode object key)
toObjectNode (WrappedObjectNode (objectNode0 :: ObjectNode fromObject key1)) =
   case dynCastOpt objectNode0 of
      Just objectNode -> hasValue objectNode
      (Nothing :: Maybe (ObjectNode toObject key2)) -> 
         let
            (objFrom :: fromObject) = error "MergeReAssign.1"
            (objTo :: toObject) = error "MergeReAssign.2"
         in
            hasError ("Merge reassignment - mismatched types; cannot unify " ++
               "an object of type "++show (typeOf objFrom)++" with an " ++
               "object of type "++show (typeOf objTo))

-- -----------------------------------------------------------------
-- The on-going state.
-- -----------------------------------------------------------------

---
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
         -- (3) Construct the LinkReAssigner, also checking that no WrappedLink
         -- referred to more than once.
         -- The second argument is an accumulating parameter.
         let
            mkLinkReAssigner :: [WrappedObjectNode] 
               -> LinkReAssigner
               -> IO LinkReAssigner
            mkLinkReAssigner [] linkReAssigner = return linkReAssigner
            mkLinkReAssigner 
                  ((WrappedObjectNode ((ObjectNode {references = referencesA})
                     :: ObjectNode object key))
                     : restObjectNodes)
                  (LinkReAssigner {
                     linkMap = fmap0,
                     allMergesMap = allMergesMap0}) =
               do
                  (references0 :: [(View,Link object)]) 
                     <- readIORef referencesA
                  let
                     (_,headLink) : _ = references0
                     headWrappedMergeLink = WrappedMergeLink headLink

                  -- Generate a link in the new view to map to.  We take
                  -- headLink unless that is already occupied, in which case
                  -- we take a new link
                  newWrappedMergeLink 
                     <- case lookupFM allMergesMap0 headWrappedMergeLink of
                        Nothing -> return headWrappedMergeLink
                        Just _ ->
                           do
                              (newLink :: Link object) 
                                 <- absolutelyNewLink repository
                              return (WrappedMergeLink newLink)

                  let
                     doLinks :: [(View,Link object)] 
                        -> FiniteMap (ViewId,WrappedMergeLink) WrappedMergeLink
                        -> FiniteMap (ViewId,WrappedMergeLink) WrappedMergeLink
                     doLinks [] fmap0 = fmap0
                     doLinks ((view,link):rest) fmap0 =
                        let
                           key = (viewId view,WrappedMergeLink link)
                        in
                           case lookupFM fmap0 key of
                              Nothing -> 
                                 doLinks rest 
                                    (addToFM fmap0 key newWrappedMergeLink)
                              Just _ -> break 
                                 "Merge failure: link occurs multiple times!"

                     fmap1 = doLinks references0 fmap0

                  seq fmap1 done

                  let
                     thisMerge =
                        map 
                           (\ (view,link) -> (view,WrappedMergeLink link)) 
                           references0

                     allMergesMap1 
                        = addToFM allMergesMap0 newWrappedMergeLink thisMerge

                     linkReAssigner1 = LinkReAssigner {
                        linkMap = fmap1,
                        allMergesMap =  allMergesMap1
                        } 

                  mkLinkReAssigner restObjectNodes linkReAssigner1
 
         linkReAssigner <- mkLinkReAssigner allNodesList
            (LinkReAssigner {linkMap = emptyFM,allMergesMap = emptyFM})

         return linkReAssigner
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
   addFallOutWE (\ break ->
      let
         allRelevantObjectTypes 
            :: [(WrappedObjectTypeTypeData,GlobalKey,WrappedObjectType)]
         allRelevantObjectTypes = concat
            (map
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
         -- (3) if not already done for this WrappedMergeLink in this view
         --     (the third argument keeps track of that), expand the links,
         --     and repeat.
         -- We return the new visited set, and the ObjectNode.

         -- Thanks to GHC's restrictions on unpacking existential types,
         -- we have to split visitNode into 3 functions, innerVisitNode and
         -- innerInnerVisitNode.
         visitNode :: WrappedMergeLink -> Maybe WrappedObjectNode 
            -> Set WrappedMergeLink -> IO (WrappedObjectNode,Set WrappedMergeLink)
         visitNode (WrappedMergeLink (link0 :: Link object))
               wrappedObjectNodeOpt visitedSet0 =
            do
               (wrappedObjectNode1,visitedSet1) 
                  <- innerVisitNode () link0 wrappedObjectNodeOpt 
                     getMergeLinks visitedSet0
               return (wrappedObjectNode1,visitedSet1)

         -- Thanks to GHC's restrictions that mutually recursive functions
         -- need to have identical contexts, innerVisitNode needs to be 
         -- provided with an extra "key" argument, which is ignored.
         innerVisitNode ::
            (Typeable key,Ord key,HasMerging object) 
            => key -> Link object -> Maybe WrappedObjectNode
            -> MergeLinks object
            -> Set WrappedMergeLink 
            -> IO (WrappedObjectNode,Set WrappedMergeLink)
         innerVisitNode _ (link1 :: Link object) wrappedObjectNodeOpt
               (MergeLinks (fn :: View -> Link object 
                  -> IO (ObjectLinks key)))
               visitedSet0 =
            do
               -- (1) create an ObjectNode corresponding to this reference,
               --     if none was supplied in the second argument.
               (objectNode :: ObjectNode object key) 
                  <- case wrappedObjectNodeOpt of
                     Nothing -> 
                        do
                           objectNode <- createObjectNode 
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

               (objectNode,set) 
                  <- innerInnerVisitNode link1 fn objectNode visitedSet0
               return (WrappedObjectNode objectNode,set)

         innerInnerVisitNode :: 
            (Typeable key,Ord key,HasMerging object) 
            => Link object
            -> (View -> Link object -> IO (ObjectLinks key))
            -> ObjectNode object key
            -> Set WrappedMergeLink 
            -> IO (ObjectNode object key,Set WrappedMergeLink)
         innerInnerVisitNode (link1 :: Link object)
               (fn :: View -> Link object -> IO (ObjectLinks key)) 
               (objectNode @ (
                  ObjectNode {references = references0,links =links0}))
               visitedSet0 =
            do
               -- (2) add this reference (View+WrappedMergeLink) to the 
               --     references Field of the ObjectNode.
               modifyIORef references0 ( (view,link1) : )

               -- (3) if not already done for this WrappedMergeLink in this 
               --     view (the third argument keeps track of that), expand the
               --     links, and repeat.
               if elementOf (WrappedMergeLink link1) visitedSet0
                  then
                     return (objectNode,visitedSet0)
                  else
                     do
                        (ObjectLinks  (linksOut :: [(WrappedMergeLink,key)]))
                           <- fn view link1

                        let
                           links1 
                              :: IORef (FiniteMap key WrappedObjectNode)
                           links1 = dynCast 
                              ("MergeReAssign: object type has "
                                 ++ "inconsistent keys!")
                              links0
 
                        (fMap0 :: FiniteMap key WrappedObjectNode) 
                           <- readIORef links1
               
                        let
                           -- function which processes links1.  The 
                           -- second argument carries the state.
                           doLinks :: [(WrappedMergeLink,key)] 
                              -> (FiniteMap key WrappedObjectNode,
                                 Set WrappedMergeLink
                                 )
                              -> IO (FiniteMap key WrappedObjectNode,
                                 Set WrappedMergeLink)
                           doLinks [] state = return state
                           doLinks 
                                 ((WrappedMergeLink link,key):rest) 
                                 (fMap0,visitedSet0) =
                              do
                                 let
                                    wrappedObjectNodeOpt 
                                       = lookupFM fMap0 key
                                 (wrappedObjectNode,visitedSet1) 
                                    <- innerVisitNode () link 
                                       wrappedObjectNodeOpt getMergeLinks
                                       visitedSet0
                                 let
                                    fMap1 = case wrappedObjectNodeOpt of
                                       Nothing -> addToFM fMap0 key
                                          wrappedObjectNode
                                       Just _ -> fMap0
                                 doLinks rest (fMap1,visitedSet1)

                        (fMap1,visitedSet1) <- doLinks linksOut
                           (fMap0,visitedSet0)
                        writeIORef links1 fMap1
                        return (objectNode,visitedSet1)

         -- Create an empty ObjectNode.  The key argument is not looked
         -- at and just gives type information.
         createObjectNode :: IO (ObjectNode object key)
         createObjectNode =
            do
               references1 <- newIORef []
               links1 <- newIORef emptyFM
               let
                  newObjectNode = ObjectNode {
                     references = references1,links =links1}
               return newObjectNode

         -- Function for processing the relevant object types, visiting 
         -- their fixed links.
         doFixedLinks 
            :: [(WrappedObjectTypeTypeData,GlobalKey,WrappedObjectType)]
            -> Set WrappedMergeLink
            -> IO ()
         doFixedLinks [] visitedSet = done
         doFixedLinks 
               ((wrappedObjectTypeTypeData,key :: GlobalKey
                  ,wrappedObjectType) : rest)
               visitedSet0 =
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
                     -> [WrappedObjectNode] -> Set WrappedMergeLink
                     -> IO ([WrappedObjectNode],Set WrappedMergeLink)
                  doLinksNodes [] wrappedObjectNodes visitedSet =
                     return (reverse wrappedObjectNodes,visitedSet)
                  doLinksNodes ((wrappedLink,wrappedObjectNodeOpt):rest)
                        wrappedObjectNodes visitedSet0 =
                     do
                        (wrappedObjectNode,visitedSet1) <- visitNode 
                           wrappedLink wrappedObjectNodeOpt visitedSet0 
                        doLinksNodes rest 
                           (wrappedObjectNode:wrappedObjectNodes) 
                           visitedSet1
               
               (wrappedObjectNodes1,visitedSet1) 
                  <- doLinksNodes linksNodes [] visitedSet0

               -- Insert wrappedObjectNodes1 into state, if necessary.
               case oldNodesOpt of
                  Nothing -> setValue registry stateKey wrappedObjectNodes1
                  Just _ -> done

               doFixedLinks rest visitedSet1

      in
         -- Now do the business!
         doFixedLinks allRelevantObjectTypes emptySet  
      )