-- | An (Environment node) represents a mapping from EntityFullName's into
-- node's, as represented by one preamble.
module Imports.Environment(
   Env(..), -- type of an environment DEBUG
   ESource, -- ESource node = SimpleSource (WithError (Env node))


   lookupEnv,
      -- :: Ord node => FolderStructure node -> Env node -> [EntityName]
      -- -> IO (SimpleSource (Maybe (Env node)))

   thisOpt,
      -- :: Env node -> Maybe node

   emptyESource,
      -- :: ESource node

   setThis,
      -- :: Env node -> node -> Env node
      -- Set the top node of the Env.  (This is used for all
      -- local environments)

   newESource,
      -- :: FolderStructure node -> node -> EntityFullName
      -- -> IO (ESource node)
      -- Construct the basic ESource corresponding to the contents of a node.

   union,
      -- :: Ord node => FolderStructure node -> ESource node -> ESource node
      -- -> ESource node
      -- construct the union of two environments, or complain about
      -- clashes.

   prefix,
      -- :: EntityName -> ESource node -> ESource node
      -- qualify all names with the given name


   rename,
      -- :: Ord node => FolderStructure node -> EntityFullName -> EntityName
      -- -> ESource node -> ESource node
      -- allow the EntityFullName to be abbreviated by the given name (deleting
      -- any associations for the given name).

   hide,
     -- :: Ord node => FolderStructure node -> [EntityFullName] -> ESource node
     -- -> ESource node
     -- hide all the given names (which must exist).

   reveal,
      -- :: Ord node => FolderStructure node -> [EntityFullName] -> ESource node
      -- -> ESource node
      -- hide all but the given names (which must exist).


   noLoop,
      -- :: TSem -> SimpleSource (WithError a) -> SimpleSource (WithError a)
      -- Given a SimpleSource, protect it against internally referring to
      -- itself (as can occur with circular chains of global exports).



   ) where

import Data.Maybe

import Util.DeprecatedFiniteMap

import Util.Computation
import Util.Sources
import Util.AtomString
import Util.ExtendedPrelude
import Util.TSem

import Imports.EntityNames
import Imports.FolderStructure
import Imports.ErrorReporting

-- --------------------------------------------------------------
-- The datatypes
-- --------------------------------------------------------------

-- an Env is a particular assignment of names to nodes.
data Env node =
      Leaf {
         this :: node
         }
   |  This {
         this :: node,
         contents :: FiniteMap EntityName (Env node)
         }
   |  NoThis {
         contents :: FiniteMap EntityName (Env node)
         }

-- basic type we manipulate in constructing the Environment.
type ESource node = SimpleSource (WithError (Env node))

-- --------------------------------------------------------------
-- Functions used for manipulating ESource
-- --------------------------------------------------------------

emptyESource :: ESource node
emptyESource = staticSimpleSource (hasValue (NoThis {contents = emptyFM}))


newESource :: FolderStructure node -> node -> EntityFullName
   -> IO (ESource node)
newESource (folderStructure :: FolderStructure node) node
      (fullName @ (EntityFullName names)) = newSource node names
   where
      newSource :: node -> [EntityName] -> IO (ESource node)
      newSource node [] = return (staticSimpleSource
         (hasValue (Leaf {this = node})))
      newSource node (name1 : names) =
         do
            (contents :: SimpleSource (FiniteMap EntityName node))
               <- getContentsSource folderStructure node
            return (mapIOSeq
               contents
               (\ contentsMap ->
                  case lookupFM contentsMap name1 of
                     Nothing -> return (staticSimpleSource
                        (hasError ("Object " ++ toString fullName ++
                           " not found")))
                     Just node2 -> newSource node2 names
                  )
               )


-- construct the union of two environments, or complain about
-- clashes.
union :: Ord node => FolderStructure node -> ESource node -> ESource node
   -> ESource node
union (folderStructure :: FolderStructure node) source1 source2 =
   let
      sources :: SimpleSource (WithError (Env node),WithError (Env node))
      sources = pairSimpleSources source1 source2

   in
      mapIOSeq
         sources
         (\ (envWE1,envWE2) ->
            case fromWithError (pairWithErrorCheckReported envWE1 envWE2) of
               Left mess -> return (staticSimpleSource (hasError mess))
               Right (env1,env2) -> unionEnv Nothing env1 env2
            )
   where
      -- If the Maybe EntityName is set, this is an inner union with the
      -- given EntityName, and we expect the corresponding nodes to be the
      -- same, if present.
      --
      -- Otherwise if thisNameOpt is Nothing, this means we are at the
      -- top level of the union, and we do not set the top node at all.
      -- (That means for example that if we import several packages,
      -- we do not attempt to unify the package-folder element of each
      -- package.)
      unionEnv :: Maybe EntityName -> Env node -> Env node
         -> IO (SimpleSource (WithError (Env node)))
      unionEnv thisNameOpt env1 env2 =
         case thisNameOpt of
            Just thisName ->
               case (thisOpt env1,thisOpt env2) of
                  (Just node1,Just node2) | node1 == node2 ->
                     wrap (\ contents ->
                        This {this = node1,contents = contents})
                  (Just node1,Just node2) -> err ("Name " ++ toString thisName
                     ++ " multiply defined")
                  (Just node1,Nothing) ->
                     wrap (\ contents ->
                        This {this = node1,contents = contents})
                  (Nothing,Just node2) ->
                     wrap (\ contents ->
                        This {this = node2,contents = contents})
                  (Nothing,Nothing) ->
                     wrap (\ contents -> NoThis {contents = contents})
            Nothing -> wrap (\ contents -> NoThis {contents = contents})
         where
            err :: String -> IO (SimpleSource (WithError (Env node)))
            err mess = return (staticSimpleSource (hasError mess))

            wrap :: ((FiniteMap EntityName (Env node)) -> Env node)
               -> IO (SimpleSource (WithError (Env node)))
            wrap wrapFn =
               do
                  mapWESource <- unionMap env1 env2
                  return (fmap
                     (\ mapWE ->
                        mapWithError
                           (\ map -> wrapFn map)
                           mapWE
                        )
                     mapWESource
                     )

      unionMap :: Env node -> Env node
         -> IO (SimpleSource (WithError (FiniteMap EntityName (Env node))))
      unionMap env1 env2 =
         do
            (contents1 :: SimpleSource (FiniteMap EntityName (Env node)))
               <- toContents folderStructure env1
            (contents2 :: SimpleSource (FiniteMap EntityName (Env node)))
               <- toContents folderStructure env2
            let
               contents :: SimpleSource (
                  FiniteMap EntityName (Env node),
                  FiniteMap EntityName (Env node))
               contents = pairSimpleSources contents1 contents2

               result :: SimpleSource (WithError
                  (FiniteMap EntityName (Env node)))
               result =
                  mapIOSeq
                     contents
                     (\ (map1,map2) ->
                        do
                           let
                              commonNames :: [EntityName]
                              commonNames = filter
                                 (\ name -> isJust (lookupFM map2 name))
                                 (keysFM map1)

                              map1' :: FiniteMap EntityName (Env node)
                              map1' = delListFromFM map1 commonNames

                              map2':: FiniteMap EntityName (Env node)
                              map2' = delListFromFM map2 commonNames

                              map0 :: FiniteMap EntityName (Env node)
                              map0 = plusFM map1' map2'


                           (commonSources :: [SimpleSource
                                 (WithError (EntityName,Env node))]) <-
                              mapM
                                 (\ name ->
                                    do
                                       let
                                          (Just env1) = lookupFM map1 name
                                          (Just env2) = lookupFM map2 name
                                       (source :: SimpleSource (WithError
                                          (Env node)))
                                          <- unionEnv (Just name) env1 env2
                                       return (fmap
                                          (\ envWE -> mapWithError
                                             (\ env -> (name,env))
                                             envWE
                                             )
                                          source
                                          )
                                    )
                                 commonNames

                           let
                              commonSource :: SimpleSource [
                                 (WithError (EntityName,Env node))]
                              commonSource = sequenceSimpleSource commonSources

                              result1 :: SimpleSource (WithError
                                 (FiniteMap EntityName (Env node)))
                              result1 = fmap
                                 (\ pairWEs ->
                                    let
                                       pairsWE
                                          = listWithErrorCheckReported pairWEs
                                    in
                                       mapWithError
                                          (\ pairs -> addListToFM map0 pairs)
                                          pairsWE
                                    )
                                 commonSource
                           return result1
                        )
            return result





-- qualify all names with the given name
prefix :: EntityName -> ESource node -> ESource node
prefix entityName =
   fmap . mapWithError $
      (\ env ->
         NoThis {
            contents = listToFM [(entityName,env)]
            }
         )


-- allow the EntityFullName to be abbreviated by the given name (deleting
-- any associations for the given name).
rename :: Ord node => FolderStructure node -> EntityFullName -> EntityName
   -> ESource node -> ESource node
rename (folderStructure :: FolderStructure node) fullName name source =
   mapIOSeqWE
      source
      (\ env0 ->
         do
            (contentsSource :: SimpleSource (FiniteMap EntityName (Env node)))
               <- toContents folderStructure env0
            return (mapIOSeq
               contentsSource
               (\ contents0 ->
                  do
                     let
                        env1 = setContents env0 contents0
                     (lookedUp :: SimpleSource (Maybe (Env node)))
                        <- lookupEntityFullName folderStructure env1 fullName
                     return (fmap
                        (\ env2Opt -> case env2Opt of
                           Nothing -> hasError ("rename: "
                              ++ toString fullName ++ " not found")
                           Just env2 ->
                              let
                                 contents1 = addToFM contents0 name env2

                                 env3 = setContents env1 contents1
                              in
                                 hasValue env3
                           )
                        lookedUp
                        )
                   )
               )
         )

-- hide all the given names (which must exist).
hide :: Ord node => FolderStructure node -> [EntityFullName] -> ESource node
   -> ESource node
hide (folderStructure :: FolderStructure node) fullNames source =
   case treeOpt of
      Nothing -> staticSimpleSource (hasError "Can't hide whole of a module")
      Just tree ->
         mapIOSeqWE
            source
            (\ env -> hideEnv tree env)
   where
      treeOpt = structureNames fullNames

      hideEnv :: EntityNameTree -> Env node
         -> IO (SimpleSource (WithError (Env node)))
      hideEnv (EntityNameTree hideMap) env0 =
         do
            let
               hideList :: [(EntityName,Maybe EntityNameTree)]
               hideList = fmToList hideMap

            (contentsSource :: SimpleSource (FiniteMap EntityName (Env node)))
               <- toContents folderStructure env0
            return (mapIOSeq
               contentsSource
               (\ contentsMap ->
                  do
                     (whatToHide1 :: [SimpleSource (WithError (EntityName,
                           Maybe (Env node)))]) <- mapM
                        (\ (name,treeOpt) ->
                           case lookupFM contentsMap name of
                              Nothing ->
                                 return (staticSimpleSource (hasError (
                                    "hide failed: " ++ toString name
                                    ++ " not found")))
                              Just env1 -> case treeOpt of
                                 Nothing -> return (staticSimpleSource (
                                    hasValue (name,Nothing)))
                                 Just tree1 ->
                                    do
                                       (source1 :: SimpleSource (WithError
                                          (Env node)))
                                          <- hideEnv tree1 env1
                                       let
                                          source2 = fmap
                                             (\ envWE -> mapWithError
                                                (\ env -> (name,Just env))
                                                envWE
                                                )
                                             source1
                                       return source2
                           )
                        hideList
                     let
                        whatToHide2 :: SimpleSource [WithError (EntityName,
                           Maybe (Env node))]
                        whatToHide2 = sequenceSimpleSource whatToHide1

                        whatToHide3 :: SimpleSource (WithError [(EntityName,
                           Maybe (Env node))])
                        whatToHide3
                           = fmap listWithErrorCheckReported whatToHide2

                        hidden :: SimpleSource (WithError (Env node))
                        hidden = fmap
                           (\ hideListWE ->
                              mapWithError
                                 (\ hideList ->
                                    let
                                       contents =
                                          foldl
                                             (\ map0 (name,envOpt) ->
                                                case envOpt of
                                                   Nothing ->
                                                      delFromFM map0 name
                                                   Just env ->
                                                      addToFM map0 name env
                                                )
                                             contentsMap
                                             hideList
                                    in
                                       setContents env0 contents
                                    )
                              hideListWE
                              )
                           whatToHide3
                     return hidden
                  )
               )

-- hide all but the given names (which must exist).
reveal :: Ord node => FolderStructure node -> [EntityFullName] -> ESource node
   -> ESource node
reveal (folderStructure :: FolderStructure node) fullNames source =
   case treeOpt of
      Nothing -> staticSimpleSource (hasError "Can't reveal whole of a module")
      Just tree ->
         mapIOSeqWE
            source
            (\ env -> revealEnv tree env)
   where
      treeOpt :: Maybe EntityNameTree
      treeOpt = structureNames fullNames

      revealEnv :: EntityNameTree -> Env node
         -> IO (SimpleSource (WithError (Env node)))
      revealEnv (EntityNameTree revealMap) env0 =
         do
            let
               revealList :: [(EntityName,Maybe EntityNameTree)]
               revealList = fmToList revealMap

            (contentsSource :: SimpleSource (FiniteMap EntityName (Env node)))
               <- toContents folderStructure env0
            return (mapIOSeq
               contentsSource
               (\ contentsMap ->
                  do
                     (whatToReveal1 :: [SimpleSource (WithError (EntityName,
                           Maybe (Env node)))]) <- mapM
                        (\ (name,treeOpt) ->
                           case lookupFM contentsMap name of
                              Nothing ->
                                 return (staticSimpleSource (hasError (
                                    "reveal failed: " ++ toString name
                                    ++ " not found")))
                              Just env1 -> case treeOpt of
                                 Nothing -> return (staticSimpleSource (
                                    hasValue (name,Nothing)))
                                 Just tree1 ->
                                    do
                                       (source1 :: SimpleSource (WithError
                                          (Env node)))
                                          <- revealEnv tree1 env1
                                       let
                                          source2 = fmap
                                             (\ envWE -> mapWithError
                                                (\ env -> (name,Just env))
                                                envWE
                                                )
                                             source1
                                       return source2
                           )
                        revealList
                     let
                        whatToReveal2 :: SimpleSource [WithError (EntityName,
                           Maybe (Env node))]
                        whatToReveal2 = sequenceSimpleSource whatToReveal1

                        whatToReveal3 :: SimpleSource (WithError [(EntityName,
                           Maybe (Env node))])
                        whatToReveal3
                           = fmap listWithErrorCheckReported whatToReveal2

                        revealed :: SimpleSource (WithError (Env node))
                        revealed = fmap
                           (\ revealListWE ->
                              mapWithError
                                 (\ (revealList
                                       :: [(EntityName,Maybe (Env node))]) ->
                                    let
                                       contents :: FiniteMap EntityName
                                          (Env node)
                                       contents =
                                          listToFM
                                             (map
                                                (\ (name,envOpt) ->
                                                   (name,
                                                      case (envOpt,
                                                         lookupFM contentsMap
                                                            name) of
                                                         (Just env,_) -> env
                                                         (Nothing,Just env) ->
                                                            env
                                                      )
                                                   )
                                                revealList
                                                )
                                    in
                                       setContents env0 contents
                                    )
                                 revealListWE
                              )
                           whatToReveal3
                     return revealed
                  )
               )

noLoop :: TSem -> SimpleSource (WithError a) -> SimpleSource (WithError a)
   -- Given an SimpleSource, protect it against internally referring to itself
   -- (as can occur with circular chains of global exports).
noLoop tSem source =
   noLoopSimpleSource
      tSem
      (\ titles ->
         hasError (
            "Illegal circular chain of global imports: " ++
            unsplitByChar ' ' titles
            )
         )
      source


-- --------------------------------------------------------------
-- (Used in hide/reveal) Structuring collections of EntityFullNames
-- as a tree.  Shorter prefixes always override longer ones, so
-- in (hide A,hide A.B), the hide A.B is simply ignored.
--
-- It is an error if we attempt to hide the whole tree and we return
-- Nothing.
-- --------------------------------------------------------------

newtype EntityNameTree =
   EntityNameTree (FiniteMap EntityName (Maybe EntityNameTree))

structureNames :: [EntityFullName] -> Maybe EntityNameTree
structureNames fullNames =
      if nameEmpty
         then
            Nothing
         else
            Just (foldl addName emptyTree fullNames)
   where
      nameEmpty = any (\ (EntityFullName name) -> null name) fullNames

      emptyTree = EntityNameTree emptyFM

      addName :: EntityNameTree -> EntityFullName -> EntityNameTree
      addName (EntityNameTree map0) (EntityFullName (name:names)) =
         case names of
            [] -> EntityNameTree (addToFM map0 name Nothing)
            _ ->
               let
                  fullName1 = EntityFullName names

                  tree1Opt = case lookupFM map0 name of
                     Nothing -> Just (addName emptyTree fullName1)
                     Just Nothing -> Nothing
                     Just (Just tree0) -> Just (addName tree0 fullName1)
               in
                  EntityNameTree (addToFM map0 name tree1Opt)

-- --------------------------------------------------------------
-- Utility functions
-- --------------------------------------------------------------

mapIOSeqWE :: SimpleSource (WithError a)
   -> (a -> IO (SimpleSource (WithError b))) -> SimpleSource (WithError b)
mapIOSeqWE source1 getSource2 =
   mapIOSeq
      source1
      (\ aWE -> case fromWithError aWE of
         Left mess -> return (staticSimpleSource (hasError mess))
         Right a -> getSource2 a
         )

lookupEntityFullName :: Ord node => FolderStructure node -> Env node
   -> EntityFullName -> IO (SimpleSource (Maybe (Env node)))
lookupEntityFullName folderStructure env (EntityFullName names)
   = lookupEnv folderStructure env names

lookupEnv :: Ord node => FolderStructure node -> Env node -> [EntityName]
   -> IO (SimpleSource (Maybe (Env node)))
lookupEnv (folderStructure :: FolderStructure node) env names
      = lookup1 env names
   where
      lookup1 :: Env node -> [EntityName]
         -> IO (SimpleSource (Maybe (Env node)))
      lookup1 env [] = return (staticSimpleSource (Just env))
      lookup1 env (name:names) =
        do
           contents <- toContents folderStructure env
           return (mapIOSeq
              contents
              (\ map0 ->
                 case lookupFM map0 name of
                    Nothing -> return (staticSimpleSource Nothing)
                    Just env -> lookup1 env names
                 )
              )


thisOpt :: Env node -> Maybe node
thisOpt (Leaf {this = this}) = Just this
thisOpt (This {this = this}) = Just this
thisOpt (NoThis {}) = Nothing

-- Set the top node of the Env.  (This is used for all
-- local environments)
setThis:: Env node -> node -> Env node
setThis env this1 =
   case contentsOpt env of
      Nothing -> Leaf {this = this1}
      Just contents1 -> This {this = this1,contents = contents1}

contentsOpt :: Env node -> Maybe (FiniteMap EntityName (Env node))
contentsOpt (Leaf {}) = Nothing
contentsOpt (This {contents = contents}) = Just contents
contentsOpt (NoThis {contents = contents}) = Just contents

toContents :: FolderStructure node -> Env node
   -> IO (SimpleSource (FiniteMap EntityName (Env node)))
toContents folderStructure env =
   case env of
      This {contents = contents} -> wrapContents contents
      NoThis {contents = contents} -> wrapContents contents
      Leaf {this = this} ->
         do
            source <- getContentsSource folderStructure this
            return (fmap
               (\ map0 ->
                  mapFM
                     (\ _ node -> Leaf {this = node})
                     map0
                  )
               source
               )
   where
      wrapContents contents = return (staticSimpleSource contents)

setContents :: Env node -> FiniteMap EntityName (Env node) -> Env node
setContents env contents =
   case thisOpt env of
      Nothing -> NoThis contents
      Just this -> This {this = this,contents = contents}
