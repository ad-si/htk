{- This is the module which has the job of co-ordinating the construction of
   Environment.Env values and their use. -}
module Imports(
   ImportsState,

   newImportsState, 
      -- :: FolderStructure node -> (String -> IO ()) -> IO (ImportsState node)
      -- Construct a new ImportsState.
      -- The second argument is a function for reporting error messages.

   LookupResult(..),

   lookupNode,
      -- :: ImportsState node -> node -> EntitySearchName 
      -- -> IO (SimpleSource (LookupResult node))


   getGlobalNodeData,GlobalNodeData(..), -- DEBUG
   getLocalNodeData,LocalNodeData(..), -- DEBUG
   ) where

import Maybe

import Computation
import Registry
import Sources
import Broadcaster
import AtomString
import ExtendedPrelude
import TSem

import EntityNames
import ErrorReporting
import Aliases
import FolderStructure
import Environment


-- ------------------------------------------------------------------------
-- Datatypes
-- ------------------------------------------------------------------------

data LookupResult node =
      Found node
   |  NotFound -- node is not found, but otherwise no errors
   |  Error -- some error has occurred, and this has been reported

data GlobalNodeData node = GlobalNodeData {
   -- Information we need about a node to import it, or which might be
   -- used as a preliminary to constructing LocalNodeData.
   importCommands :: ImportCommands,
      -- import commands for this node
   aliases :: Aliases,
      -- aliases according to which links within the node are interpreted
   globalEnv :: Env node
      -- environment exported by this node
   }

data LocalNodeData node = LocalNodeData {
   -- Information we need about a node to resolve a reference with in it
   localEnv :: Env node
      -- environment used to resolve references within this node.  
   }

data ImportsState node = ImportsState {
   folders :: FolderStructure node,
   globalState 
      :: LockedRegistry node (SimpleSource (WithError (GlobalNodeData node))),
   localState
      :: LockedRegistry node (SimpleSource (WithError (LocalNodeData node))),
   reportError :: String -> IO ()
   }

-- ------------------------------------------------------------------------
-- External Functions
-- ------------------------------------------------------------------------

newImportsState :: Ord node => FolderStructure node -> (String -> IO ()) 
   -> IO (ImportsState node)
newImportsState folders reportError =
   do
      globalState <- newRegistry
      localState <- newRegistry
      return (ImportsState {
         folders = folders,
         globalState = globalState,
         localState = localState,
         reportError = reportError
         })         
               

lookupNode :: Ord node => ImportsState node -> node -> EntitySearchName 
   -> IO (SimpleSource (LookupResult node))
   -- get value of search name within some object.
lookupNode importsState (node :: node) searchName =
   do
      sourceOpt <- getLocalNodeData importsState node
      let
         names = mkNames searchName
         folderStructure = folders importsState

      case sourceOpt of
         Nothing -> 
            do
               name <- getName (folders importsState) node
               reportError importsState
                  (toString name ++ " is not an MMiSS package")
               return (staticSimpleSource Error)
         Just source ->
            return (
               do
                  localNodeDataWE <- source
                  case fromWithError localNodeDataWE of
                     Left mess -> return Error
                     Right (localNodeData :: LocalNodeData node) -> 
                        let
                           source1 :: SimpleSource (Maybe (Env node))
                           source1 =  
                              mkIOSimpleSource (lookupEnv folderStructure
                                 (localEnv localNodeData) names)

                           source2 = mapIO
                              (\ envOpt -> case envOpt of
                                 Just env -> 
                                    case thisOpt env of
                                       Just node -> return (Found node)
                                       Nothing ->
                                          do
                                             name <- getName 
                                                (folders importsState) node
                                             reportError importsState
                                                (toString name
                                                   ++ ": Name " 
                                                   ++ toString searchName
                                                   ++ " is incomplete"
                                                   )
                                             return Error
                                 Nothing -> return NotFound
                                 )
                              source1
                        in
                           source2
               )
            

-- ------------------------------------------------------------------------
-- Constructing the Global Data.
-- ------------------------------------------------------------------------

getGlobalNodeData :: Ord node => ImportsState node -> node 
   -> IO (Maybe (SimpleSource (WithError (GlobalNodeData node))))
getGlobalNodeData importsState node =
   do
      sourceChecked 
         <- lockedRegistryCheck (getGlobalNodeData1 importsState node)
      case sourceChecked of
         Right sourceOpt -> return sourceOpt
         Left _ -> 
            do
               source <- mkError importsState node "attempts to import itself"
               return (Just source)

getGlobalNodeData1 :: Ord node => ImportsState node -> node 
   -> IO (Maybe (SimpleSource (WithError (GlobalNodeData node))))
getGlobalNodeData1 (importsState :: ImportsState node) (node :: node) =
   transformValue (globalState importsState) node
      (\ (sourceOpt :: Maybe (SimpleSource (WithError (GlobalNodeData node))))
            -> 
         case sourceOpt of
            Just source -> return (sourceOpt,Just source)
               -- already constructed
            Nothing -> -- we must do some more work.
               do
                  let
                     folderStructure = folders importsState
                  importCommandsOpt <- getImportCommands folderStructure node
                  case importCommandsOpt of
                     Nothing ->
                        return (sourceOpt,Nothing)
                     Just importCommands ->
                        do
                           source <- mkGlobalNodeData importsState node 
                              importCommands
                           return (Just source,Just source)
         )
 
 
mkGlobalNodeData :: Ord node => ImportsState node -> node 
   -> SimpleSource ImportCommands 
   -> IO (SimpleSource (WithError (GlobalNodeData node)))
mkGlobalNodeData importsState (node :: node) importCommandsSource =
   do
      let
         folderStructure = folders importsState

         titleAct :: IO String
         titleAct =
            do
               fullName <- getName folderStructure node
               return (toString fullName)

      (eSourceIn :: ESource node) 
         <- newESource folderStructure node (EntityFullName [])

      tSem <- newTSem titleAct

      let
         globalSource1 :: SimpleSource (WithError (GlobalNodeData node))
         globalSource1 = 
            mapIOSeq importCommandsSource
               (\ importCommands ->
                  do
                     case fromWithError (mkAliases importCommands) of
                        Left mess -> 
                           do
                              if mess /= reported
                                 then
                                    reportError importsState mess
                                 else
                                    done
                              return (staticSimpleSource reportedError)
                        Right aliases ->
                           do
                              let
                                 ImportCommands commandList = importCommands

                                 globalImportCommands 
                                    :: [([Directive],EntitySearchName)]
                                 globalImportCommands = mapMaybe
                                    (matchImport True) commandList

                              (globalImports :: [ESource node])
                                 <- mapM 
                                    (\ (directives,searchName) ->
                                       mkNodeImport importsState node
                                          aliases directives searchName
                                       )
                                    globalImportCommands

                              let
                                 globalESource :: ESource node
                                 globalESource = unionList folderStructure
                                    (eSourceIn : globalImports)

                                 mkGlobalNodeData :: WithError (Env node)
                                    -> WithError (GlobalNodeData node)
                                 mkGlobalNodeData =
                                    mapWithError
                                       (\ globalEnv ->
                                          GlobalNodeData {
                                             importCommands = importCommands,
                                             aliases = aliases,
                                             globalEnv = globalEnv
                                             }
                                          )
                      
                                 globalSource :: SimpleSource (
                                    WithError (GlobalNodeData node))
                                 globalSource 
                                    = fmap mkGlobalNodeData globalESource

                              return globalSource
                  )

      let
         globalSource2 = noLoop tSem globalSource1

         globalSource3 = reportErrors importsState globalSource2

      (globalSource,closeDown) <- mirrorSimpleSource globalSource3

      return globalSource

-- ------------------------------------------------------------------------
-- Constructing the Local Data.
-- ------------------------------------------------------------------------

getLocalNodeData :: Ord node => ImportsState node -> node 
   -> IO (Maybe (SimpleSource (WithError (LocalNodeData node))))
getLocalNodeData importsState node =
   do
      sourceChecked 
         <- lockedRegistryCheck (getLocalNodeData1 importsState node)
      case sourceChecked of
         Right sourceOpt -> return sourceOpt
         Left _ -> 
            do
               source <- mkError importsState node "getLocalNodeData bug?"
               return (Just source)

getLocalNodeData1 :: Ord node => ImportsState node -> node 
   -> IO (Maybe (SimpleSource (WithError (LocalNodeData node))))
getLocalNodeData1 (importsState :: ImportsState node) (node :: node) =
   transformValue (localState importsState) node
      (\ (sourceOpt :: Maybe (SimpleSource (WithError (LocalNodeData node)))) 
            -> 
         case sourceOpt of
            Just source -> return (sourceOpt,Just source)
               -- already constructed
            Nothing -> -- we must do some more work.
               do
                  globalNodeDataOpt <- getGlobalNodeData importsState node
                  case globalNodeDataOpt of
                     Nothing ->
                        return (sourceOpt,Nothing)
                     Just globalNodeData ->
                        do
                           source <- mkLocalNodeData importsState node 
                              globalNodeData
                           return (Just source,Just source)
         )
 
mkLocalNodeData :: Ord node => ImportsState node -> node 
   -> SimpleSource (WithError (GlobalNodeData node))
   -> IO (SimpleSource (WithError (LocalNodeData node)))
mkLocalNodeData importsState (node :: node) globalNodeDataSource =
   do
      let
         folderStructure = folders importsState

         titleAct :: IO String
         titleAct =
            do
               fullName <- getName folderStructure node
               return (toString fullName)

      let
         localSource1 :: SimpleSource (WithError (LocalNodeData node))
         localSource1 = 
            mapIOSeq globalNodeDataSource
               (\ globalNodeDataWE ->
                  case fromWithError globalNodeDataWE of
                     Left mess -> 
                        do
                           if mess /= reported
                              then
                                 reportError importsState mess
                              else
                                 done
                           return (staticSimpleSource reportedError)
                     Right globalNodeData ->
                        do
                           let
                              ImportCommands commandList 
                                 = importCommands globalNodeData

                              localImportCommands 
                                 :: [([Directive],EntitySearchName)]
                              localImportCommands = mapMaybe
                                 (matchImport False) commandList

                           (localImports :: [ESource node])
                              <- mapM 
                                 (\ (directives,searchName) ->
                                    mkNodeImport importsState node
                                       (aliases globalNodeData) directives 
                                       searchName
                                    )
                                 localImportCommands

                           let
                              globalSource :: ESource node
                              globalSource = staticSimpleSource 
                                 (hasValue (globalEnv globalNodeData))

                              localESource :: ESource node
                              localESource = unionList folderStructure
                                 (globalSource : localImports)

                              mkLocalNodeData :: WithError (Env node)
                                 -> WithError (LocalNodeData node)
                              mkLocalNodeData =
                                 mapWithError
                                    (\ localEnv ->
                                       LocalNodeData {
                                          localEnv = localEnv
                                          }
                                       )

                              localSource :: SimpleSource (
                                 WithError (LocalNodeData node))
                              localSource = fmap mkLocalNodeData localESource

                           return localSource
                  )
      let
         -- we don't check for loops here, since a local import cannot
         -- include another local import.

         localSource3 = reportErrors importsState localSource1

      (localSource,closeDown) <- mirrorSimpleSource localSource3

      return localSource

-- ------------------------------------------------------------------------
-- Functions for interpreting import directives
-- ------------------------------------------------------------------------

matchImport :: Bool -> ImportCommand -> Maybe ([Directive],EntitySearchName)
-- If import command is global and bool is true, or import command is local
-- and bool is false, return its directives and search name.
matchImport mustBeGlobal (Import directives searchName)
      | isGlobal directives == mustBeGlobal
      = Just (directives,searchName)
   where
      isGlobal :: [Directive] -> Bool
      isGlobal directives =
         fromMaybe False
            (findJust 
               (\ directive -> case directive of
                  Global -> Just True
                  Local -> Just False
                  _ -> Nothing
                  )
               directives
               )
matchImport _ _ = Nothing

   

mkNodeImport :: Ord node => ImportsState node -> node -> Aliases
   -> [Directive] -> EntitySearchName -> IO (ESource node)
-- construct the import for one node, given the aliases
-- return True if the import is a global one.
mkNodeImport importsState (node0 :: node) aliases directives searchName1 =
   do
      let
         folderStructure = folders importsState

         searchName2 = expandAliases aliases searchName1

      (nodeSource :: SimpleSource (Maybe node)) 
         <- lookupSearchName folderStructure node0 searchName2

      let
         (eSource0 :: ESource node) = mapIOSeq
            nodeSource
            (\ nodeOpt -> case nodeOpt of
               Nothing -> 
                  mkError importsState node0 ("Could not find " 
                     ++ toString searchName2)
               Just node1 ->
                  do
                     (globalNodeDataSourceOpt 
                        :: Maybe (SimpleSource (WithError (
                           GlobalNodeData node))))
                        <- getGlobalNodeData importsState node1
                     case globalNodeDataSourceOpt of
                        Just (globalNodeDataSource :: SimpleSource ( 
                              WithError (GlobalNodeData node))) ->
                           return (fmap
                              (\ globalNodeDataWE ->
                                 mapWithError globalEnv globalNodeDataWE)
                              globalNodeDataSource
                              )
                        Nothing -> mkError importsState node0 
                           ("Target of " ++ toString searchName2 ++
                              " is not an MMiSS object")
               )

      let
         applyDirectives1 :: [Directive] -> ESource node
         applyDirectives1 [] = eSource0
         applyDirectives1 (directive : directives1) =
            let
               eSource1 = applyDirectives1 directives1
            in
               case directive of
                  Hide names -> hide folderStructure names eSource1
                  Reveal names -> reveal folderStructure names eSource1
                  Rename {newName = newName1,oldName = oldName1} 
                     -> rename folderStructure oldName1 newName1 eSource1
                  _ -> eSource1

         eSource2 = applyDirectives1 directives

         isQualified =
            fromMaybe False
               (findJust 
                  (\ directive -> case directive of
                     Qualified -> Just True
                     Unqualified -> Just False
                     _ -> Nothing
                     )
                  directives
                  )    

         prefixSearchName :: EntitySearchName -> ESource node -> ESource node
         prefixSearchName searchName eSource0 =
            foldr (\ name1 eSource -> prefix name1 eSource)
               eSource0 (mkNames searchName)

         eSource3 = prefixSearchName searchName1 eSource2

         eSource4 = 
            if isQualified 
               then 
                  eSource3 
               else
                  union folderStructure eSource2 eSource3

      return eSource4

-- ------------------------------------------------------------------------
-- Error control functions
-- ------------------------------------------------------------------------

reportErrors 
   :: ImportsState node -> SimpleSource (WithError a) 
      -> SimpleSource (WithError a)
-- This function does reports the errors on the SimpleSource, which are
-- replaced by "REPORTED".  Errors already "REPORTED" are ignored.
reportErrors importsState simpleSource =
   mapIO
      (\ aWE ->
         case fromWithError aWE of
            Left mess | mess /= reported ->
               do
                  -- Remove duplicate lines
                  let
                     mess2 = unlines . uniqOrd . lines $ mess
                  reportError importsState mess2
                  return reportedError
            _ -> return aWE
          )
      simpleSource

mkError :: ImportsState node -> node -> String 
   -> IO (SimpleSource (WithError a))
mkError importsState node mess =
   do
      name <- getName (folders importsState) node
      return (staticSimpleSource (hasError 
         (toString name ++ ": " ++ mess)))

-- ------------------------------------------------------------------------
-- Miscellanous utility functions
-- ------------------------------------------------------------------------

mkNames :: EntitySearchName -> [EntityName]
mkNames (FromHere (EntityFullName names)) = names
mkNames (FromCurrent (EntityFullName names)) = (EntityName "Current") : names
mkNames (FromRoot (EntityFullName names)) = (EntityName "Root") : names
mkNames (FromParent searchName1) = (EntityName "Parent") : mkNames searchName1

unionList :: Ord node => FolderStructure node -> [ESource node] -> ESource node
unionList folderStructure [] = emptyESource
unionList folderStructure l = foldl1 (union folderStructure) l

