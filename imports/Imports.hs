-- |
-- Description: Lookup 'EntityNames.EntityFullName's from 'EntityNames.ImportCommands'.
-- 
-- This is the module which has the job of co-ordinating the construction of
-- Environment.Env values and their use. 
module Imports(
   ImportsState(folders),

   newImportsState,
      -- :: Ord node => FolderStructure node 
      -- -> Delayer -> IO (ImportsState node)
      -- Construct a new ImportsState.
      -- The second argument is a Delayer for preventing unnecessary error
      -- messages during node updates.

   LookupResult(..),


   lookupNodes,
      -- :: Ord node => ImportsState node -> node 
      -- -> [(EntitySearchName,value)] 
      -- -> IO (SimpleSource [(LookupResult node,value)])

   lookupNode,
      -- :: ImportsState node -> node -> EntitySearchName 
      -- -> IO (SimpleSource (LookupResult node))

   bracketForImportErrors2,
      -- :: Ord node => ImportsState node -> IO a -> IO a

--   getGlobalNodeData,GlobalNodeData(..), -- DEBUG
--   getLocalNodeData,LocalNodeData(..), -- DEBUG
   ) where

import Maybe

import Data.IORef
import Control.Concurrent.MVar

import Computation
import Registry
import Sources
import Broadcaster
import AtomString
import ExtendedPrelude
import TSem
import Delayer

import EntityNames
import ErrorReporting
import ErrorManagement
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
   errorManagementState :: ErrorManagementState node,
   delayer :: Delayer,
   errorMessageCache :: IORef String
   }

-- ------------------------------------------------------------------------
-- External Functions
-- ------------------------------------------------------------------------

newImportsState :: Ord node => FolderStructure node 
   -> Delayer -> IO (ImportsState node)
newImportsState (folders :: FolderStructure node) delayer =
   do
      globalState <- newRegistry
      localState <- newRegistry

      importsStateMVar <- newEmptyMVar
         -- we need to tie the knot.

      errorMessageCache <- newIORef ""
 
      let
         getImportsState = readMVar importsStateMVar

         mkError :: node -> String -> IO (Maybe String)
         mkError node mess0 =
           do
              name <- getName folders node
              let
                 mess1 = lines mess0
                 mess2 = map ((toString name ++ ":") ++) mess1
              return (Just (unlines mess2))


         checkErrorLocation :: ErrorLocation node -> IO (Maybe String)
         checkErrorLocation (GlobalError node) =
            do
               importsState <- getImportsState
               globalNodeDataSource <- getGlobalNodeData importsState node
               case globalNodeDataSource of
                  Nothing -> mkError node notMMiSSPackageError
                  Just source ->
                     do
                        gWE <- readContents source
                        case fromWithError gWE of
                           Left mess -> mkError node mess
                           Right _ -> return Nothing
         checkErrorLocation (LocalError node) =
            do
               importsState <- getImportsState
               localNodeDataSource <- getLocalNodeData importsState node
               case localNodeDataSource of
                  Nothing -> mkError node notMMiSSPackageError
                  Just source ->
                     do
                        lWE <- readContents source
                        case fromWithError lWE of
                           Left mess -> mkError node mess
                           Right _ -> return Nothing
         checkErrorLocation (SearchError node searchName) =
            do
               importsState <- getImportsState
               searchResultSource <- lookupNode importsState node searchName
               searchResult <- readContents searchResultSource
               case searchResult of
                  Error ->
                     do
                        errorMessage <- readIORef errorMessageCache
                        mkError node errorMessage

      errorManagementState 
         <- newErrorManagementState delayer checkErrorLocation

      let 
         importsState = ImportsState {
            folders = folders,
            globalState = globalState,
            localState = localState,
            errorManagementState = errorManagementState,
            errorMessageCache = errorMessageCache,
            delayer = delayer
            }
      putMVar importsStateMVar importsState
 
      return importsState

lookupNodes :: Ord node => ImportsState node -> node 
   -> [(EntitySearchName,value)] 
   -> IO (SimpleSource [(LookupResult node,value)])
lookupNodes importsState (node :: node) 
      (searchData :: [(EntitySearchName,value)]) =
   do
      sourceOpt <- getLocalNodeData importsState node
      let
         folderStructure = folders importsState

         mkGlobalError :: (EntitySearchName,value) 
            -> IO (SimpleSource (LookupResult node,value))
            -- resolve only FromAbsolute searches.
         mkGlobalError (FromAbsolute fullName,value) =
            do
               simpleSource <- lookupFullName folderStructure node fullName
               return (fmap
                  (\ nodeOpt -> 
                     (case nodeOpt of
                        Nothing -> NotFound
                        Just node1 -> Found node1,
                        value
                        )
                     )
                  simpleSource
                  )
         mkGlobalError (searchName,value) =
            return (staticSimpleSource (Error,value))

         getGlobalError :: IO (SimpleSource [(LookupResult node,value)])
         getGlobalError =
            do
               (globalErrors :: [SimpleSource (LookupResult node,value)])
                  <- mapM mkGlobalError searchData
               return (sequenceSimpleSource globalErrors)

         mkNonError :: Env node -> (EntitySearchName,value) ->
            IO (SimpleSource (LookupResult node,value))
         mkNonError localEnv (searchName,value) =
            case searchName of
               FromAbsolute fullName ->
                  do
                     nodeOptSource 
                        <- lookupFullName folderStructure node fullName
                     return (fmap
                        (\ nodeOpt -> 
                           (case nodeOpt of
                              Nothing -> NotFound
                              Just node1 -> Found node1,
                              value
                              )
                           ) 
                        nodeOptSource
                        )
               _ ->
                  do
                     (source1 :: SimpleSource (Maybe (Env node))) 
                        <- lookupEnv folderStructure
                           localEnv (mkNames searchName)
                     let
                        source2 
                           :: SimpleSource (LookupResult node,
                              value)
                        source2 =
                           mapIO
                              (\ envOpt ->
                                 do
                                    lookupResult <- 
                                       mkLookupResult 
                                          importsState node
                                          searchName envOpt
                                    return (lookupResult,value)
                                 )
                              source1
                     
                     return source2


      case sourceOpt of
         Nothing -> 
            do
               name <- getName (folders importsState) node
               notMMiSSPackage importsState node
               getGlobalError
         Just (source :: SimpleSource (WithError (LocalNodeData node))) ->
            return (mapIOSeq
               source
               (\ localNodeDataWE -> case fromWithError localNodeDataWE of
                  Left mess -> 
                     do
                        if mess /= reported
                           then
                              do
                                 localError importsState node mess
                                 done
                           else
                              done
                        getGlobalError
                  Right localNodeData ->
                     do
                        let
                           localEnv0 = localEnv localNodeData
                        (lookupSources0 
                           :: [SimpleSource (LookupResult node,value)])
                           <- mapM
                              (\ searchValue -> 
                                 mkNonError localEnv0 searchValue) 
                              searchData
                        let
                           lookupSources1 
                              :: SimpleSource [(LookupResult node,value)]
                           lookupSources1 = sequenceSimpleSource
                              lookupSources0

                        return lookupSources1
                  )
               )             

lookupNode :: Ord node => ImportsState node -> node -> EntitySearchName 
   -> IO (SimpleSource (LookupResult node))
   -- get value of search name within some object.
lookupNode importsState (node :: node) (FromAbsolute fullName) =
   do
      fullNameSource <- lookupFullName (folders importsState) node fullName
      return (fmap
         (\ nodeOpt -> case nodeOpt of
            Nothing -> NotFound
            Just node -> Found node
            )
         fullNameSource
         )
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
               notMMiSSPackage importsState node
               return (staticSimpleSource Error)
         Just source ->
            return (
               do
                  localNodeDataWE <- source
                  case fromWithError localNodeDataWE of
                     Left mess -> mkIOSimpleSource (
                        do
                           if mess /= reported
                              then
                                 do
                                    localError importsState node mess
                                    done
                              else
                                 done

                           return (staticSimpleSource Error)
                        )
                     Right (localNodeData :: LocalNodeData node) -> 
                        let
                           source1 :: SimpleSource (Maybe (Env node))
                           source1 =  
                              mkIOSimpleSource (lookupEnv folderStructure
                                 (localEnv localNodeData) names)

                           source2 = mapIO
                              (\ envOpt -> mkLookupResult importsState
                                    node searchName envOpt
                                 )
                              source1
                        in
                           source2
               )
            
mkLookupResult :: Ord node => ImportsState node -> node -> EntitySearchName 
   -> Maybe (Env node) -> IO (LookupResult node)
mkLookupResult importsState (parentNode :: node) searchName envOpt =
   case envOpt of
      Nothing -> return NotFound
      Just env ->
         case thisOpt env of
            Just node -> return (Found node)
            Nothing -> 
               do
                  searchError importsState parentNode searchName
                     "Name is incomplete"
                  return Error

bracketForImportErrors2 :: Ord node => ImportsState node -> IO a -> IO a
bracketForImportErrors2 importsState 
   = bracketForImportErrors1 (errorManagementState importsState)


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
               source 
                  <- globalError importsState node "attempts to import itself"
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
                           return (staticSimpleSource (fail mess))
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

      (globalSource3,closeDown) <- mirrorSimpleSource globalSource2

      return globalSource3

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
               source <- localError importsState node "getLocalNodeData bug?"
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

         localSource1 :: SimpleSource (WithError (LocalNodeData node))
         localSource1 = 
            mapIOSeq globalNodeDataSource
               (\ globalNodeDataWE ->
                  case fromWithError globalNodeDataWE of
                     Left mess -> 
                        return (staticSimpleSource (fail mess))
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
                                          localEnv = setThis localEnv node
                                          }
                                       )

                              localSource :: SimpleSource (
                                 WithError (LocalNodeData node))
                              localSource = fmap mkLocalNodeData localESource

                           return localSource
                  )
      -- we don't check for loops here, since a local import cannot
      -- include another local import.

      (localSource3,closeDown) <- mirrorSimpleSource localSource1 

      return localSource3

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
                  globalError importsState node0 ("Could not find " 
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
                        Nothing -> globalError importsState node0 
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
-- Error reporting functions
-- ------------------------------------------------------------------------

globalError :: Ord node => ImportsState node -> node -> String 
   -> IO (SimpleSource (WithError a))
globalError importsState node mess =
   do
      recordError (errorManagementState importsState) (GlobalError node)
      return (staticSimpleSource (fail mess))

notMMiSSPackage :: Ord node 
   => ImportsState node -> node -> IO (SimpleSource (WithError a))
notMMiSSPackage importsState node = globalError importsState node 
   notMMiSSPackageError

notMMiSSPackageError :: String
notMMiSSPackageError = "not an MMiSS package"

localError :: Ord node => ImportsState node -> node -> String 
   -> IO (SimpleSource (WithError a))
localError importsState node mess =
   do
      recordError (errorManagementState importsState) (LocalError node)
      return (staticSimpleSource (fail mess))

searchError :: Ord node => ImportsState node -> node -> EntitySearchName 
   -> String -> IO (SimpleSource (WithError a))
searchError importsState node searchName mess =
   do
      recordError (errorManagementState importsState) 
         (SearchError node searchName)
      let
         errorMessage = "Searching for " ++ toString searchName ++ ", " ++ mess

      writeIORef (errorMessageCache importsState) errorMessage

      return (staticSimpleSource (fail errorMessage))


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

