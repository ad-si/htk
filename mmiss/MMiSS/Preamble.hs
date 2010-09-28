-- | Code for implementing the MMiSSPreamble type.
-- Possible changes: at the moment there is no support for alternative
-- preambles depending on variant attribute.  Should there be?
module MMiSS.Preamble(
   MMiSSPreamble,
   MMiSSPreambleType,

   createPreamble,
      -- :: View -> WrappedLink -> MMiSSLatexPreamble
      -- -> IO (Link MMiSSPreamble)
   readPreamble, -- :: View -> Link MMiSSPreamble -> IO MMiSSLaTeXPreamble
   writePreamble,
      -- :: Link MMiSSPreamble -> View -> MMiSSLaTeXPreamble -> IO ()

   readOntology, -- :: View -> Link MMiSSPreamble -> IO MMiSSOntology
   writeOntology, -- :: Link MMiSSPreamble -> View -> MMiSSOntologyFlat -> IO ()

   readImportedByList, -- :: View -> Link MMiSSPreamble -> IO [EntityFullName]
   writeImportedByList, -- :: Link MMiSSPreamble -> View -> [EntityFullName] -> IO ()

   toImportCommands, -- :: MMiSSPreamble -> SimpleSource ImportCommands

   ) where

import Maybe

import System.IO.Unsafe

import Util.Computation
import Util.ExtendedPrelude
import Util.Dynamics
import Util.AtomString
import Util.Sources
import Util.Broadcaster
import Util.Messages

import Reactor.BSem

--import Graph(NodeType)
import Graphs.GraphDisp
import Graphs.GraphConfigure

import Types.CodedValue
import Types.ObjectTypes
import Types.GlobalRegistry
import Types.SpecialNodeActions
import Types.Link
import Types.View
import Types.MergeTypes
import Types.MergePrune
import Types.DisplayParms(fontStyleSource)

import Imports.EntityNames

import Types.LinkManager (bracketForImportErrors)

import Emacs.Edit
import Emacs.Content

import MMiSS.LaTeX.Preamble

import MMiSS.ImportExportErrors
import MMiSS.Bundle as Bundle
import MMiSS.BundleSimpleUtils
import MMiSS.BundleNodeWriteClass
import MMiSS.OntologyStore

-- -------------------------------------------------------------------
-- MMiSSPreambleType
-- -------------------------------------------------------------------

data MMiSSPreambleType = MMiSSPreambleType deriving (Typeable)

instance Monad m => HasBinary MMiSSPreambleType m where
   writeBin = mapWrite (\ MMiSSPreambleType -> ())
   readBin = mapRead (\ () -> MMiSSPreambleType)

-- -------------------------------------------------------------------
-- MMiSSPreamble
-- -------------------------------------------------------------------

data MMiSSPreamble = MMiSSPreamble {
   preamble :: SimpleBroadcaster MMiSSLatexPreamble,
   ontology :: SimpleBroadcaster MMiSSOntologyFlat,
   importedBy :: [EntityFullName],
   editLock :: BSem
   } deriving (Typeable)

instance HasBinary MMiSSPreamble CodingMonad where
   writeBin = mapWriteIO
      (\ (MMiSSPreamble {preamble = preamble, ontology = ontology, importedBy = importedBy}) ->
         do
            latexPreamble <- readContents preamble
            mmissOntology <- readContents ontology
            let importedByPackages = importedBy
            return (latexPreamble,mmissOntology,importedByPackages)
         )
   readBin = mapReadIO createPreamble1

-- -------------------------------------------------------------------
-- Merging
-- -------------------------------------------------------------------

instance HasMerging MMiSSPreamble where
   getMergeLinks = emptyMergeLinks

   attemptMerge linkReAssigner newView newLink vlos =
      do
         vlosPruned <- mergePrune vlos
         let
            (headView,headLink,_):_ = vlosPruned

         (preambles :: [MMiSSLatexPreamble]) <-
            mapM
               (\ (_,_,preamble0) -> readContents (preamble preamble0))
               vlosPruned

         let
            (mergedPreamble,errors) = mergePreambles preambles

         case errors of
            _:_ -> return (fail (unlines errors))
            [] ->
               do
                  if mergedPreamble == head preambles
                     then
                        cloneLink headView headLink newView newLink
                     else
                        do
-- TODO: Merging of Ontologies and importedByLists!!!! :
                           preamble1 <- createPreamble1 (mergedPreamble, emptyMMiSSOntologyFlat,[])
                           setLink newView preamble1 newLink
                           done
                  return (return ())


-- -------------------------------------------------------------------
-- The instance of ObjectType.
-- -------------------------------------------------------------------

instance ObjectType MMiSSPreambleType MMiSSPreamble where
   objectTypeTypeIdPrim _ = "MMiSSPreamble"
   objectTypeIdPrim _ = mmissPreambleTypeKey
   objectTypeGlobalRegistry _ = globalRegistry
   getObjectTypePrim preamble = MMiSSPreambleType
   extraObjectTypes = return [MMiSSPreambleType]
   createObjectTypeMenuItemPrim _ = Nothing
   createObjectMenuItemPrim _ = Nothing
   toLinkedObjectOpt _ = Nothing
   nodeTitlePrim _ = "PREAMBLE"

   getNodeDisplayData view _ MMiSSPreambleType _ =
      let
         (theNodeType :: NodeType) = fromString ""

         editOptions = [
            Button "Edit Preamble" (\ link
               -> editPreamble view link)
            ]

         menu = LocalMenu (Menu (Just "Preamble options") editOptions)

         nodeDisplayData = NodeDisplayData {
            topLinks = [],
            arcTypes = [],
            nodeTypes = [(theNodeType,
               menu $$$
               ValueTitle (\ _ -> return "PREAMBLE") $$$
               fontStyleSource view $$$
               Color "blue" $$$
               emptyNodeTypeParms
               )],
            getNodeType = const theNodeType,
            getNodeLinks = const (return emptyArcEnds),
            specialNodeActions = const emptyNodeActions
            }
      in
         return (Just nodeDisplayData)

-- -------------------------------------------------------------------
-- Editing the Preamble
--
-- We do this via EmacsEdit, providing a rather trivial file system.
-- -------------------------------------------------------------------

editPreamble :: View -> Link MMiSSPreamble -> IO ()
editPreamble view link =
   do
      preamble <- readLink view link
      let
         preambleFS = mkPreambleFS view link

      editEmacs preambleFS printAction preamble

mkPreambleFS :: View -> Link MMiSSPreamble -> EmacsFS MMiSSPreamble
mkPreambleFS view link =
   let
      editFS (MMiSSPreamble {preamble = preamble, ontology = ontology, editLock = editLock}) =
         addFallOutWE (\ break ->
            do
               lockGot <- tryAcquire editLock
               if lockGot then done else
                  break "Preamble is already being edited"
               latexPreamble <- readContents preamble
               mmissOntology <- readContents ontology
               let
                  latexPreambleStr = toString latexPreamble
                  emacsContent = EmacsContent [EditableText latexPreambleStr]
                  writeData (EmacsContent [EditableText latexPreambleStr]) =
                     do
                        let
                           latexPreambleWE = fromStringWE latexPreambleStr
                        mapWithErrorIO
                           (\ latexPreamble ->
                              do
                                 bracketForImportErrors view (
                                    broadcast preamble latexPreamble)
                                 broadcast ontology mmissOntology
                                 dirtyLink view link
                                 return Nothing
                              )
                           latexPreambleWE
                  writeData _ = error "MMiSSPreamble.bug 1"
                  finishEdit = release editLock
               return (emacsContent,EditedFile {
                  writeData = writeData,finishEdit = finishEdit})

            )

      toMiniType _ = 'B'
      toDescription _ = "Preamble"
      createRef _ = return (hasError "You can't import into a preamble!")
   in
      EmacsFS {editFS = editFS,toMiniType = toMiniType,
         toDescription = toDescription,createRef = createRef}

printAction :: PrintAction MMiSSPreamble
printAction = PrintAction
   (\ _ _ -> errorMess "Cannot print a preamble!")

-- | We need to define an ordering on MMiSSPreamble\'s for EditFS, but since
-- that only ever sees one preamble at a time we can afford to make it trivial.
instance Eq MMiSSPreamble where
   (==) = mapEq editLock

instance Ord MMiSSPreamble where
   compare preamble1 preamble2 =
      if preamble1 == preamble2 then EQ else
         error "MMiSSPreamble: Bug 2"

-- -------------------------------------------------------------------
-- Creating and Reading Preambles
-- -------------------------------------------------------------------

writePreamble :: Link MMiSSPreamble -> View -> MMiSSLatexPreamble -> MMiSSOntologyFlat
                   -> IO ()
writePreamble preambleLink view latexPreamble mmissOntology =
   do
      isNew <- isEmptyLink view preambleLink
      if isNew
         then
            do
               mmissPreamble <- createPreamble1 (latexPreamble, mmissOntology, [])
               writeLink view preambleLink mmissPreamble
         else
            do
               oldPreamble <- readLink view preambleLink
               oldLaTeXPreamble <- readContents (preamble oldPreamble)
               oldMMiSSOntology <- readContents (ontology oldPreamble)
               if (oldLaTeXPreamble /= latexPreamble) || (oldMMiSSOntology /= mmissOntology)
                  then
                     do
                        broadcast (preamble oldPreamble) latexPreamble
                        broadcast (ontology oldPreamble) mmissOntology
                        dirtyLink view preambleLink
                  else
                     done


writeOntology :: Link MMiSSPreamble -> View -> MMiSSOntologyFlat -> IO ()

writeOntology preambleLink view onto =
   do
      isNew <- isEmptyLink view preambleLink
      if isNew
         then return()
         else
            do
               oldPreamble <- readLink view preambleLink
               oldMMiSSOntology <- readContents (ontology oldPreamble)
               broadcast (ontology oldPreamble) onto
               dirtyLink view preambleLink



writeImportedByList :: Link MMiSSPreamble -> View -> [EntityFullName] -> IO ()

writeImportedByList preambleLink view list =
   do
      isNew <- isEmptyLink view preambleLink
      if isNew
         then return()
         else
            do
               oldPreamble <- readLink view preambleLink
               let oldLock = editLock oldPreamble
                   newPreamble = MMiSSPreamble {preamble = preamble oldPreamble,
                                                ontology = ontology oldPreamble,
                                                importedBy = list,
                                                editLock = oldLock}
               writeLink view preambleLink newPreamble



createPreamble :: View -> WrappedLink -> MMiSSLatexPreamble
   -> IO (Link MMiSSPreamble)
createPreamble view (WrappedLink parentLink) latexPreamble =
   do
      mmissPreamble <- createPreamble1 (latexPreamble, emptyMMiSSOntologyFlat, [])
      preambleVers <- createObject view parentLink mmissPreamble
      return (makeLink preambleVers)

createPreamble1 :: (MMiSSLatexPreamble, MMiSSOntologyFlat, [EntityFullName]) -> IO MMiSSPreamble
createPreamble1 (mmissLatexPreamble, mmissOntology, importedByList) =
   do
      preamble <- newSimpleBroadcaster mmissLatexPreamble
      ontology <- newSimpleBroadcaster mmissOntology
      editLock <- newBSem
      return (MMiSSPreamble {preamble = preamble, ontology = ontology, importedBy = importedByList, editLock = editLock})

readPreamble :: View -> Link MMiSSPreamble -> IO MMiSSLatexPreamble
readPreamble view link =
   do
      mmissPreamble <- readLink view link
      latexPreamble <- readContents (preamble mmissPreamble)
      return latexPreamble

readOntology :: View -> Link MMiSSPreamble -> IO MMiSSOntology
readOntology view link =
   do
      mmissPreamble <- readLink view link
      ontologyFlat <- readContents (ontology mmissPreamble)
      return (fromFlat ontologyFlat)

readImportedByList :: View -> Link MMiSSPreamble -> IO [EntityFullName]
readImportedByList view link =
   do
      mmissPreamble <- readLink view link
      return (importedBy mmissPreamble)


instance HasBundleNodeWrite MMiSSPreamble where
   bundleNodeWrite view bundleNode preambleLink =
      do
         let
            Bundle.Object [(_,preambleText)] = bundleNodeData bundleNode

         preamble <- coerceWithErrorOrBreakIO importExportError
               (fromBundleTextWE preambleText)
         writePreamble preambleLink view preamble emptyMMiSSOntologyFlat


-- -------------------------------------------------------------------
-- Interface needed for MMiSSPackageFolder
-- -------------------------------------------------------------------

toImportCommands :: MMiSSPreamble -> SimpleSource ImportCommands
toImportCommands mmissPreamble =
   fmap
      (\ mmissLaTeXPreamble -> fromMaybe trivialImportCommands
         (importCommands mmissLaTeXPreamble))
      (toSimpleSource (preamble mmissPreamble))


-- -------------------------------------------------------------------
-- The Global Registry.  This will in fact be empty.
-- -------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSPreambleType
globalRegistry = unsafePerformIO mkGlobalRegistry
{-# NOINLINE globalRegistry #-}

mmissPreambleTypeKey :: GlobalKey
mmissPreambleTypeKey = oneOffKey "MMiSSPreamble" "MMiSSPreamble"

mkGlobalRegistry :: IO (GlobalRegistry MMiSSPreambleType)
mkGlobalRegistry = createGlobalRegistry
