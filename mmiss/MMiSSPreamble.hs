{- Code for implementing the MMiSSPreamble type.
   Possible changes: at the moment there is no support for alternative
   preambles depending on variant attribute.  Should there be? -}
module MMiSSPreamble(
   MMiSSPreamble,
   MMiSSPreambleType,

   createPreamble, -- :: View -> MMiSSLatexPreamble -> IO (Link MMiSSPreamble)
   readPreamble, -- :: View -> Link MMiSSPreamble -> IO MMiSSLaTeXPreamble
   writePreamble, -- :: Link Preamble -> View -> MMiSSLaTeXPreamble -> IO ()


   toImportCommands, -- :: MMiSSPreamble -> SimpleSource ImportCommands

   ) where

import Maybe

import Control.Concurrent
import System.IO.Unsafe

import Computation
import ExtendedPrelude
import Dynamics
import AtomString
import VariableSet
import Sources
import Broadcaster

import BSem

import DialogWin

import Graph(NodeType)
import GraphDisp
import GraphConfigure

import CodedValue
import ObjectTypes
import GlobalRegistry
import SpecialNodeActions
import Link
import View
import MergeTypes
import MergePrune
import EntityNames

import EmacsEdit
import EmacsContent

import LaTeXParser

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
   editLock :: BSem
   } deriving (Typeable)

instance HasBinary MMiSSPreamble CodingMonad where
   writeBin = mapWriteIO
      (\ (MMiSSPreamble {preamble = preamble}) ->
         do
            latexPreamble <- readContents preamble
            return latexPreamble
         )
   readBin = mapReadIO
      (\ latexPreamble ->
         do
            preamble <- newSimpleBroadcaster latexPreamble
            editLock <- newBSem
            return (MMiSSPreamble {preamble = preamble,editLock = editLock})
         )

-- -------------------------------------------------------------------
-- Merging
-- -------------------------------------------------------------------

instance HasMerging MMiSSPreamble where

   -- We allow only trivial merging, for now.
   getMergeLinks = emptyMergeLinks

   attemptMerge linkReAssigner newView newLink vlos =
      do
         vlosPruned <- mergePrune vlos
         case vlosPruned of
            a:b:_ -> return (hasError "Sorry, can't merge preambles")
            [(view,preambleLink,oldPreamble)] ->
               do
                  cloneLink view preambleLink newView newLink
                  return (hasValue ())

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
               Color "blue" $$$
               emptyNodeTypeParms
               )],
            getNodeType = const theNodeType,
            getNodeLinks = const (return emptyArcEnds),
            closeDown = done,
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
      editFS (MMiSSPreamble {preamble = preamble,editLock = editLock}) =
         addFallOutWE (\ break ->
            do
               lockGot <- tryAcquire editLock
               if lockGot then done else
                  break "Preamble is already being edited"
               latexPreamble <- readContents preamble
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
                                 broadcast preamble latexPreamble
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
   (\ _ _ -> createErrorWin "Cannot print a preamble!" [])

---
-- We need to define an ordering on MMiSSPreamble's for EditFS, but since
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

writePreamble :: Link MMiSSPreamble -> View -> MMiSSLatexPreamble -> IO ()
writePreamble preambleLink view latexPreamble =
   do
      preamble <- newSimpleBroadcaster latexPreamble
      editLock <- newBSem
      let
         mmissPreamble 
            = MMiSSPreamble {preamble = preamble,editLock = editLock}

      writeLink view preambleLink mmissPreamble

createPreamble :: View -> MMiSSLatexPreamble -> IO (Link MMiSSPreamble)
createPreamble view latexPreamble =
   do
      preamble <- newSimpleBroadcaster latexPreamble
      editLock <- newBSem
      let
         mmissPreamble 
            = MMiSSPreamble {preamble = preamble,editLock = editLock}

      preambleVers <- createObject view mmissPreamble
      link <- makeLink view preambleVers
      return link

readPreamble :: View -> Link MMiSSPreamble -> IO MMiSSLatexPreamble
readPreamble view link =
   do
      mmissPreamble <- readLink view link
      latexPreamble <- readContents (preamble mmissPreamble)
      return latexPreamble

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
