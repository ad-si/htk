{- Code for implementing the MMiSSPreamble type.
   Possible changes: at the moment there is no support for alternative
   preambles depending on variant attribute.  Should there be? -}
module MMiSSPreamble(
   MMiSSPreamble,
   MMiSSPreambleType,

   createPreamble, -- :: View -> MMiSSLatexPreamble -> IO (Link MMiSSPreamble)
   readPreamble, -- :: View -> Link MMiSSPreamble -> IO MMiSSLaTeXPreamble
--  readLinkEnvironment, -- :: View -> Link MMiSSPreamble 
-- -> IO LinkEnvironment

   ) where

import Concurrent
import IOExts

import Computation
import ExtendedPrelude
import Dynamics
import AtomString
import VariableSet
import Sources

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

import EmacsEdit
import EmacsContent

import LaTeXParser

-- -------------------------------------------------------------------
-- MMiSSPreambleType
-- -------------------------------------------------------------------

data MMiSSPreambleType = MMiSSPreambleType

mmissPreambleType_tyRep = mkTyRep "MMiSSPreamble" "MMiSSPreambleType"

instance HasTyRep MMiSSPreambleType where
   tyRep _ = mmissPreambleType_tyRep

instance HasCodedValue MMiSSPreambleType where
   encodeIO = mapEncodeIO (\ _ -> ())
   decodeIO codedValue0 view = 
      do
         ((),codedValue1) <- decodeIO codedValue0 view
         return (MMiSSPreambleType,codedValue1)

-- -------------------------------------------------------------------
-- MMiSSPreamble
-- -------------------------------------------------------------------

data MMiSSPreamble = MMiSSPreamble {
   preamble :: IORef MMiSSLatexPreamble,
   editLock :: BSem
   }

mmissPreamble_tyRep = mkTyRep "MMiSSPreamble" "MMiSSPreamble"

instance HasTyRep MMiSSPreamble where
   tyRep _ = mmissPreamble_tyRep

instance HasCodedValue MMiSSPreamble where
   encodeIO (MMiSSPreamble {preamble = preamble}) 
         codedValue view =
      do
         latexPreamble <- readIORef preamble
         encodeIO latexPreamble codedValue view
   decodeIO codedValue0 view =
      do
         (latexPreamble,codedValue1) 
            <- decodeIO codedValue0 view
         preamble <- newIORef latexPreamble
         editLock <- newBSem
         return (MMiSSPreamble {preamble = preamble,editLock = editLock},
            codedValue1)

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

   linkAsData _ = True

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
      editEmacs preambleFS printAction preamble

preambleFS :: EmacsFS MMiSSPreamble
preambleFS =
   let
      editFS (MMiSSPreamble {preamble = preamble,editLock = editLock}) =
         addFallOutWE (\ break ->
            do
               lockGot <- tryAcquire editLock
               if lockGot then done else
                  break "Preamble is already being edited"
               latexPreamble <- readIORef preamble
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
                                 writeIORef preamble latexPreamble
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
   (==) (MMiSSPreamble {preamble = preamble1}) 
      (MMiSSPreamble {preamble = preamble2}) = preamble1 == preamble2

instance Ord MMiSSPreamble where
   compare preamble1 preamble2 =
      if preamble1 == preamble2 then EQ else
         error "MMiSSPreamble: Bug 2"

-- -------------------------------------------------------------------
-- Creating and Reading Preambles
-- -------------------------------------------------------------------

createPreamble :: View -> MMiSSLatexPreamble -> IO (Link MMiSSPreamble)
createPreamble view latexPreamble =
   do
      preamble <- newIORef latexPreamble
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
      latexPreamble <- readIORef (preamble mmissPreamble)
      return latexPreamble

-- -------------------------------------------------------------------
-- readLinkEnvironment
-- -------------------------------------------------------------------

---
-- Currently returns just the . link environment
-- readLinkEnvironment :: View -> Link MMiSSPreamble -> IO LinkEnvironment

-- -------------------------------------------------------------------
-- The Global Registry.  This will in fact be empty.
-- -------------------------------------------------------------------

globalRegistry :: GlobalRegistry MMiSSPreambleType
globalRegistry = IOExts.unsafePerformIO mkGlobalRegistry
{-# NOINLINE globalRegistry #-}

mmissPreambleTypeKey :: GlobalKey
mmissPreambleTypeKey = oneOffKey "MMiSSPreamble" "MMiSSPreamble"

mkGlobalRegistry :: IO (GlobalRegistry MMiSSPreambleType)
mkGlobalRegistry = createGlobalRegistry
