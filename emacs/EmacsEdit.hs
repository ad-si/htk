{- This module edits Emacs content, including "magic buttons" to allow
   the user expand bits of content further. -}
module EmacsEdit(
   editEmacs, -- :: EmacsFS -> String -> IO ()
   EmacsFS(..),
   EditedFile(..),
   ) where

import Computation
import Registry

import Events
import Channels
import Synchronized
import Destructible

import LockEvent

import DialogWin

import EmacsBasic
import EmacsContent
import EmacsCommands
import Extents

-- ----------------------------------------------------------------------
-- What the caller needs to provide
-- ----------------------------------------------------------------------

---
-- EmacsFS describes the interface this module needs to the file system.
data EmacsFS = EmacsFS {
   -- | editFS name
   -- attempts to edit the file name.
   -- It returns the initial contents and the file's EditedFile structure.
   editFS :: String -> IO (WithError (EmacsContent String,EditedFile)),
   -- | existsFS name
   -- checks that the file exists and can be read (without trying to edit it)
   existsFS :: String -> IO (WithError ())
   }

---
-- EditedFile (provided by the caller) describes a file as it is edited by
-- this module
data EditedFile = EditedFile {
   writeData :: EmacsContent String -> IO (WithError ()),
      -- ^ Attempt to write back the edited content (this may be done more than
      --   once)
   finishEdit :: IO ()
      -- action to be called at the end (when we no longer want to edit
      -- this file).
   }

-- ----------------------------------------------------------------------
-- Other datatypes
-- ----------------------------------------------------------------------

data EditorState = EditorState {
   emacsSession :: EmacsSession,
   emacsFS :: EmacsFS,
   openFiles :: Registry String EditedFile -- ^ currently edited files
   }      

-- ----------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------

---
-- editEmacs edits a particular file, with the specified file system. 
-- This function terminates when the user finishes editing.
editEmacs :: EmacsFS -> String -> IO ()
editEmacs (emacsFS @ (EmacsFS {editFS = editFS,existsFS = existsFS})) name =
   do
      -- (1) Initialise Emacs and MMiSS-TeX.
      emacsSession <- newEmacsSession name
      execEmacs emacsSession "MMiSS-init"

      -- (2) Construct the container.
      addContainerBuffer emacsSession (normalName name) 

      -- (3) Construct the EditorState structure
      openFiles <- newRegistry
      let
         editorState = EditorState {
            emacsSession = emacsSession,
            emacsFS = emacsFS,
            openFiles = openFiles
            }

      -- (4) Open the file
      openFile editorState (normalName name) name

      -- (5) Handle the Emacs events, until the user quits.
      sync (handleEvents editorState)

-- ----------------------------------------------------------------------
-- Open a new file and insert it in a container
-- ----------------------------------------------------------------------

openFile :: EditorState -> String -> String -> IO ()
openFile state parent name =
   do
      emacsFileWE <- editFS (emacsFS state) name
      case fromWithError emacsFileWE of
         Left message -> createErrorWin message []
         Right (EmacsContent initialContents,emacsFile) ->
            do
               -- Add a new entry to the registry
               setValue (openFiles state) name emacsFile

               let
                  session = emacsSession state 

               -- Insert the button
               addButton session parent (headName name) name

               -- Insert the contents
               mapM
                  (\ dataItem -> case dataItem of
                     EmacsLink child -> 
                        addButton session parent (normalName name) name
                     EditableText str ->
                        addText session parent str
                     )
                  initialContents

               -- Insert the boundary
               boundContainer session parent

-- ----------------------------------------------------------------------
-- The event handler
-- ----------------------------------------------------------------------

handleEvents :: EditorState -> Event ()
handleEvents editorState =
   let
      session = emacsSession editorState

      event :: String -> Event String 
      event key = emacsEvent session key

      iterate = handleEvents editorState

      confirm :: String -> Event () -> Event ()
      confirm str event =
         do
            goAhead <- always (createConfirmWin str [])
            if goAhead then event else iterate

      showError :: String -> IO ()
      showError str = createErrorWin str []

   in
         (do
            str <- event "COMMIT"
            confirm ("Commit?") (always (do
               containers <- listContainers session
               mapM_
                  (\ container ->
                     do
                        fileOpt 
                           <- getValueOpt (openFiles editorState) container
                        let
                           file = case fileOpt of
                              Just file -> file
                              Nothing -> error ("handleEvents: container "++
                                 container ++" does not exist")
                        contents0 <- containerContents session container
                        let
                           contents1list = case contents0 of
                              EmacsContent (EmacsLink headButton : list)
                                 | headButton == headName container
                                 -> list
                              _ -> error ("Couldn't find head button for "++
                                  container)
                           contents2list = 
                              map
                                 (\ dataItem -> case dataItem of
                                    EmacsLink button -> 
                                       case parseButton button of
                                          Normal name -> EmacsLink name
                                          Head name -> error (
                                             "Unexpected head "++name)
                                    _ -> dataItem
                                    )
                                 contents1list
                        written <- writeData file (EmacsContent contents2list)
                        case fromWithError written of
                           Left mess -> showError ("Writing "++container++": "
                              ++mess)
                           Right () -> done
                     )
                  containers
               sync iterate
              ))
         )
      +> (do
            str <- event "BUTTON"
            case parseButton str of
               Normal name ->
                  confirm ("Expand "++name++"?") (
                     always (do
                        expand session str
                        openFile editorState str name
                        sync iterate                        
                        )
                     )
               Head name ->
                  confirm ("Collapse "++name++" without saving?") (
                     always (do
                        collapse session (normalName name) name
                        deleteFromRegistry (openFiles editorState) name
                        sync iterate
                        )
                     )
         )
      +> (do
            str <- event "QUIT"
            confirm "Exit without saving anything?" (always (do
               destroy session
               openFilesContents
                  <- listRegistryContents (openFiles editorState)
               mapM_
                  (\ (_,editedFile) -> finishEdit editedFile)
                  openFilesContents
               ))
         )
      +> (do
            str <- event "ENLARGE"
            always (showError 
               "Sorry, the Enlarge operation is currently not supported")
            iterate            
         )

-- ----------------------------------------------------------------------
-- Button names
--
-- Buttons put at the head of a container have names constructed as
--   'H'+area name
-- Other buttons/containers have names 'N'+area name.
-- ----------------------------------------------------------------------

headName :: String -> String
headName = ('H' :)

normalName :: String -> String
normalName = ('N' :)

data ButtonName = Head String | Normal String

parseButton :: String -> ButtonName
parseButton ('H':str) = Head str
parseButton ('N':str) = Normal str
parseButton (badName) = error ("Bad name "++badName)

     