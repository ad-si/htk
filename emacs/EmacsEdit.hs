{- This module edits Emacs content, including "magic buttons" to allow
   the user expand bits of content further. -}
module EmacsEdit(
   editEmacs, -- :: EmacsFS -> String -> IO ()
   EmacsFS(..),
   EditedFile(..),

   TypedName,
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
-- 
-- We include along with the name a single character which corresponds to
-- a type designation.  It should be one of the characters listed in
-- allmmiss.el's variable MMiSS-colours, so currently one of G/U/A/T.
-- NB.  We assume that no two TypedName's exist with the same String but
-- different Chars.
type TypedName = (String,Char)

data EmacsFS = EmacsFS {
   -- | editFS name
   -- attempts to edit the file name.
   -- It returns the initial contents and the file's EditedFile structure.
   editFS :: TypedName -> IO (WithError (EmacsContent TypedName,EditedFile)),
   -- | existsFS name
   -- checks that the file exists and can be read (without trying to edit it)
   existsFS :: TypedName -> IO (WithError ())
   }

---
-- EditedFile (provided by the caller) describes a file as it is edited by
-- this module
data EditedFile = EditedFile {
   writeData :: EmacsContent TypedName -> IO (WithError ()),
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
editEmacs :: EmacsFS -> TypedName -> IO ()
editEmacs emacsFS name =
   do
      let
         -- action for opening the Emacs window, after openFile has managed
         -- to open the file.
         parentAction =
            do
               -- (1) Initialise Emacs and MMiSS-TeX.
               emacsSession <- newEmacsSession (describe name)
               execEmacs emacsSession "MMiSS-init"
               setColourHack emacsSession

               -- (2) Construct the container.
               let
                  parent = normalName name

               addContainerBuffer emacsSession parent

               -- (3) Construct the EditorState structure
               openFiles <- newRegistry
               let
                  editorState = EditorState {
                     emacsSession = emacsSession,
                     emacsFS = emacsFS,
                     openFiles = openFiles
                     }
               return (parent,editorState)

      editorStateOpt <- openFile emacsFS parentAction name

      case editorStateOpt of
         Just editorState -> 
             -- (5) Handle the Emacs events, until the user quits.
            sync (handleEvents editorState)
         Nothing -> done

-- ----------------------------------------------------------------------
-- Open a new file and insert it in a container
-- ----------------------------------------------------------------------

---
-- openFile attempts to open the file "name".
--
-- The parentAction should return the identifier of the parent window
-- and is executed after we have successed in getting the file
-- from the repository.  It also returns the EditorState to use,
-- which openFile in turn returns.
--
-- If we do not succeed, openFile returns Nothing.
openFile :: EmacsFS -> IO (String,EditorState) -> TypedName 
   -> IO (Maybe EditorState)
openFile emacsFS parentAction name =
   do
      emacsFileWE <- editFS emacsFS name
      case fromWithError emacsFileWE of
         Left message -> 
            do
               createErrorWin message []
               return Nothing
         Right (EmacsContent initialContents,emacsFile) ->
            do
               (parent,state) <- parentAction

               -- Add a new entry to the registry
               setValue (openFiles state) (key name) emacsFile

               let
                  session = emacsSession state 

               -- Insert the button
               addButton session parent (headName name) (describe name)

               -- Insert the contents
               mapM
                  (\ dataItem -> case dataItem of
                     EmacsLink child -> 
                        addButton session parent (normalName child) 
                           (describe child)
                     EditableText str ->
                        addText session parent str
                     )
                  initialContents

               -- Insert the boundary
               boundContainer session parent
               return (Just state)

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
                  (\ hContainer ->
                     do
                        let
                           container = case parseButton hContainer of
                              Normal container -> container
                              _ -> error ("EmacsEdit: Mysterious container "++
                                 hContainer)
                        fileOpt <- getValueOpt (openFiles editorState) 
                           (key container)
                        let
                           file = case fileOpt of
                              Just file -> file
                              Nothing -> error ("handleEvents: container "++
                                 describe container ++" does not exist")
                        contents0 <- containerContents session hContainer
                        let
                           contents1list = case contents0 of
                              EmacsContent (EmacsLink headButton : list)
                                 | headButton == headName container
                                 -> list
                              _ -> error ("Couldn't find head button for "++
                                  describe container)
                           contents2list = 
                              map
                                 (\ dataItem -> case dataItem of
                                    EmacsLink button -> 
                                       case parseButton button of
                                          Normal name -> EmacsLink name
                                          Head name -> error (
                                             "Unexpected head "++describe name)
                                    EditableText text -> EditableText text
                                    )
                                 contents1list
                        written <- writeData file (EmacsContent contents2list)
                        case fromWithError written of
                           Left mess -> showError ("Writing "
                              ++describe container++": "++mess)
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
                  confirm ("Expand "++describe name++"?") (
                     let
                        parentAction =
                           do
                              expand session str
                              return (str,editorState)
                     in
                        always (
                           do
                              openFile (emacsFS editorState) parentAction name
                              sync iterate
                           )
                     )
               Head name ->
                  confirm ("Collapse "++describe name++" without saving?") (
                     always (do
                        collapse session (normalName name) (describe name)
                        transformValue (openFiles editorState) (key name)
                           (\ stateOpt ->
                              do
                                 case stateOpt of
                                    Just state -> finishEdit state
                                    Nothing -> putStrLn ("Odd - "
                                       ++describe name++" already collapsed")
                                 return (Nothing,())
                              ) 
                        sync iterate
                        )
                     )
         )
      +> (do
            str <- event "QUIT"
            confirm "Exit without saving anything?" (always (do
               execEmacs session "MMiSS-delete"
               diyDestroy session
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

headName :: TypedName -> String
headName tn = 'H' : (unparseTypedName tn)

normalName :: TypedName -> String
normalName tn = 'N' : (unparseTypedName tn)

data ButtonName = Head TypedName | Normal TypedName

parseButton :: String -> ButtonName
parseButton ('H':str) = Head (parseTypedName str)
parseButton ('N':str) = Normal (parseTypedName str)
parseButton (badName) = error ("Bad name "++badName)


-- ----------------------------------------------------------------------
-- TypedName utilities
-- ----------------------------------------------------------------------

unparseTypedName :: TypedName -> String
unparseTypedName (str,t) = str ++ [t] 

parseTypedName :: String -> TypedName
parseTypedName [] = error "parseTypedName given empty String"
parseTypedName [t] = ("",t)
parseTypedName (c:cs) = 
   let 
      (str,t) = parseTypedName cs
   in
      (c:str,t)

---
-- Unique key for TypedName's.
key :: TypedName -> String
key (str,c) = str

---
-- How the user sees a TypedName
describe :: TypedName -> String
describe (str,c) = str

     