{- This module edits Emacs content, including "magic buttons" to allow
   the user expand bits of content further. -}
module EmacsEdit(
   editEmacs, -- :: EmacsFS -> String -> IO ()
   EmacsFS(..),
   EditedFile(..),
   PrintAction(..),

   TypedName,
   ) where

import Maybe
import Monad

import Computation
import Registry
import NameMangle
import ExtendedPrelude

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
--
-- It is also assumed that editFS will stop the same file being opened if
-- it is already open (without it being closed by finishEdit).
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

---
-- The PrintAction, supplied by the caller, prints the given TypedName 
-- as displayed in the buffer, which should be included in the buffer.  To 
-- do this it in turn is provided with a function which given a particular 
-- String returns what the Emacs buffer
-- currently contains for that String.  Included items are given as 
-- EmacsLink; the Bool indicates whether the included item is expanded (True)
-- or not.  
newtype PrintAction = PrintAction 
   (String -> (String -> IO (WithError (EmacsContent (Bool,TypedName))))
      -> IO ()
      )

-- ----------------------------------------------------------------------
-- Other datatypes
-- ----------------------------------------------------------------------

data EditorState = EditorState {
   emacsSession :: EmacsSession,
   emacsFS :: EmacsFS,
   openFiles :: Registry String EditedFile, -- ^ currently edited files
      -- (the names are not mangled)
   typedNameMangler :: TypedNameMangler,
      -- This makes all names seen by Emacs unique.
   printAction :: PrintAction,
   topMangledName :: MangledTypedName
   }      

-- ----------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------

---
-- editEmacs edits a particular file, with the specified file system. 
-- This function terminates when the user finishes editing.
editEmacs :: EmacsFS -> PrintAction -> TypedName -> IO ()
editEmacs emacsFS printAction name =
   do
      typedNameMangler <- newTypedNameMangler
      mangledName <- newMangledTypedName typedNameMangler name

      let
         -- action for opening the Emacs window, after openFile has managed
         -- to open the file.
         parentAction =
            do
               -- (1) Initialise Emacs and MMiSS-TeX.
               emacsSession <- newEmacsSession (describe name)
               execEmacs emacsSession "MMiSS-init"
               lockBuffer emacsSession
               setColourHack emacsSession

               -- (2) Construct the container.
               let
                  parent = normalName mangledName

               addContainerBuffer emacsSession parent

               -- (3) Construct the EditorState structure
               openFiles <- newRegistry
               let
                  editorState = EditorState {
                     emacsSession = emacsSession,
                     emacsFS = emacsFS,
                     openFiles = openFiles,
                     typedNameMangler = typedNameMangler,
                     printAction = printAction,
                     topMangledName = mangledName
                     }
               return (parent,editorState)

      editorStateOpt <- openFile emacsFS parentAction name mangledName

      case editorStateOpt of
         Just editorState ->
            do
               unlockBuffer (emacsSession editorState)
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
openFile :: EmacsFS -> IO (String,EditorState) -> TypedName -> MangledTypedName
   -> IO (Maybe EditorState)
openFile emacsFS parentAction name mangledName =
   do
      emacsFileWE <- editFS emacsFS name
      case fromWithError emacsFileWE of
         Left message -> 
            do
               createErrorWin message []
               return Nothing
         Right (emacsContent,emacsFile) ->
            do
               let
                  (EmacsContent initialContents) 
                     = collapseEmacsContent emacsContent
               (parent,state) <- parentAction

               -- Add a new entry to the registry
               setValue (openFiles state) (key name) emacsFile

               let
                  session = emacsSession state

                  (headString,endString) = containerTexts name 

               -- Insert the button
               addHeadButton session parent (headName mangledName) headString

               -- Insert the contents
               mapM
                  (\ dataItem -> case dataItem of
                     EmacsLink child -> 
                        do
                           mangledChild <- newMangledTypedName 
                              (typedNameMangler state) child
                           addButton session parent (normalName mangledChild) 
                              (buttonText child)
                     EditableText str ->
                        addText session parent str
                     )
                  initialContents

               -- Insert the end text
               addUneditable session parent endString

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

      nameMangler = typedNameMangler editorState
      readMangled = readMangledTypedName nameMangler

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

      getModified =
         do
            containers <- listContainers session
            filterM (isModified session) containers
   in
         (do
            str <- event "COMMIT"
            confirm ("Commit?") (always (do
               lockBuffer session
               containers <- getModified
               mapM_
                  (\ hContainer ->
                     do
                        let
                           mangledContainer = case parseButton hContainer of
                              Normal mangledContainer -> mangledContainer
                              _ -> error ("EmacsEdit: Mysterious container "++
                                 hContainer)
                        container <- readMangled mangledContainer
                        fileOpt <- getValueOpt (openFiles editorState) 
                           (key container)
                        let
                           file = case fileOpt of
                              Just file -> file
                              Nothing -> error ("handleEvents: container "++
                                 describe container ++" does not exist")
                        mangledContents 
                           <- extractContents editorState mangledContainer 
                        (unmangledContents,_) 
                           <- unmangleContents editorState mangledContents 
                        written <- writeData file 
                           (fmap 
                              (\ (b,typedName) -> typedName) 
                              unmangledContents
                              )
                        case fromWithError written of
                           Left mess -> showError ("Writing "
                              ++describe container++": "++mess)
                           Right () -> 
                              -- clear modified flag for container.
                              unmodify session hContainer
                     )
                  containers

               case containers of
                  [] -> createWarningWin "Nothing to commit!" []
                  _ -> done

               unlockBuffer session
               sync iterate
               ))
         )
      +> (do
            str <- event "PRINT"
            -- we don't CONFIRM here as it is assumed the Print operation
            -- provided by the caller provides some way of cancelling.
            always (doPrint editorState (topMangledName editorState))
            iterate
            )
      +> (do
            str <- event "BUTTON"
            case parseButton str of
               Normal mangledName ->
                  do
                     name <- always (readMangled mangledName)
                     confirm ("Expand "++describe name++"?") (
                        let
                           parentAction =
                              do
                                 expand session str
                                 return (str,editorState)
                        in
                           always (
                              do
                                 lockBuffer session
                                 openFile (emacsFS editorState) parentAction 
                                    name mangledName
                                 unlockBuffer session
                                 sync iterate
                              )
                        )
               Head mangledName ->
                  always (
                     do
                        let
                           normal = normalName mangledName

                        name <- readMangled mangledName

                        lockBuffer session

                        proceed1 <-
                           do
                              modified <- isModified session normal
                              if modified
                                 then
                                    createConfirmWin ("Collapse "
                                       ++describe name++" without saving?") []
                                 else
                                    return True

                        proceed2 <-
                           if proceed1
                              then
                                 do
                                    children <- containerChildren session 
                                       normal
                                    let
                                       childContainers = mapMaybe
                                          (\ child -> case child of
                                             Button _ -> Nothing
                                             Container str -> Just str
                                             ) 
                                          children
                                    case childContainers of
                                       [] -> return True
                                       (str:_) -> case parseButton str of
                                          Normal mangledName2 ->
                                             do
                                                name2 
                                                   <- readMangled mangledName2 
                                                createErrorWin ("Collapse " 
                                                   ++ describe name2
                                                   ++ " first!") []
                                                return False
                              else
                                 return False
                        if proceed2
                           then
                              do
                                 collapse session normal (buttonText name)
                                 transformValue (openFiles editorState) 
                                       (key name)
                                    (\ stateOpt ->
                                       do
                                          case stateOpt of
                                             Just state -> finishEdit state
                                             Nothing -> putStrLn ("Odd - "
                                                ++ describe name
                                                ++ " already collapsed")
                                          return (Nothing,())
                                       ) 
                           else
                              done

                        unlockBuffer session
                        sync iterate
                        )
         )
      +> (do
            str <- event "QUIT"
            always (do
               lockBuffer session
               proceed <-
                  do
                     modified <- getModified
                     case modified of
                        [] -> return True
                        _ ->
                          createConfirmWin "Exit without saving anything?" []
               if proceed 
                  then
                     do
                        execEmacs session "MMiSS-delete"
                        diyDestroy session
                        openFilesContents
                           <- listRegistryContents (openFiles editorState)
                        mapM_
                           (\ (_,editedFile) -> finishEdit editedFile)
                           openFilesContents
                  else
                     do
                        unlockBuffer session
                        sync iterate
               )
            )
      +> (do
            str <- event "ENLARGE"
            always (showError 
               "Sorry, the Enlarge operation is currently not supported")
            iterate            
         )

-- ----------------------------------------------------------------------
-- Printing 
-- ----------------------------------------------------------------------

doPrint :: EditorState -> MangledTypedName -> IO ()
doPrint editorState mangledToEdit =
   do
      -- The main problem here is writing the function to be passed to the
      -- print action.

      -- Since the print action only supplies TypedNames we create a registry
      -- mapping the String part of the TypedNames to their corresponding
      -- MangledTypedName (which should currently be open).  Since files are
      -- only supposed to be open once (see comments to EditFS) this is not
      -- a problem.
      (openMangledNames :: Registry String MangledTypedName) <- newRegistry

      let
         session = emacsSession editorState

         readMangled = readMangledTypedName (typedNameMangler editorState)

         printFunction :: String 
            -> IO (WithError (EmacsContent (Bool,TypedName)))
         printFunction toGetStr =
            addFallOutWE (\ break ->
               do
                  mangledToGetOpt <- getValueOpt openMangledNames toGetStr
                  let
                     mangledToGet = case mangledToGetOpt of
                        Just mangledToGet -> mangledToGet
                        Nothing -> break ("EmacsEdit: couldn't find "
                           ++toGetStr)
                  seq mangledToGet done

                  mangledContents <- extractContents editorState mangledToGet
                  (unmangledContents,associations) 
                     <- unmangleContents editorState mangledContents  
                  mapM_
                     (\ (typedName,mangledTypedName) -> setValue 
                        openMangledNames (key typedName) mangledTypedName
                        )
                     associations
                  return unmangledContents
               )

         (PrintAction mkPrint) = printAction editorState

      toEdit <- readMangled mangledToEdit
      setValue openMangledNames (key toEdit) mangledToEdit

      lockBuffer session
      toEditType <- getExtentType session (normalName mangledToEdit)
      if toEditType == "container"
         then
            mkPrint (key toEdit) printFunction
         else
            createErrorWin ("Extent "++describe toEdit
               ++" is not currently open") []
      unlockBuffer session

-- ----------------------------------------------------------------------
-- Extracting the contents of a buffer
-- The head button is removed (and checked for).
-- Bools indicate that the corresponding extent is further expanded.
-- ----------------------------------------------------------------------

extractContents :: EditorState -> MangledTypedName 
   -> IO (EmacsContent (Bool,MangledTypedName))
extractContents editorState mangledToGet =
   do
      let
         extentName = normalName mangledToGet
      contents0 
         <- containerFullContents (emacsSession editorState) extentName
      let
         list = case contents0 of
            EmacsContent (EmacsLink (False,headButton) : list)
               | headButton == headName mangledToGet
               -> list                
            _ -> error ("Couldn't find head button for "++extentName)
         list2 = map
            (\ dataItem -> case dataItem of
               EditableText text -> EditableText text
               EmacsLink (b,str) ->
                  case parseButton str of
                     Head _ -> error ("Unexpected head button "++str)
                     Normal mangledName -> EmacsLink (b,mangledName)
               ) 
            list

      return (EmacsContent list2)

---
-- Unmangled the MangledTypedNames in an EmacsContent and return a list
-- of the associations where the Bool is True.
unmangleContents :: EditorState -> EmacsContent (Bool,MangledTypedName)
   -> IO (EmacsContent (Bool,TypedName),[(TypedName,MangledTypedName)])
unmangleContents editorState (EmacsContent list0) =
   do
      let
         readMangled = readMangledTypedName (typedNameMangler editorState)

         doList :: [EmacsDataItem (Bool,MangledTypedName)] ->
            IO ([EmacsDataItem (Bool,TypedName)],
               [(TypedName,MangledTypedName)])
         doList [] = return ([],[])
         doList (EditableText text : rest) =
            do
               (l1,l2) <- doList rest
               return (EditableText text : l1,l2)
         doList (EmacsLink (b,mangledTypedName) : rest) =
            do
               (l1,l2) <- doList rest
               typedName <- readMangled mangledTypedName
               let
                  l2' = if b then (typedName,mangledTypedName):l2 else l2
               return (EmacsLink (b,typedName) : l1,l2')
       
      (list1,associations) <- doList list0
      return (EmacsContent list1,associations)

-- ----------------------------------------------------------------------
-- Button names
--
-- Buttons put at the head of a container have names constructed as
--   'H'+area name
-- Other buttons/containers have names 'N'+area name.
-- ----------------------------------------------------------------------

headName :: MangledTypedName -> String
headName tn = 'H' : (unparseMangledTypedName tn)

normalName :: MangledTypedName -> String
normalName tn = 'N' : (unparseMangledTypedName tn)

data ButtonName = Head MangledTypedName | Normal MangledTypedName

parseButton :: String -> ButtonName
parseButton ('H':str) = Head (parseMangledTypedName str)
parseButton ('N':str) = Normal (parseMangledTypedName str)
parseButton (badName) = error ("Bad name "++badName)


-- ----------------------------------------------------------------------
-- MangledTypedName utilities
-- ----------------------------------------------------------------------

unparseMangledTypedName :: MangledTypedName -> String
unparseMangledTypedName (MangledTypedName str t) = str ++ [t] 

parseMangledTypedName :: String -> MangledTypedName
parseMangledTypedName [] = error "parseMangledTypedName given empty String"
parseMangledTypedName [t] = MangledTypedName "" t
parseMangledTypedName (c:cs) = 
   let 
      (MangledTypedName str t) = parseMangledTypedName cs
   in
      MangledTypedName (c:str) t


-- ----------------------------------------------------------------------
-- TypedName utilities
-- ----------------------------------------------------------------------

---
-- Unique key for TypedName's.
key :: TypedName -> String
key (str,c) = str

---
-- How the user sees a TypedName
describe :: TypedName -> String
describe (str,c) = str

---
-- Head button text and end text for a container
containerTexts :: TypedName -> (String,String)
containerTexts name =
   ("["++describe name++":\n","]\n")

---
-- Button text for a (collapsed) button
buttonText :: TypedName -> String
buttonText name =
   ("["++describe name++"]")

-- ----------------------------------------------------------------------
-- The Typed Name Mangler.
-- We actually need a number of Name Manglers, one for each letter.
-- (It is somewhat anomalous that we preserve the letters, but Emacs needs
-- to know them to know what colour to make buttons.
-- ----------------------------------------------------------------------

data TypedNameMangler = TypedNameMangler (Registry Char NameMangler)

data MangledTypedName = MangledTypedName String Char

newTypedNameMangler :: IO TypedNameMangler 
newTypedNameMangler = 
   do
      registry <- newRegistry
      return (TypedNameMangler registry)

newMangledTypedName :: TypedNameMangler -> TypedName -> IO MangledTypedName
newMangledTypedName (TypedNameMangler registry) (str,c) =
   transformValue registry c 
      (\ nameManglerOpt -> 
         do
            nameMangler <- case nameManglerOpt of
               Nothing -> newNameMangler
               Just nameMangler -> return nameMangler
            mangledName <- newMangledName nameMangler str
            return (Just nameMangler,MangledTypedName mangledName c)
         )

readMangledTypedName :: TypedNameMangler -> MangledTypedName -> IO TypedName
readMangledTypedName (TypedNameMangler registry) (MangledTypedName name c) =
   do
      nameManglerOpt <- getValueOpt registry c
      case nameManglerOpt of
         Nothing -> error "EmacsEdit: unknown letter"
         Just nameMangler ->
            do
               str <- readMangledName nameMangler name
               return (str,c)   