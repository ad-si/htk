{- This module edits Emacs content, including "magic buttons" to allow
   the user expand bits of content further. -}
module EmacsEdit(
   editEmacs, -- :: EmacsFS -> String -> IO ()
   EmacsFS(..),
   EditedFile(..),
   PrintAction(..),
   ) where

import Maybe
import Monad

import Computation
import Registry
import NameMangle
import ExtendedPrelude
import Messages

import Events
import Channels
import Synchronized
import Destructible

import LockEvent

import HTk(text)
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
-- The file system is allowed its own handle, a ref.  For the functions,
-- it is required that "ref" instances Eq and Ord.
-- 
-- It is also assumed that editFS will stop the same file being opened if
-- it is already open (without it being closed by finishEdit).
data EmacsFS ref = EmacsFS {
   -- | editFS name
   -- attempts to edit the file name.
   -- It returns the initial contents and the file's EditedFile structure.
   editFS :: ref -> IO (WithError (EmacsContent ref,EditedFile ref)),
   -- | miniType returns a character corresponding to a type designation.
   -- These letters correspond to those in the list MMiSS-colours in
   -- allmmiss.el.
   toMiniType :: ref -> Char,
   -- | toDescription returns a user-friendly name for the reference (used in
   -- error messages and for button names).
   toDescription :: ref -> String,
   -- | createRef is called when the user indicates that he wants to
   -- create a new import reference.  The argument is the reference that
   -- will contain the new ref.  The result is an error, or Nothing if the
   -- user cancelled, or the new ref.
   createRef :: ref -> IO (WithError (Maybe ref))
   }

---
-- EditedFile (provided by the caller) describes a file as it is edited by
-- this module
data EditedFile ref = EditedFile {
   writeData :: EmacsContent ref -> IO (WithError (Maybe (EmacsContent ref))),
      -- ^ Attempt to write back the edited content (this may be done more than
      --   once)
      --
      -- writeData may return Just (EmacsContent ref).  This should correspond
      -- to the new text of the corresponding container, which is to replace
      -- the old text.  
      -- RESTRICTION - the ref's in such a new text must be a supersequence of
      -- the refs in the old text.  That is, every ref in the old text must
      -- occur in the new text, and in the same order, though other refs,
      -- and different texts, may also be inserted.
   finishEdit :: IO ()
      -- ^ action to be called at the end (when we no longer want to edit
      -- this file).
   }

---
-- The PrintAction, supplied by the caller, prints the given ref 
-- as displayed in the buffer, which should be included in the buffer.  To 
-- do this it in turn is provided with a function which given a particular 
-- String returns what the Emacs buffer
-- currently contains for that String.  Included items are given as 
-- EmacsLink; the Bool indicates whether the included item is expanded (True)
-- or not.  
newtype PrintAction ref = PrintAction 
   (ref -> (ref -> IO (WithError (EmacsContent (Bool,ref))))
      -> IO ()
      )

-- ----------------------------------------------------------------------
-- Other datatypes
-- ----------------------------------------------------------------------

data EditorState ref = EditorState {
   emacsSession :: EmacsSession,
   emacsFS :: EmacsFS ref,
   openFiles :: Registry ref (EditedFile ref), -- ^ currently edited files

   typedNameMangler :: TypedNameMangler ref,
      -- This makes all names seen by Emacs unique.
   printAction :: PrintAction ref,
   topMangledName :: MangledTypedName
   }      

-- ----------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------

---
-- editEmacs edits a particular file, with the specified file system. 
-- This function terminates when the user finishes editing.
editEmacs :: Ord ref => EmacsFS ref -> PrintAction ref -> ref -> IO ()
editEmacs emacsFS printAction ref =
   do
      typedNameMangler <- newTypedNameMangler
      mangledName <- newMangledTypedName typedNameMangler ref 
         (toMiniType emacsFS ref)

      let
         -- action for opening the Emacs window, after openFile has managed
         -- to open the file.
         parentAction =
            do
               -- (1) Initialise Emacs and MMiSS-TeX.
               emacsSession <- newEmacsSession (toDescription emacsFS ref)
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

      editorStateOpt <- openFile emacsFS parentAction ref mangledName

      case editorStateOpt of
         Just editorState ->
            do
               clearModifiedFlag (emacsSession editorState)
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
openFile :: Ord ref 
   => EmacsFS ref -> IO (String,EditorState ref) -> ref -> MangledTypedName
   -> IO (Maybe (EditorState ref))
openFile emacsFS parentAction ref mangledName =
   do
      emacsFileWE <- editFS emacsFS ref
      case fromWithError emacsFileWE of
         Left message -> 
            do
               errorMess message
               return Nothing
         Right (emacsContent,emacsFile) ->
            do
               let
                  (EmacsContent initialContents) 
                     = collapseEmacsContent emacsContent
               (parent,state) <- parentAction

               -- Add a new entry to the registry
               setValue (openFiles state) ref emacsFile

               let
                  session = emacsSession state

                  (headString,endString) = containerTexts emacsFS ref

               -- Insert the button
               addHeadButton session parent (headName mangledName) headString

               -- Insert the contents
               mapM
                  (\ dataItem -> case dataItem of
                     EmacsLink child -> 
                        do
                           mangledChild <- newMangled state child
                           addButton session parent (normalName mangledChild) 
                              (buttonText emacsFS child)
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

handleEvents :: Ord ref => EditorState ref -> Event ()
handleEvents (editorState :: EditorState ref) =
   let
      session = emacsSession editorState

      nameMangler = typedNameMangler editorState
      readMangled = readMangledTypedName nameMangler

      event :: String -> Event String 
      event key = emacsEvent session key

      iterate = handleEvents editorState

      fs :: EmacsFS ref
      fs = emacsFS editorState

      describe :: ref -> String
      describe ref = toDescription fs ref

      confirm :: String -> Event () -> Event ()
      confirm str event =
         do
            goAhead <- always (confirmMess str)
            if goAhead then event else iterate

      showError :: String -> IO ()
      showError str = errorMess str

      getModified =
         do
            containers <- listContainers session
            filterM (isModified session) containers

      -- Action to be taken when a normal button is clicked.  This is
      -- complex enough to deserve its own function.
      buttonMenu :: MangledTypedName -> IO ()
      buttonMenu mangledTypedName =
         do
            ref <- readMangled mangledTypedName
            let
               buttonId = normalName mangledTypedName

            -- work out what to do.
            (action :: NormalButtonAction) <- createDialogWin
               [("Expand",Expand),("Delete",Delete),("Cancel",Cancel)]
               Nothing
               [text ("Do what to "++ describe ref ++ "?")]
               [text "Button action window"]

            case action of
               Cancel -> done
               Expand ->
                  do
                     let
                        parentAction =
                           do
                              expand session buttonId
                              return (buttonId,editorState)
                     lockBuffer session
                     wasModified <- getModifiedFlag session
                     openFile fs parentAction ref mangledTypedName
                     unless wasModified (clearModifiedFlag session)
                     unlockBuffer session
               Delete ->
                  do
                     goAhead <- confirmMess
                        ("Really delete " ++ describe ref ++ "?")
                     when goAhead (deleteExtent session buttonId)

   in
         (do
            str <- event "COMMIT"
            confirm ("Commit?") (always (do
               lockBuffer session
               containers <- getModified
               (success :: [Bool]) <- mapM
                  (\ hContainer ->
                     do
                        let
                           mangledContainer = case parseButton hContainer of
                              Normal mangledContainer -> mangledContainer
                              _ -> error ("EmacsEdit: Mysterious container "++
                                 hContainer)
                        container <- readMangled mangledContainer
                        fileOpt <- getValueOpt (openFiles editorState) 
                           container
                        
                        file <- case fileOpt of
                           Just file -> return file
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
                           Left mess -> 
                              do
                                 showError ("Writing "
                                    ++describe container++": "++mess)
                                 return False
                           Right newContentsOpt -> 
                              do
                                 -- Alter contents if necessary.
                                 case newContentsOpt of
                                    Nothing -> done
                                    Just newContents ->
                                       modifyFile editorState mangledContainer
                                          (fmap snd mangledContents)
                                          (fmap snd unmangledContents)
                                          newContents
                                 -- clear modified flag for container.
                                 unmodify session hContainer
                                 return True
                     )
                  containers

               case containers of
                  [] -> warningMess "Nothing to commit!"
                  _ -> done

               if and success
                  then
                     clearModifiedFlag session
                  else
                     done

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

            -- boilerplate: check that the button exists.  (It could not,
            -- if for example the user queued two button click events, the
            -- first of which was a delete.)
            extentExists <- always (extentExists session str)
            
            case (extentExists,parseButton str) of
               (False,_) -> iterate
               (True,Normal mangledName) ->
                  do
                     always (buttonMenu mangledName)
                     iterate
               (True,Head mangledName) ->
                  always (
                     do
                        let
                           normal = normalName mangledName

                        ref <- readMangled mangledName

                        lockBuffer session
                        
                        proceed1 <-
                           do
                              modified <- isModified session normal
                              if modified
                                 then
                                    confirmMess ("Collapse "
                                       ++describe ref++" without saving?")
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
                                                ref2 
                                                   <- readMangled mangledName2
                                                errorMess ("Collapse " 
                                                   ++ describe ref2 
                                                   ++ " first!")
                                                return False
                              else
                                 return False
                        if proceed2
                           then
                              do
                                 wasModified <- getModifiedFlag session

                                 collapse session normal (buttonText fs ref)

                                 unless wasModified (clearModifiedFlag session)

                                 transformValue (openFiles editorState) 
                                       ref
                                    (\ (stateOpt :: Maybe (EditedFile ref)) ->
                                       do
                                          case stateOpt of
                                             Just state -> finishEdit state
                                             Nothing ->
                                                putStrLn ("Odd - "
                                                   ++ describe ref
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
                          confirmMess "Exit without saving anything?"
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
            str <- event "IMPORT"
            always (do
               lockBuffer session
               point <- currentPoint session 
                  -- we need to get hold of this because the user might
                  -- otherwise move it during the following interactions,
                  -- even though the Haskell buffer is locked.
               containerWE <- checkInsertion session point
               case fromWithError containerWE of
                  Left error -> errorMess error
                  Right containerStr ->
                     do
                        let
                           Normal mangledTypedName = parseButton containerStr
                        container <- readMangled mangledTypedName
                        newRefOptWE <- createRef fs container
                        case fromWithError newRefOptWE of
                           Left error -> errorMess error
                           Right Nothing -> done
                           Right (Just ref) ->
                              do
                                 mangledTypedName 
                                    <- newMangled editorState ref
                                 let
                                    extentId = normalName mangledTypedName
                                    text = buttonText fs ref
                                 insertButtonAt session point extentId text
               unlockBuffer session 
               )
            iterate
         )
      +> (do
            str <- event "ENLARGE"
            always (showError 
               "Sorry, the Enlarge operation is currently not supported")
            iterate            
         )

-- ----------------------------------------------------------------------
-- Action to be taken when we click on a normal button
-- ----------------------------------------------------------------------


data NormalButtonAction = Expand | Delete | Cancel

-- ----------------------------------------------------------------------
-- Printing 
-- ----------------------------------------------------------------------

doPrint :: Ord ref => EditorState ref -> MangledTypedName -> IO ()
doPrint (editorState :: EditorState ref) mangledToEdit =
   do
      -- The main problem here is writing the function to be passed to the
      -- print action.

      -- We create a registry mapping ref's to their corresponding
      -- MangledTypedName (which should currently be open).  Since files are
      -- only supposed to be open once (see comments to EditFS) this is not
      -- a problem.
      (openMangledNames :: Registry ref MangledTypedName) <- newRegistry

      let
         session = emacsSession editorState

         describe :: ref -> String
         describe = toDescription (emacsFS editorState)

         readMangled = readMangledTypedName (typedNameMangler editorState)

         printFunction :: ref 
            -> IO (WithError (EmacsContent (Bool,ref)))
         printFunction ref =
            addFallOutWE (\ break ->
               do
                  mangledToGetOpt <- getValueOpt openMangledNames ref

                  mangledToGet <- case mangledToGetOpt of
                     Just mangledToGet -> return mangledToGet
                     Nothing -> break (
                        "EmacsEdit: couldn't find "++describe ref)
                  seq mangledToGet done

                  mangledContents <- extractContents editorState mangledToGet
                  (unmangledContents,associations) 
                     <- unmangleContents editorState mangledContents  
                  mapM_
                     (\ (ref,mangledTypedName) -> setValue 
                        openMangledNames ref mangledTypedName
                        )
                     associations
                  return unmangledContents
               )

         (PrintAction mkPrint) = printAction editorState

      toEdit <- readMangled mangledToEdit
      setValue openMangledNames toEdit mangledToEdit

      lockBuffer session
      toEditType <- getExtentType session (normalName mangledToEdit)
      if toEditType == "container"
         then
            mkPrint toEdit printFunction
         else
            errorMess ("Extent "++describe toEdit
               ++" is not currently open")
      unlockBuffer session


-- ----------------------------------------------------------------------
-- Modifying a particular opened file.
-- ----------------------------------------------------------------------

--
-- Replace the old content of a container by new content.
--
-- We don't lock the buffer during this, since we assume that is already done.
--
-- MAJOR RESTRICTION - it is assumed that all the references in the old
-- content are also contained in the new content, in the same order.
modifyFile :: Ord ref
   => EditorState ref -- ^ editor state
   -> MangledTypedName -- ^ MTN for the container.
   -> EmacsContent MangledTypedName 
      -- ^ old contents for the container as MTNs.
   -> EmacsContent ref -- ^ old contents, translated into refs.
   -> EmacsContent ref -- ^ new contents.
   -> IO ()
modifyFile (state :: EditorState ref) headMTN 
      (EmacsContent oMTNs) oContent nContent =
   let
      -- (1) extract the mtns for the old contents.
      mtns :: [MangledTypedName]
      mtns = mapMaybe 
         (\ dataItem -> case dataItem of
            EditableText _ -> Nothing
            EmacsLink mtn -> Just mtn
            )
         oMTNs

      -- (2) get the dataitems for the contents, collapsing both contents first
      -- just in case no-one else has.
      oItems :: [EmacsDataItem ref]
      EmacsContent oItems = collapseEmacsContent oContent

      nItems :: [EmacsDataItem ref]
      EmacsContent nItems = collapseEmacsContent nContent

      -- (3) zip everything together.
      zippedItems :: [(String,String,[EmacsDataItem ref])]
      -- 1st String: extent-id for the preceding extent.
      -- 2nd String: text following (or possibly "") up to next ref in old 
      --    contents
      -- List: stuff up to the same ref in the new contents.
      zippedItems = zipItems (headName headMTN) mtns oItems nItems

      zipItems :: String -> [MangledTypedName] 
         -> [EmacsDataItem ref] -> [EmacsDataItem ref]
         -> [(String,String,[EmacsDataItem ref])]
      zipItems extentId mtns0 oDataItems [] = []
      zipItems extentId mtns0 oDataItems0 nDataItems0 =
         let
            (oFollowingText,followingRefOpt,oDataItems1) = case oDataItems0 of
               [] -> ("",Nothing,[])
               [EditableText text] -> (text,Nothing,[])
               EditableText text : EmacsLink ref : oDataItems1 ->
                  (text,Just ref,oDataItems1)
               EmacsLink ref : oDataItems1 -> ("",Just ref,oDataItems1)
               -- EditableText : EditableText is impossible because of the
               -- call to collapseEmacsContent
         in
            case followingRefOpt of
               Nothing -> [(extentId,oFollowingText,nDataItems0)]
               Just followingRef ->
                  let
                     splitNDataItems = splitToElem
                        (\ dataItem -> case dataItem of
                           EmacsLink ref | ref == followingRef
                              -> True
                           _ -> False
                           )
                        nDataItems0


                     (theseDataItems,nDataItems1) =
                         fromMaybe (error 
                              ("EmacsEdit.modifyFile given new contents not a"
                                 ++ "supersequence of the old one."))
                              splitNDataItems
                   
                     thisItem = (extentId,oFollowingText,theseDataItems)

                     (nextMTN : mtns1) = mtns0

                     restItems = zipItems (normalName nextMTN) mtns1 
                        oDataItems1 nDataItems1
                  in
                     thisItem : restItems

      mapItem :: (String,String,[EmacsDataItem ref]) -> IO (Maybe [Multi])
      mapItem (extentId,oldText,dataItems) =
         case (oldText,dataItems) of
            (text1,[EditableText text2]) | text1 == text2
               -> return Nothing
            ("",[]) -> return Nothing
            _ ->
               do
                  let
                     headCommand = pointAfterExtent extentId
                     delCommand = deleteFromPoint (length oldText)
                  insertItems <- mapM
                     (\ dataItem ->
                        case dataItem of
                           EmacsLink child ->
                              do
                                 mangledChild <- newMangled state child
                                 return (insertButtonBeforePoint 
                                    (normalName mangledChild) 
                                    (buttonText (emacsFS state) child))
                           EditableText str -> return (insertBeforePoint str)
                        )        
                     dataItems

                  return (Just (headCommand:delCommand:insertItems))
   in 
      mapM_
         (\ zippedItem ->
            do
               multisOpt <- mapItem zippedItem
               case multisOpt of
                  Nothing -> done
                  Just multis -> execEmacs (emacsSession state) multis
            )
         zippedItems

-- ----------------------------------------------------------------------
-- Extracting the contents of a buffer
-- The head button is removed (and checked for).
-- Bools indicate that the corresponding extent is further expanded.
-- ----------------------------------------------------------------------

extractContents :: EditorState ref -> MangledTypedName 
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
unmangleContents :: EditorState ref -> EmacsContent (Bool,MangledTypedName)
   -> IO (EmacsContent (Bool,ref),[(ref,MangledTypedName)])
unmangleContents (editorState :: EditorState ref) (EmacsContent list0) =
   do
      let
         readMangled = readMangledTypedName (typedNameMangler editorState)

         doList :: [EmacsDataItem (Bool,MangledTypedName)] ->
            IO ([EmacsDataItem (Bool,ref)],
               [(ref,MangledTypedName)])
         doList [] = return ([],[])
         doList (EditableText text : rest) =
            do
               (l1,l2) <- doList rest
               return (EditableText text : l1,l2)
         doList (EmacsLink (b,mangledTypedName) : rest) =
            do
               (l1,l2) <- doList rest
               ref <- readMangled mangledTypedName

               let
                  l2' = if b then (ref,mangledTypedName):l2 else l2
               return (EmacsLink (b,ref) : l1,l2')
       
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
-- Extracting texts from refs.
-- ----------------------------------------------------------------------

---
-- Head button text and end text for a container
containerTexts :: EmacsFS ref -> ref -> (String,String)
containerTexts fs ref = ("["++toDescription fs ref++":\n","]\n")

---
-- Button text for a (collapsed) button
buttonText :: EmacsFS ref -> ref -> String
buttonText fs ref = "["++toDescription fs ref++"]"

-- ----------------------------------------------------------------------
-- Higher-level interface to newMangledTypedName
-- ----------------------------------------------------------------------

newMangled :: Ord ref => EditorState ref -> ref -> IO MangledTypedName
newMangled state ref =
   newMangledTypedName 
      (typedNameMangler state)
      ref
      (toMiniType (emacsFS state) ref)

-- ----------------------------------------------------------------------
-- The NameGenerator.  This has to map the "ref's" in an EmacsFS into
-- MangledTypedName's.
-- ----------------------------------------------------------------------

data TypedNameMangler ref = TypedNameMangler (Registry Char (NameMangler ref))

data MangledTypedName = MangledTypedName String Char deriving (Show)

newTypedNameMangler :: IO (TypedNameMangler ref)
newTypedNameMangler = 
   do
      registry <- newRegistry
      return (TypedNameMangler registry)

---
-- newMangledTypedName expects in addition to the thing to be mangled, its
-- minitype.
newMangledTypedName :: TypedNameMangler ref -> ref -> Char 
   -> IO MangledTypedName
newMangledTypedName (TypedNameMangler registry) ref c =
   transformValue registry c 
      (\ nameManglerOpt -> 
         do
            nameMangler <- case nameManglerOpt of
               Nothing -> newNameMangler
               Just nameMangler -> return nameMangler
            mangledName <- newMangledName nameMangler ref
            return (Just nameMangler,MangledTypedName mangledName c)
         )

readMangledTypedName :: TypedNameMangler ref -> MangledTypedName -> IO ref
readMangledTypedName (TypedNameMangler registry) (MangledTypedName name c) =
   do
      nameManglerOpt <- getValueOpt registry c
      case nameManglerOpt of
         Nothing -> error "EmacsEdit: unknown letter"
         Just nameMangler ->
            do
               ref <- readMangledName nameMangler name
               return ref