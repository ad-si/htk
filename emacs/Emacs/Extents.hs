-- | Haskell interface to the high-level extent functions in extents.el
module Emacs.Extents(
   initBuffer,
   lockBuffer,
   unlockBuffer,
   addContainerBuffer,
   prependContainerBuffer,
   addContainer,
   addButton,
   addHeadButton,
   addUneditable,
   addText,
   boundContainer,
   deleteExtent,
   getExtentType,
   expand,
   collapse,
   containerContents,
   containerFullContents,
   ContainerChild(..),
   containerChildren,
   isModified,
   unmodify,
   listContainers,
   setColourHack,
   clearModifiedFlag,
   getModifiedFlag,
   extentExists,

   pointAfterExtent,
   deleteFromPoint,
   insertBeforePoint,
   insertButtonBeforePoint,

   currentPoint,
   checkInsertion,
   insertButtonAt,
   ) where

import Data.Maybe

import Util.DeepSeq
import Util.Computation

import Emacs.Basic
import Emacs.Commands
import Emacs.Content
import Emacs.SExp

initBuffer :: EmacsSession -> IO ()
initBuffer es = execEmacs es ("uni-initialise-extents")

addContainerBuffer :: EmacsSession -> String -> IO ()
addContainerBuffer emacsSession str =
   execEmacs emacsSession ("uni-add-container-buffer",[str])

prependContainerBuffer :: EmacsSession -> String -> IO ()
prependContainerBuffer emacsSession str =
   execEmacs emacsSession ("uni-prepend-container-buffer",[str])

addContainer :: EmacsSession -> String -> String -> IO ()
addContainer emacsSession parent new =
   execEmacs emacsSession ("uni-add-container",[parent,new])

addButton :: EmacsSession -> String -> String -> String -> IO ()
addButton emacsSession parent new text =
   execEmacs emacsSession ("uni-add-button",[parent,new,text])

addHeadButton :: EmacsSession -> String -> String -> String -> IO ()
addHeadButton emacsSession parent new text =
   execEmacs emacsSession ("uni-add-head-button",[parent,new,text])

setColourHack :: EmacsSession -> IO ()
setColourHack emacsSession =
   execEmacs emacsSession "uni-set-colour-hack"

addText :: EmacsSession -> String -> String -> IO ()
addText emacsSession parent text =
   execEmacs emacsSession ("uni-add-text",[parent,text])

addUneditable :: EmacsSession -> String -> String -> IO ()
addUneditable emacsSession parent text =
   execEmacs emacsSession ("uni-add-uneditable",[parent,text])

boundContainer :: EmacsSession -> String -> IO ()
boundContainer emacsSession parent =
   execEmacs emacsSession ("uni-bound-container",[parent])

deleteExtent :: EmacsSession -> String -> IO ()
deleteExtent emacsSession this =
   execEmacs emacsSession ("uni-delete-extent",this)

expand :: EmacsSession -> String -> IO ()
expand emacsSession this = execEmacs emacsSession ("uni-expand",this)

collapse :: EmacsSession -> String -> String -> IO ()
collapse emacsSession this text =
   execEmacs emacsSession ("uni-collapse",[this,text])

containerContents :: EmacsSession -> String -> IO (EmacsContent String)
containerContents emacsSession this =
   do
      str <- evalEmacsQuick emacsSession
         (Prin ("uni-container-contents",this))
      return (parseEmacsContent str)

getExtentType :: EmacsSession -> String -> IO String
getExtentType emacsSession this =
   evalEmacsQuick emacsSession (Prin ("uni-get-extent-id-type",this))

-- | Like containerContents, but also returns a Bool for each link indicating
-- True if the object is expanded here.
containerFullContents :: EmacsSession -> String
   -> IO (EmacsContent (Bool,String))
containerFullContents emacsSession this =
   do
      str <- evalEmacsQuick emacsSession
         (Prin ("uni-container-contents",this))
      return (parseEmacsContentGeneral str)

-- | Returns 'True' if a container is modified
isModified :: EmacsSession -> String -> IO Bool
isModified emacsSession this =
   do
      str <- evalEmacsQuick emacsSession
         (Prin ("uni-container-modified",this))
      return (case str of
         "nil" -> False
         "t" -> True
         _ -> error ("Extents.isModified: unexpected Emacs return "++str)
         )

-- | Unmodify a container
unmodify :: EmacsSession -> String -> IO ()
unmodify emacsSession this =
   execEmacs emacsSession ("uni-unmodify-container",this)


data ContainerChild = Button String | Container String

containerChildren :: EmacsSession -> String -> IO [ContainerChild]
containerChildren emacsSession this =
   do
      str <- evalEmacsQuick emacsSession
         (Prin ("uni-container-children",this))
      let
         (EmacsContent dataItems) = parseEmacsContentGeneral str
         result = map
            (\ dataItem -> case dataItem of
               EmacsLink (b,str) ->
                  if b then Container str else Button str
               EditableText _ -> error "uni-container-children returned text"
               )
            dataItems
      return result

listContainers :: EmacsSession -> IO [String]
listContainers emacsSession =
   do
      str <- evalEmacsQuick emacsSession (Prin "uni-list-containers")
      let
         bad = error ("listContainers: couldn't parse Emacs response "++str)
         result =
            case doParse str of
               List sexps ->
                  map
                     (\ sexp -> case sexp of
                        String s -> s
                        _ -> bad
                        )
                     sexps
               _ -> bad
      result `deepSeq` (return result)

lockBuffer :: EmacsSession -> IO ()
lockBuffer es = execEmacs es ("uni-lock-buffer")

unlockBuffer :: EmacsSession -> IO ()
unlockBuffer es = execEmacs es ("uni-unlock-buffer")

-- | Clear the buffer's modified flag (hopefully clearing the \"**\" in the
-- XEmacs status display).
clearModifiedFlag :: EmacsSession -> IO ()
clearModifiedFlag es = execEmacs es (Literal "(set-buffer-modified-p nil)")

getModifiedFlag :: EmacsSession -> IO Bool
getModifiedFlag emacsSession =
   do
      str <- evalEmacsQuick emacsSession (Prin "buffer-modified-p")
      return (doParseBool str)

extentExists :: EmacsSession -> String -> IO Bool
extentExists emacsSession extentId =
   do
      str <- evalEmacsQuick emacsSession (Prin ("uni-extent-exists",extentId))
      return (doParseBool str)


currentPoint :: EmacsSession -> IO Int
currentPoint emacsSession =
   do
      str <- evalEmacsQuick emacsSession (Prin ("point"))
      return (doParseInt str)

-- | Check whether it is safe to insert a button at this point.
-- If Yes, return the containing container.
checkInsertion :: EmacsSession -> Int -> IO (WithError String)
checkInsertion emacsSession i =
   do
      str <- evalEmacsQuick emacsSession (Prin
         ("uni-check-insertion-dottedpair",i))
      return (case doParse str of
         DotList [isOKsexp] (String result)
            | Just isOK <- sexpToBool isOKsexp
               -- that's a GHC extension, by the way.
               -> if isOK then hasValue result else hasError result
         _ -> error ("Extents.checkInsertion - can't parse "++str)
         )

insertButtonAt :: EmacsSession -> Int -> String -> String -> IO ()
insertButtonAt emacsSession i extentId text =
   execEmacs emacsSession [gotoChar i,insertButtonBeforePoint extentId text]

gotoChar :: Int -> Multi
gotoChar i = multi ("goto-char",i)


-- | The following operations are used for modifying the contents of a
-- container.
-- They return a value of type \"Multi\" since they indicate the operation
-- to be performed; the caller then needs to perform them all at once as
-- a list.
pointAfterExtent :: String -> Multi
pointAfterExtent extentId = multi ("uni-point-after-extent",extentId)

deleteFromPoint :: Int -> Multi
deleteFromPoint nchars = multi ("delete-char",nchars)

insertBeforePoint :: String -> Multi
insertBeforePoint text = multi ("insert",text)

insertButtonBeforePoint :: String -> String -> Multi
insertButtonBeforePoint extentId text
   = multi ("uni-add-button-point",[extentId,text])


