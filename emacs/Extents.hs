{- Haskell interface to the high-level extent functions in extents.el-}
module Extents(
   initBuffer,
   lockBuffer,
   unlockBuffer,
   addContainerBuffer,
   prependContainerBuffer,
   addContainer,
   addButton,
   addText,
   boundContainer,
   deleteExtent,
   expand,
   collapse,
   containerContents,
   ContainerChild(..),
   containerChildren,
   listContainers,
   setColourHack,
   ) where

import Maybe

import DeepSeq

import EmacsBasic
import EmacsCommands
import EmacsContent
import EmacsSExp

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

setColourHack :: EmacsSession -> IO ()
setColourHack emacsSession =
   execEmacs emacsSession "uni-set-colour-hack"

addText :: EmacsSession -> String -> String -> IO ()
addText emacsSession parent text =
   execEmacs emacsSession ("uni-add-text",[parent,text])

boundContainer :: EmacsSession -> String -> IO ()
boundContainer emacsSession parent =
   execEmacs emacsSession ("uni-bound-container",[parent])

deleteExtent :: EmacsSession -> String -> IO ()
deleteExtent emacsSession this =
   execEmacs emacsSession ("uni-delete-extent",[this])

expand :: EmacsSession -> String -> IO ()
expand emacsSession this = execEmacs emacsSession ("uni-expand",[this])

collapse :: EmacsSession -> String -> String -> IO ()
collapse emacsSession this text =
   execEmacs emacsSession ("uni-collapse",[this,text])

containerContents :: EmacsSession -> String -> IO (EmacsContent String)
containerContents emacsSession this =
   do
      str <- evalEmacsQuick emacsSession 
         (Prin ("uni-container-contents",[this]))
      return (parseEmacsContent str)

data ContainerChild = Button String | Container String

containerChildren :: EmacsSession -> String -> IO [ContainerChild]
containerChildren emacsSession this =
   do
      str <- evalEmacsQuick emacsSession 
         (Prin ("uni-container-children",[this]))
      let
         List children = doParse str
         result = mapMaybe
            (\ (DotList [Id objectType] (String childName)) ->
               case objectType of
                  "button" -> Just (Button childName)
                  "container" -> Just (Container childName)
                  _ -> Nothing
               )
            children
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




