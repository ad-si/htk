{- Haskell interface to the high-level extent functions in sendmess.el-}
module Extents(
   initBuffer,
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
   listContainers,
   ) where

import DeepSeq

import EmacsBasic
import EmacsCommands
import EmacsContent
import EmacsSExp

initBuffer :: EmacsSession-> IO ()
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


