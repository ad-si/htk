{- Haskell interface to the high-level extent functions in sendmess.el-}
module Extents(
   addContainerBuffer,
   prependContainerBuffer,
   addContainer,
   addButton,
   addText,
   boundContainer,
   deleteExtent,
   boundContainer,
   collapse,
   containerContents,
   ) where

import EmacsBasic
import EmacsCommands
import ExtentContents

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

containerContents :: EmacsSession -> String -> IO (EmacsContents String)
containerContents emacsSession this =
   do
      str <- evalEmacs emacsSession (Prin ("uni-container-contents",[this]))
      return (parseEmacsContents str)


