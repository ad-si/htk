module Folders where

data Folder

getImportsState :: ViewType.View 
   -> GHC.IOBase.IO (Imports.ImportsState LinkManager.LinkedObject)
