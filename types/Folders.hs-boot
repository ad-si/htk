module Folders where

import {-# SOURCE #-} ViewType
import Imports
import {-# SOURCE #-} LinkManager

data Folder

getImportsState :: ViewType.View 
   -> IO (Imports.ImportsState LinkManager.LinkedObject)
