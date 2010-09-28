module Types.Folders where

import {-# SOURCE #-} Types.ViewType
import Imports.Imports
import {-# SOURCE #-} Types.LinkManager

data Folder

getImportsState :: View -> IO (ImportsState LinkedObject)
