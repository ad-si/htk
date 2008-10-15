module Folders where

import {-# SOURCE #-} ViewType
import Imports
import {-# SOURCE #-} LinkManager

data Folder

getImportsState :: View -> IO (ImportsState LinkedObject)
