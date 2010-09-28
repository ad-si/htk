module MMiSS.InsertionPoint where

import Imports.EntityNames as EntityNames
import Types.LinkManager as LinkManager
import Types.Link as Link
import Types.Folders as Folders

type InsertionPoint = Either LinkedObject (Link Folder,EntityName)
