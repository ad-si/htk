module MMiSSInsertionPoint where

import EntityNames
import LinkManager
import Link
import Folders

type InsertionPoint = Either LinkedObject (Link Folder,EntityName)