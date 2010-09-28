module Types.NoAccessObject where

import {-# SOURCE #-} Types.ViewType
import {-# SOURCE #-} Types.ObjectTypes
import {-# SOURCE #-} Types.LinkManager

data NoAccessObject

createNoAccessObject :: View -> WrappedLink -> IO NoAccessObject

isNoAccessLink :: WrappedLink -> Bool

noAccessObjectToLinkedObject :: NoAccessObject -> LinkedObject
