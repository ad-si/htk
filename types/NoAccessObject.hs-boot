module NoAccessObject where

import {-# SOURCE #-} ViewType
import {-# SOURCE #-} ObjectTypes
import {-# SOURCE #-} LinkManager

data NoAccessObject

createNoAccessObject :: View -> WrappedLink -> IO NoAccessObject

isNoAccessLink :: WrappedLink -> Bool

noAccessObjectToLinkedObject :: NoAccessObject -> LinkedObject
