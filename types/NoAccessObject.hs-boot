module NoAccessObject where

import {-# SOURCE #-} ViewType
import {-# SOURCE #-} ObjectTypes
import {-# SOURCE #-} LinkManager

data NoAccessObject

createNoAccessObject :: 
   ViewType.View 
   -> ObjectTypes.WrappedLink 
   -> IO NoAccessObject

isNoAccessLink ::
   ObjectTypes.WrappedLink
   -> Bool

noAccessObjectToLinkedObject :: NoAccessObject -> LinkManager.LinkedObject