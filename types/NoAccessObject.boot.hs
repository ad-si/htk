module NoAccessObject where

data NoAccessObject

createNoAccessObject :: 
   ViewType.View 
   -> ObjectTypes.WrappedLink 
   -> GHC.IOBase.IO NoAccessObject

isNoAccessLink ::
   ObjectTypes.WrappedLink
   -> GHC.Base.Bool

noAccessObjectToLinkedObject :: NoAccessObject -> LinkManager.LinkedObject