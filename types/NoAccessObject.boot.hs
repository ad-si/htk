module NoAccessObject where

createNoAccessObject :: 
   ViewType.View 
   -> ObjectTypes.WrappedLink 
   -> GHC.IOBase.IO LinkManager.LinkedObject

isNoAccessLink ::
   ObjectTypes.WrappedLink
   -> GHC.Base.Bool
