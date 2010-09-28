-- | This is a boot version of DisplayView. 
module Types.DisplayView where

-- We need to at least force the kinds of the type variables to be right.
data  DisplayedView graph graphParms 
   (node :: * -> *) (nodeType :: * -> *) (nodeTypeParms :: * -> *)
   (arc :: * -> *) (arcType :: * -> *) (arcTypeParms :: * -> *)
