{- This is a boot version of DisplayView. -}
module DisplayView(
   DisplayedView,
   ) where

-- We need to at least force the kinds of the type variables to be right.
data  DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms =
   NOTHING {
      graph :: graph,
      graphParms :: graphParms,
      node :: node Int,
      nodeType :: nodeType Int,
      nodeTypeParms :: nodeTypeParms Int,
      arc :: arc Int,
      arcType :: arcType Int,
      arcTypeParms :: arcTypeParms Int
      }

