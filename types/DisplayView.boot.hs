{- This is a boot version of DisplayView. -}
module DisplayView where

#if (__GLASGOW_HASKELL__ >= 503)
#define PrelInt GHC.Base.Int
#else
#define PrelInt Int
#endif

-- We need to at least force the kinds of the type variables to be right.
data  DisplayedView graph graphParms node nodeType nodeTypeParms arc arcType 
      arcTypeParms =
   NOTHING {
      graph :: graph,
      graphParms :: graphParms,
      node :: node PrelInt,
      nodeType :: nodeType PrelInt,
      nodeTypeParms :: nodeTypeParms PrelInt,
      arc :: arc PrelInt,
      arcType :: arcType PrelInt,
      arcTypeParms :: arcTypeParms PrelInt
      }

