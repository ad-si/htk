{- This module contains various aliases for TemplateHaskell, in particular
   to deal with the fact that a number of names seem to be different between
   ghc6 and ghc6.3.
   -}
module TemplateHaskellHelps(dynName) where

#if (__GLASGOW_HASKELL__ >= 603)
import Language.Haskell.TH

dynName :: String -> ExpQ
dynName = varE . mkName

#else

import Language.Haskell.THSyntax

dynName :: String -> ExpQ
dynName = var

#endif
