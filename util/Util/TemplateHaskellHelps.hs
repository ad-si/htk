-- | This module contains various aliases for TemplateHaskell, in particular
-- to deal with the fact that a number of names seem to be different between
-- ghc6, ghc6.2 and ghc6.3.
module Util.TemplateHaskellHelps(dynName) where

/* We need a three-way switch here. */
#if (__GLASGOW_HASKELL__ >= 602)

#if (__GLASGOW_HASKELL__ == 602)

{- ghc6.2 -}
import Language.Haskell.THSyntax

dynName :: String -> ExpQ
dynName = varE

#else

{- ghc6.3 or later -}

import Language.Haskell.TH

dynName :: String -> ExpQ
dynName = varE . mkName

#endif

#else

{- ghc 6.0 -}
import Language.Haskell.THSyntax

dynName :: String -> ExpQ
dynName = var

#endif
