-- | Module that should also compile on Windows for blocking sigPIPE on
-- Unix, something you need to do to avoid the entire system crashing when
-- a pipe is closed.
module BlockSigPIPE(
   blockSigPIPE, -- :: IO ()
   ) where

import Language.Haskell.TH
import System.Posix.Signals
-- this module still seems to exist on Windows, though without doing
-- anything useful.

import Computation
import CompileFlags
import TemplateHaskellHelps


$(
   if isWindows
      then
         [d|
            blockSigPIPE = done
         |]
      else
         [d|
            blockSigPIPE = 
               do
                  $(dynName "installHandler") $(dynName "sigPIPE") 
                     $(conE . mkName $ "Ignore") Nothing
                  done
         |]
   )

blockSigPIPE :: IO ()

