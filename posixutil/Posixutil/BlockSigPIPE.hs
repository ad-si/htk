{-# LANGUAGE CPP #-}
-- | Module that should also compile on Windows for blocking sigPIPE on
-- Unix, something you need to do to avoid the entire system crashing when
-- a pipe is closed.
module Posixutil.BlockSigPIPE(
   blockSigPIPE, -- :: IO ()
   ) where

import Util.Computation

#ifndef WINDOWS
import System.Posix.Signals
#endif

blockSigPIPE :: IO ()
blockSigPIPE = do
#ifndef WINDOWS
  installHandler sigPIPE Ignore Nothing
#endif
  done
