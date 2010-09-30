{-# LANGUAGE CPP #-}

-- | This module contains flags which control compilation.
module Util.CompileFlags where

isDebug :: Bool
#ifdef DEBUG
isDebug = True
#else
isDebug = False
#endif

uniVersion :: String
uniVersion = "2.2"
