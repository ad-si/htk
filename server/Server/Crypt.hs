{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module encapsulates the Unix crypt function to provide a means of
-- verifying passwords.
module Server.Crypt(
   verifyPassword,
   ) where

import Foreign.C.String

import System.IO.Unsafe

import Events.Synchronized

import Reactor.BSem

-- Reimplementation of crypt(3) which doesn't require libcrypt.
foreign import ccall unsafe "dipperstein_crypt.h dipperstein_crypt" crypt0
   :: CString -> CString -> IO CString


-- Our basic interface.
crypt1 :: String -> String -> IO String
crypt1 key salt =
   synchronize theBSem (
      withCString key (\ keyCString ->
         withCString salt (\ saltCString ->
            do
               encryptedCString <- crypt0 keyCString saltCString
               peekCString encryptedCString
            )
         )
      )

-- This locks access to crypt.  It's necessary because the return value of
-- crypt0 is statically allocated.
theBSem :: BSem
theBSem = unsafePerformIO newBSem
{-# NOINLINE theBSem #-}

-- Returns True if the password (first argument) matches the encrypted
-- password (second arg).
verifyPassword :: String -> String -> IO Bool
verifyPassword password encrypted =
   case encrypted of
      (salt1:salt2:_) ->
         do
            encrypted2 <- crypt1 password [salt1,salt2]
            return (encrypted == encrypted2)
      _ -> return False

