{- This module contains a patched version of hGetChar (mostly due to
   Simon Marlow) to fix a bug in ghc 5.04.3.  See archives of 
   glasgow-haskell-users for 29th-30th April 2003.  Hopefully this patch
   will go away when ghc 5.04.4 arrives. 

   NB.  This hack assumes the handle is block-buffered. -}
module HGetCharHack(
   hackhGetChar,
   )

where

import GHC.Enum
import GHC.Base
import GHC.IOBase
import GHC.Handle	-- much of the real stuff is in here
import GHC.Real
import GHC.Num
import GHC.Show
import GHC.List
import GHC.Exception    ( ioError, catch )
import GHC.Conc


hackhGetChar :: Handle -> IO Char
hackhGetChar handle =
   wantReadableHandle "hGetChar" handle $ \handle_ -> 
      do
         let 
            fd = haFD handle_
            ref = haBuffer handle_

         buf <- readIORef ref
         if not (bufferEmpty buf)
            then 
               hGetcBuffered fd ref buf
            else 
               do
                  -- buffer is empty.
                  new_buf <- fillReadBuffer fd True (haIsStream handle_) buf
                  hGetcBuffered fd ref new_buf

hGetcBuffered fd ref buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w }
 = do (c,r) <- readCharFromBuffer b r
      let new_buf | r == w    = buf{ bufRPtr=0, bufWPtr=0 }
	          | otherwise = buf{ bufRPtr=r }
      writeIORef ref new_buf
      return c
