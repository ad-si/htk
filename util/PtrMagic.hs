-- |  This module allows us to store constant Strings directly in the
--  executable as Addr# constants (which are written like Strings except
--  they are followed by a # sign and must also be null terminated, as in 
--  "Hello World\0"#).  
-- 
--  If I think of other useful things to do with the Ptr module I will probably
--  put them here . . .
module PtrMagic(
   readStringFromAddr#,
   ) where

import GHC.Ptr(Ptr(Ptr))

import GlaExts(Addr#)
import CString

readStringFromAddr# :: Addr# -> IO String
readStringFromAddr# addr = peekCString (Ptr addr)


