{- This module implements a simple storage system in which data 
   can only be added, and is retrieved by a handle. 
   It is assumed that all the functions will be used as if in a single 
   thread, so no locking is done. -}
module SimpleStore(
   SimpleStore,
   SimpleStorePtr, -- instance of Read/Show
   -- An instance CacheTable.LookupTable SimpleStore SimpleStorePtr String
   -- is defined.
   ) where

import IO

import IOExts

import Computation
import WBFiles
import QuickReadShow
import CacheTable

-- --------------------------------------------------------------
-- Types
-- --------------------------------------------------------------

data SimpleStore = SimpleStore {
   handle :: Handle,
   atEnd :: IORef Bool -- if true, the file pointer is at the end of the file.
   }

newtype SimpleStorePtr = SimpleStorePtr Integer

instance QuickRead SimpleStorePtr where
   quickRead = WrapRead (\ i -> SimpleStorePtr i)

instance QuickShow SimpleStorePtr where
   quickShow = WrapShow (\ (SimpleStorePtr i) -> i)

instance Eq SimpleStorePtr where
   (==) (SimpleStorePtr p1) (SimpleStorePtr p2) = (==) p1 p2
   (/=) (SimpleStorePtr p1) (SimpleStorePtr p2) = (/=) p1 p2

instance Ord SimpleStorePtr where
   compare (SimpleStorePtr p1) (SimpleStorePtr p2) = compare p1 p2

-- --------------------------------------------------------------
-- Writing a String to a file.
-- We null-terminate.  The null character itself is stored as
-- "\x01\x00"; "\x01" is stored as "\x01\x01".  Other characters
-- are left unchanged.  
-- --------------------------------------------------------------

hWriteStr :: Handle -> String -> IO ()
hWriteStr handle str = 
   let 
      hP = hPutChar handle
      hW str =
         case str of
            [] -> hP '\x00'
            ch : rest ->
               do
                  case ch of
                     '\x00' ->
                        do
                           hP '\x01'
                           hP '\x00'
                     '\x01' ->
                        do
                           hP '\x01'
                           hP '\x01'
                     _ -> hP ch
                  hW rest
   in
      hW str

hReadStr :: Handle -> IO String
hReadStr handle =
   let
      hG = hGetChar handle
      hR :: String -> IO String
      hR acc =
         do
            ch <- hG
            case ch of
               '\x00' -> return (reverse acc)
               '\x01' ->
                  do
                     ch2 <- hG
                     case ch2 of
                        '\x00' -> hR ('\x00':acc)
                        '\x01' -> hR ('\x01':acc)
               ch -> hR (ch:acc)
   in
      hR []

-- --------------------------------------------------------------
-- The instance
-- --------------------------------------------------------------

instance LookupTable SimpleStore SimpleStorePtr String where
   openTable =
      do
         fpath <- getSimpleStore
         handle <- openFile fpath ReadWriteMode 
         atEnd <- newIORef False 
            -- may be false if we are using an existing file.
         return (SimpleStore {handle = handle,atEnd = atEnd})

   flushTable (SimpleStore {handle = handle}) = hFlush handle

   putTable (SimpleStore {handle = handle,atEnd = atEnd}) str =
      do
         isAtEnd <- readIORef atEnd
         if isAtEnd
            then
               done
            else
               hSeek handle SeekFromEnd 0
         currentSize <- hFileSize handle
         hWriteStr handle str
         writeIORef atEnd True
         return (SimpleStorePtr currentSize)
         
   getTable (SimpleStore {handle = handle,atEnd = atEnd}) 
         (SimpleStorePtr posn) =
      do
         hSeek handle AbsoluteSeek posn
         writeIORef atEnd False
         hReadStr handle




   