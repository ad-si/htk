{- This defines primitive byte operations, to be used with binary conversion.
   For the present we use the FFI.  There are probably lots of better ways.
   -}

#if (__GLASGOW_HASKELL__ == 602)
{- Sigh.  We need to work around the ghc6.02 hGetBuf bug.
   Actually we don't probably need to for ghc6.02.1, but at the moment I can't
   be bothered to distinguish the two versions. -}
#define FIX_hGetBuf 1
#endif

module Bytes(
   Byte, 
      -- this type is expected to be an instance of Eq, Ord, Num, Bits, 
      -- Integral, Show and contain (at least) the values 0..255.
   Bytes,
      -- an array of values of type Byte.
      -- NB.  The caller is responsible for making sure writes to and from
      -- this array are within bounds.


   putByteToBytes,
      -- :: Byte -> Bytes -> Int -> IO ()
      -- write byte to index.
   getByteFromBytes,
      -- :: Bytes -> Int -> IO Byte

   putBytesToBytes,
      -- :: Bytes -> Int -> Byte -> Int -> Int -> IO ()
      -- putBytesToBytes source sourceIndex dest destIndex length
      --    copies length bytes starting at source[sourceIndex] to
      --    dest[destIndex]
      -- It assumes that the source and destination areas don't overlap.
   hPutByte,
      -- :: Handle -> Byte -> IO ()
   hGetByte,
      -- :: Handle -> IO Byte

   hPutBytes,
      -- :: Handle -> Bytes -> Int -> IO ()
   hGetBytes,
      -- :: Handle -> Int -> IO Bytes
      -- hGetBytes allocates an area, which needs to be
      -- freed using freeBytes.

   -- the following are similar to C's malloc/alloc/realloc/free.
   bytesMalloc,
      -- :: Int -> IO Bytes
   bytesReAlloc,
      -- :: Bytes -> Int -> IO Bytes.
   bytesAlloca,
      -- :: Int -> (Bytes -> IO a) -> IO a
   bytesFree,
      -- :: Bytes -> IO ()

   withBytesAsCChars,
      -- :: Bytes -> (Ptr CChar -> IO a) -> IO a
      -- This gives you access to the contents of Bytes as a Ptr CChar.
      -- The length will be the number of Bytes in the array.
      -- NB.  The Ptr CChar may become invalid (or garbage) after the
      -- function supplied by the caller returns.

   mkBytes,
      -- :: Ptr CChar -> Bytes
   unMkBytes,
      -- :: Bytes -> Ptr CChar
      -- low-level interface (and therefore likely to change)


   compareBytes, -- :: Bytes -> Bytes -> Int -> IO Ordering
      -- Compare two Bytes items up to the given length, in a consistent
      -- way.
   ) where

-- Haskell 98 imports
import IO
import Char

-- FFI imports
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- Other GHC imports.
import Data.Bits(Bits)
#ifdef FIX_hGetBuf
import GHC.Handle
import GHC.IO hiding (hGetBuf)
import GHC.IOBase
import GHC.Base
import Foreign
import Foreign.C
import GHC.Ptr
import Control.Monad
#else
import GHC.IO(hPutBuf,hGetBuf)
#endif

import System.IO.Error
import Control.Exception(Exception(IOException),throw)



-- ----------------------------------------------------------------------
-- The datatypes
-- ----------------------------------------------------------------------

newtype Byte = Byte CUChar deriving (Eq,Ord,Num,Bits,Show,Real,Enum,Integral)

newtype Bytes = Bytes (Ptr CChar)

-- ----------------------------------------------------------------------
-- The exported functions
--  ----------------------------------------------------------------------

putByteToBytes :: Byte -> Bytes -> Int -> IO ()
putByteToBytes (Byte u) (Bytes ptr) i
   = pokeArray (advancePtr ptr i) [fromIntegral u]

getByteFromBytes :: Bytes -> Int -> IO Byte
getByteFromBytes (Bytes ptr) i =
   do
      [c] <- peekArray 1 (advancePtr ptr i)
      return (Byte (fromIntegral c))

putBytesToBytes :: Bytes -> Int -> Bytes -> Int -> Int -> IO ()
putBytesToBytes (Bytes sourcePtr) sourceIndex (Bytes destPtr) destIndex len
   = copyArray (advancePtr destPtr destIndex) 
      (advancePtr sourcePtr sourceIndex) len

hPutByte :: Handle -> Byte -> IO ()
hPutByte handle (Byte u) = hPutChar handle (chr (fromIntegral u))

hGetByte :: Handle -> IO Byte
hGetByte handle =
   do
      char <- hGetChar handle
      return (Byte (fromIntegral (ord char)))

hPutBytes :: Handle -> Bytes -> Int -> IO ()
hPutBytes handle (Bytes ptr) len =
   hPutBuf handle ptr len

hGetBytes ::  Handle -> Int -> IO Bytes
hGetBytes handle len =
   do
      (bytes@(Bytes ptr)) <- bytesMalloc len
      lenRead <- hGetBuf handle ptr len
      if lenRead < len
         then
            do
               bytesFree bytes
               throwEOF handle
         else
            return bytes

bytesMalloc :: Int -> IO Bytes
bytesMalloc i =
   do
      ptr <- mallocBytes i
      return (Bytes ptr)

bytesReAlloc :: Bytes -> Int -> IO Bytes
bytesReAlloc (Bytes ptr1) newLen0 =
   do
      let
         newLen =
         -- work around GHC6 bug
#if (__GLASGOW_HASKELL__ == 600)
            max 1 newLen0
#else
            newLen0
#endif
      ptr2 <- reallocBytes ptr1 newLen
      return (Bytes ptr2)

bytesAlloca :: Int -> (Bytes -> IO a) -> IO a
bytesAlloca len fn = allocaBytes len (\ ptr -> fn (Bytes ptr))

bytesFree :: Bytes -> IO ()
bytesFree (Bytes ptr) = free ptr


withBytesAsCChars :: Bytes -> (Ptr CChar -> IO a) -> IO a
withBytesAsCChars (Bytes ptr) fn = fn ptr



mkBytes :: Ptr CChar -> Bytes
mkBytes = Bytes

unMkBytes :: Bytes -> Ptr CChar
unMkBytes (Bytes ptr) = ptr

-- ----------------------------------------------------------------------
-- Throw an EOF error
-- ----------------------------------------------------------------------
 
throwEOF :: Handle -> IO a
throwEOF handle =
   do
      let
         eofError = IOException (
            mkIOError eofErrorType
               "BinaryIO" (Just handle)
               Nothing
            )
      throw eofError

-- ----------------------------------------------------------------------
-- Compare two Bytes values in an unspecified but consistent way.
-- ----------------------------------------------------------------------

compareBytes :: Bytes -> Bytes -> Int -> IO Ordering
compareBytes (Bytes p1) (Bytes p2) len =
   do
      res <- compareBytesPrim p1 p2 (fromIntegral len)
      return (compare res 0)

foreign import ccall unsafe "string.h memcmp" 
   compareBytesPrim :: Ptr CChar -> Ptr CChar -> CSize -> IO CInt

#ifdef FIX_hGetBuf
-- ----------------------------------------------------------------------
-- Simon Marlow's special version for ghc-6.2.
-- ----------------------------------------------------------------------


fillReadBufferWithoutBlocking :: FD -> Bool -> Buffer -> IO Buffer
fillReadBufferWithoutBlocking fd is_stream
      buf@Buffer{ bufBuf=b, bufRPtr=r, bufWPtr=w, bufSize=size } =
  -- buffer better be empty:
  assert (r == 0 && w == 0) $ do
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoopNoBlock: bytes = " ++ show bytes ++ "\n")
#endif
  res <- readRawBufferNoBlock "fillReadBuffer" fd is_stream b
  		       0 (fromIntegral size)
  let res' = fromIntegral res
#ifdef DEBUG_DUMP
  puts ("fillReadBufferLoopNoBlock:  res' = " ++ show res' ++ "\n")
#endif
  return buf{ bufRPtr=0, bufWPtr=res' }
 
readRawBufferNoBlock :: String -> FD -> Bool -> RawBuffer -> Int -> CInt -> IO CInt
readRawBufferNoBlock loc fd is_stream buf off len = 
  throwErrnoIfMinus1RetryOnBlock loc
	    (read_rawBuffer fd is_stream buf off len)
	    (return 0)

-- -----------------------------------------------------------------------------
-- utils

throwErrnoIfMinus1RetryOnBlock  :: String -> IO CInt -> IO CInt -> IO CInt
throwErrnoIfMinus1RetryOnBlock loc f on_block  = 
  do
    res <- f
    if (res :: CInt) == -1
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfMinus1RetryOnBlock loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
	         then do on_block
                 else throwErrno loc
      else return res

-- ---------------------------------------------------------------------------
-- hGetBuf

-- | 'hGetBuf' @hdl buf count@ reads data from the handle @hdl@
-- into the buffer @buf@ until either EOF is reached or
-- @count@ 8-bit bytes have been read.
-- It returns the number of bytes actually read.  This may be zero if
-- EOF was reached before any data was read (or if @count@ is zero).
--
-- 'hGetBuf' never raises an EOF exception, instead it returns a value
-- smaller than @count@.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGetBuf' will behave as if EOF was reached.

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h ptr count
  | count == 0 = return 0
  | count <  0 = illegalBufferSize h "hGetBuf" count
  | otherwise = 
      wantReadableHandle "hGetBuf" h $ 
	\ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> do
	    bufRead fd ref is_stream ptr 0 count

-- small reads go through the buffer, large reads are satisfied by
-- taking data first from the buffer and then direct from the file
-- descriptor.
bufRead fd ref is_stream ptr so_far count =
  seq fd $ seq so_far $ seq count $ do -- strictness hack
  buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r, bufSize=sz } <- readIORef ref
  if bufferEmpty buf
     then if count > sz  -- small read?
		then do rest <- readChunk fd is_stream ptr count
			return (so_far + rest)
	 	else do mb_buf <- maybeFillReadBuffer fd True is_stream buf
			case mb_buf of
		          Nothing -> return so_far -- got nothing, we're done
		          Just new_buf -> do 
			    writeIORef ref new_buf
		   	    bufRead fd ref is_stream ptr so_far count
     else do 
  	let avail = w - r
	if (count == avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
		return (so_far + count)
	   else do
	if (count < avail)
	   then do 
		memcpy_ptr_baoff ptr raw r (fromIntegral count)
		writeIORef ref buf{ bufRPtr = r + count }
		return (so_far + count)
	   else do
  
	memcpy_ptr_baoff ptr raw r (fromIntegral avail)
	writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
	let remaining = count - avail
	    so_far' = so_far + avail
	    ptr' = ptr `plusPtr` avail

	if remaining < sz
	   then bufRead fd ref is_stream ptr' so_far' remaining
	   else do 

	rest <- readChunk fd is_stream ptr' remaining
	return (so_far' + rest)

readChunk :: FD -> Bool -> Ptr a -> Int -> IO Int
readChunk fd is_stream ptr bytes = loop 0 bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return off
  loop off bytes = do
    r <- fromIntegral `liftM`
           readRawBufferPtr "readChunk" (fromIntegral fd) is_stream 
	   		    (castPtr ptr) off (fromIntegral bytes)
    if r == 0
	then return off
	else loop (off + r) (bytes - r)


maybeFillReadBuffer fd is_line is_stream buf
  = catch 
     (do buf <- fillReadBuffer fd is_line is_stream buf
	 return (Just buf)
     )
     (\e -> do if isEOFError e 
		  then return Nothing 
		  else ioError e)


-----------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn (sz :: Int) = 
	ioException (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 sz [])
			    Nothing)

#endif