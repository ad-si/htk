{- Interface for calling the zlib library to compress and uncompress files. -}
module ZLib(
   compress,
   ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable


-- returned CString should be freed by Foreign.Marshal.Alloc.free after use.
compress :: CStringLen -> IO CStringLen
compress (cStrIn,lenIn) =
   do
      -- compute length of output buffer using formula in zlib.h
      let
         lenOut = lenIn + lenIn `div` 10 + 12
      
      cStrOut <- mallocBytes lenOut
      (retCode,actualLen) <- alloca
         (\ (lenOutPtr :: Ptr CULong) ->
            do
               poke lenOutPtr (fromIntegral lenOut)
               retCode <- compress0 cStrOut lenOutPtr cStrIn (fromIntegral 
                  lenIn)
               actualLen <- peek lenOutPtr
               return (retCode,actualLen)
            )

      if (retCode /= 0) 
         then
            error ("zlib compress returned " ++ show retCode)
         else
            return ()

      return (cStrOut,fromIntegral actualLen)

foreign import ccall unsafe "zlib.h compress" 
   compress0 :: Ptr CChar -> Ptr CULong -> Ptr CChar -> CULong -> IO CInt