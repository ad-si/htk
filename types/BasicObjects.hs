{- This file describes the basic untyped objects stored in the 
   repository.  Since they are versioned, they need to be
   wrapped in Versioned; EG (Versioned SimpleFile) represents
   a file (with no attributes.
   -}
module BasicObjects(
   SimpleFile, -- a file with no frills.
   newSimpleFile, -- :: Repository -> Versioned SimpleFile
   getSimpleFileName, -- :: Versioned SimpleFile -> IO FilePath
   -- use dirty after this file is updated.
   ) where

data SimpleFile = SimpleFile 
   -- Yes, that's right, it contains no data.  The file
   -- itself is accessed by direct access in CVS's own store.