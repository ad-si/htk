{- Various generic functions to be used with the Binary module. -}
module BinaryUtils(
   mapWrite, -- :: HasBinary b m => (a -> b) -> (WriteBinary m -> a -> m ())
   mapRead, -- :: (Monad m,HasBinary b m) => (b -> a) -> (ReadBinary m -> m a)
   mapWriteIO, 
      -- :: (HasBinary b m,MonadIO m) 
      -- => (a -> IO b) -> (WriteBinary m -> a -> m ())
   mapReadIO,
      -- :: (HasBinary b m,MonadIO m) 
      -- => (b -> IO a) -> (ReadBinary m -> m a)

   ArgMonad, 
      -- A type for encoding a monadic action which requires an
      -- extra argument (of type "arg").
      --    ArgMonad arg m
      -- is an instance of Monad (and Functor), if m is.
      --
      -- ArgMonad is intended as a way of writing instances of Binary which
      -- require a bit of context.  Thus you would write something like
      --
      -- instance Monad m => HasBinary MyType1 (ArgMonad context m) where
      --    writeBinary wb (MyType1 v1 v2) = mkArgMonad 
      --       (\ context ->
      --           do 
      --              runArgMonad context (writeBinary rb v1)
      --                 -- this is something which is automatically
      --                 -- an instance of HasBinary for (ArgMonad context m)
      --                 -- like the standard types.
      --              runArgMonad context (writeBinary rb (f v2 context))
      --                 -- this is something which needs to be changed by
      --                 -- f, using context, to give a suitable instance.
      --           )
      --  (and likewise for readBinary).
      --
      --
      -- Then if you want to encode MyType2, containing MyType1, and providing
      -- this context, you could write
      --
      -- instance Monad m => HasBinary MyType2 m where
      --    writeBinary wb (MyType2 v3 v4) = 
      --       do
      --          context <- ...
      --          writeBinary wb v3 -- encoding v3 doesn't need context
      --          runArgMonad context 
      --             (writeBinary (writeBinaryToArgMonad wb) v4)
      --             -- encoding v4 does need context.
   mkArgMonad, -- :: (arg -> m a) -> ArgMonad arg m a
   toArgMonad, -- :: m a -> ArgMonad arg m a
   runArgMonad, -- :: arg -> ArgMonad arg m a -> m a

   writeBinaryToArgMonad, -- :: WriteBinary m -> WriteBinary (ArgMonad arg m)
   readBinaryToArgMonad, -- :: ReadBinary m -> ReadBinary (ArgMonad arg m)


   WrappedBinary(..),
      -- a wrapper for instances of HasBinary _ IO.
   hWriteWrappedBinary, -- :: Handle -> WrappedBinary -> IO ()

   WrapBinary(..),
      -- more general wrapped for any monad. 
   ) where

import IO(Handle)

-- GHC imports
import Control.Monad.Trans

-- our imports
import Binary

-- ----------------------------------------------------------------------
-- Mapping HasBinary instances
-- ----------------------------------------------------------------------

-- Two functions for constructing writeBinary/readBinary functions given
-- a conversion function.  Yes I know this is trivial, but it's also
-- VERY common.
mapWrite :: HasBinary b m => (a -> b) -> (WriteBinary m -> a -> m ())
mapWrite  fn wb a = writeBin wb (fn a)

mapRead :: (Monad m,HasBinary b m) => (b -> a) -> (ReadBinary m -> m a)
mapRead fn rb =
   do
      b <- readBin rb
      return (fn b)

-- Equivalents which rely allow IO functions, of course only for monads
-- which admit IO actions.
mapWriteIO :: (HasBinary b m,MonadIO m) 
   => (a -> IO b) -> (WriteBinary m -> a -> m ())
mapWriteIO fn wb a = 
   do
      b <- liftIO (fn a)
      writeBin wb b

mapReadIO :: (HasBinary b m,MonadIO m) 
   => (b -> IO a) -> (ReadBinary m -> m a)
mapReadIO fn rb =
   do
      b <- readBin rb
      liftIO (fn b)


-- ----------------------------------------------------------------------
-- Creating HasBinary instances that need extra information about their
-- context
-- ----------------------------------------------------------------------

newtype ArgMonad arg m a = ArgMonad (arg -> m a)

mkArgMonad :: (arg -> m a) -> ArgMonad arg m a
mkArgMonad = ArgMonad

toArgMonad :: m a -> ArgMonad arg m a
toArgMonad act = ArgMonad (const act)

writeBinaryToArgMonad :: WriteBinary m -> WriteBinary (ArgMonad arg m)
writeBinaryToArgMonad (WriteBinary {
   writeByte = writeByte1,writeBytes = writeBytes1}) =

   let
      writeByte2 byte = toArgMonad (writeByte1 byte)
      writeBytes2 bytes int = toArgMonad (writeBytes1 bytes int)
   in
      WriteBinary {writeByte = writeByte2,writeBytes = writeBytes2}

readBinaryToArgMonad :: ReadBinary m -> ReadBinary (ArgMonad arg m)
readBinaryToArgMonad (ReadBinary {
   readByte = readByte1,readBytes = readBytes1}) =

   let
      readByte2 = toArgMonad readByte1
      readBytes2 len = toArgMonad (readBytes1 len)
   in
      ReadBinary {readByte = readByte2,readBytes = readBytes2}

runArgMonad :: arg -> ArgMonad arg m a -> m a
runArgMonad arg (ArgMonad fn) = fn arg

instance Functor m => Functor (ArgMonad arg m) where 
   fmap mapFn (ArgMonad fn) =
      let
         fn2 arg = fmap mapFn (fn arg)
      in
         ArgMonad fn2 

instance Monad m => Monad (ArgMonad arg m) where
   (>>=) (ArgMonad fn1) getArgMonad =
      let
         fn arg =
            do
               v1 <- fn1 arg
               let
                  (ArgMonad fn2) = getArgMonad v1
               fn2 arg
      in
         ArgMonad fn

   return v = ArgMonad (const (return v))

   fail s = ArgMonad (const (fail s))

instance MonadIO m => MonadIO (ArgMonad arg m) where
   liftIO act = ArgMonad (\ arg -> liftIO act)

-- ----------------------------------------------------------------------
-- A wrapper for instances of Binary.  This can be written, but not
-- read (since we wouldn't know what type to decode).
-- ----------------------------------------------------------------------

data WrappedBinary = 
   forall v . HasBinary v IO => WrappedBinary v

hWriteWrappedBinary :: Handle -> WrappedBinary -> IO ()
hWriteWrappedBinary handle (WrappedBinary v) = hWrite handle v


-- ----------------------------------------------------------------------
-- More generally we provide a wrapped type for each monad, and a way
-- of writing it.  Of course we have to leave the method for reading it
-- undefined
-- ----------------------------------------------------------------------
 
data WrapBinary m = forall v . HasBinary v m => WrapBinary v

instance HasBinary (WrapBinary m) m where
   writeBin wb (WrapBinary v) = writeBin wb v

   readBin = error "BinaryUtils: can't read a general wrapped binary type"