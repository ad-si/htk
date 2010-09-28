-- | A Store a contains an (a) value which is only to be computed once,
-- when it is first needed.
--
-- Perhaps we should use laziness and unsafePerformIO?
module Util.Store(
   Store,
   newStore, -- :: IO (Store a)
   takeStore, -- :: IO a -> Store a -> IO a
   ) where

import Control.Concurrent.MVar

newtype Store a = Store (MVar (Maybe a))

newStore :: IO (Store a)
newStore =
   do
      mVar <- newMVar Nothing
      return (Store mVar)

takeStore :: IO a -> Store a -> IO a
takeStore getA (Store mVar) =
   modifyMVar
      mVar
      (\ aOpt -> case aOpt of
         Just a -> return (aOpt,a)
         Nothing ->
            do
               a <- getA
               return (Just a,a)
         )
