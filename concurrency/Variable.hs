{- #########################################################################

MODULE        : Variable
AUTHOR        : Walter Norzel, 
                Einar W. Karlsen  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Mutable variables with class operations - sic!


   ######################################################################### -}


module Variable (
        Variable(..),
        Synchronized(..),

        applyVar,

        MVar,
        newEmptyMVar,
        newMVar,
        takeMVar,
        putMVar,
        swapMVar,

        newPVar,
        PVar

        ) where

import Thread
import Concurrent
import Lock
import Debug(debug,(@:))

-- --------------------------------------------------------------------------
-- Class Mutable Variable
-- --------------------------------------------------------------------------

class Variable v a where
-- the assumption seems to be that unlike MVars, Variables always
-- have something in them.
   updVar          :: v a -> (a -> IO (a,b)) -> IO b
   -- update Var with action
   setVar          :: v a -> a -> IO ()
   -- replace value in Var with this value
   getVar          :: v a -> IO a
   -- get value in Var, leaving it full
   changeVar       :: v a -> (a -> IO a) -> IO ()
   -- like updVar
   withVar         :: v a -> (a -> IO b) -> IO b
   -- take action based on contents of Var
   updVar'         :: v a -> (a -> (a,b)) -> IO b
   -- like updVar  
   changeVar'      :: v a -> (a -> a) -> IO ()
   -- like changeVar
   withVar'        :: v a -> (a -> b) -> IO b
   -- like withVar
   setVar var val   = changeVar' var (\_ -> val)
   getVar var       = withVar' var id
   changeVar var c  = updVar var (\v -> c v >>= \r -> return (r,()))
   withVar var c    = updVar var (\v -> c v >>= \r -> return (v,r))
   updVar' var f    = updVar var (return . f)
   changeVar' var f = updVar' var (\v -> (f v,()))
   withVar' var f   = updVar' var (\v -> (v,f v))          


applyVar :: Variable v a => v a -> (a -> a) -> IO a
applyVar v f = updVar' v (\x -> let x' = f x in (x',x')) 


-- --------------------------------------------------------------------------
-- Mutable MVars
-- --------------------------------------------------------------------------

instance Variable MVar a where
        setVar mv val = do {takeMVar mv; "70" @: putMVar mv val;}
        getVar = readMVar
        updVar mv f = do 
                v <- takeMVar mv
                ans <- try (f v)
                case ans of
                        (Right (v',r)) -> do {"71" @: putMVar mv v'; return r}
                        (Left e) -> do {"72" @: putMVar mv v; raise e}  
        updVar' mvar f = do {
                v <- takeMVar mvar; 
                let (v',r) = f v in do {
                        "73" @: putMVar mvar v';
                        return r
                        }
                }

instance Synchronized (MVar a) where
        synchronize mv c = do
                v <- takeMVar mv
                ans <- try c
                "74" @: putMVar mv v
                propagate ans

-- --------------------------------------------------------------------------
-- Protected Variables
-- --------------------------------------------------------------------------

newtype PVar a = PVar (MVar a)

newPVar :: a -> IO (PVar a)
newPVar val = newMVar val >>= return . PVar     

instance Variable PVar a where
        setVar (PVar mv) val = setVar mv val
        getVar (PVar mv) = getVar mv
        updVar (PVar mvar) cmd = updVar mvar cmd
        updVar' (PVar mvar) f = updVar' mvar f

instance Synchronized (PVar a) where
        synchronize (PVar mv) = synchronize mv

instance Eq (PVar a) where
        (PVar mv1) == (PVar mv2) = mv1 == mv2
