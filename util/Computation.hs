{- #########################################################################

MODULE        : Computation
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1998
VERSION       : 0.2
DESCRIPTION   : Various Computations as they are found
                in Java, Tcl etc. An then. on top, the state and
                error monad (Hip Hip Hurray).


   ######################################################################### -}


module Computation (
        Answer,

        Monad(..),
        MonadError(..),
        MonadState(..),

--        STM,
        IOS,

        embedIOS,
        execIOS,

        done,

        ( # ), -- reverse of application

        -- exceptions and handlers
        propagate,
        try, -- in IO
        tryM,
        finally,
        catchall,
        tryUntilOK,
        eitherM,

        -- sequencers
        sequence_,
        sequence,
        (@@),

        -- selectors
        when,  -- when b y means "if b is true, perform y"
        unless,
        incase,

        -- iterators
        forever,
        foreach,
        while,

        -- configure command
        Config, 
        configure,
        config,

        -- The new-style configuration command
        HasConfig(..),

        -- writeLog is now obsolescent.  Debug.debug should be used
        -- instead.
        setLogFile,
        writeLog,
        setLogCommand,

        setErrorHandler,
        getErrorHandler

        ) 
where

import IO
import Maybes -- load this from util if Hugs, in GHC it's part of the 
              -- standard library
import Monad
import IOExts(unsafePerformIO)
import Concurrent

import Debug(debug)

infixr 1 @@
infixr 2 #


-- --------------------------------------------------------------------------
-- Type Definitions
-- --------------------------------------------------------------------------

type Answer a = Either IOError a


-- --------------------------------------------------------------------------
-- Error Monad
-- --------------------------------------------------------------------------

class Monad m => MonadError m where
        raise :: IOError -> m a
        trap  :: m a -> (IOError -> m a) -> m a


instance MonadError IO where
        raise e = do {
                debug ("RAISED EXCP: " ++ (show e) ++ "\n");
                ioError e
                }
        trap = catch


-- --------------------------------------------------------------------------
-- State Monad
-- --------------------------------------------------------------------------

class Monad m => MonadState m s where
-- MonadState is only used by www/HtmlKernel as far as I can see,
-- with the instance IOS.
        update     :: (s -> s) -> m s -- only seems to be used internally
        getStateM  :: m s
        setStateM  :: s -> m ()
        getStateM   = update id
        setStateM s = update (const s) >> done


-- --------------------------------------------------------------------------
-- State + Error + IO Monad
-- --------------------------------------------------------------------------

newtype IOS s a = IOS (s -> IO (s,a))
-- An IOS s a is basically an IO a with some additional state 
-- encoded in a pure fashion in s.

instance Functor (IOS s) where
   fmap f c = c >>= return . f

instance Monad (IOS s) where
   return v = IOS (\s -> return (s,v))
   (IOS f) >>= k = -- k :: [ result of f action ] -> IOS f'
      IOS (\ s ->
         do -- this do is an IO action
            (s',a) <- (f s) -- compute new state and do IO
            let 
                  (IOS f') = k a -- compute new IOS action
               in f' s'
         )


instance MonadState (IOS s) s where
   update f = IOS (\s -> return (f s, s))
   -- So update maps the additional state, getStateM reads it, setStateM
   -- sets it.   

instance MonadError (IOS s) where
   raise e  = IOS (\s -> raise e) 
   trap (IOS f) h = IOS (\s -> trap (f s) (handle h s))
      where handle h s e = let (IOS f') = h e in f' s

embedIOS :: IO a -> IOS s a
embedIOS c = IOS (\s -> c >>= \v -> return (s,v))

execIOS :: IOS s a -> s -> IO (s,a)
execIOS (IOS f) s = f s

{- this kind of state and IO monad throws away the changed state in
   presence of errors and passes, to the error handler, the state initially
   passed to trap.
-}


-- --------------------------------------------------------------------------
-- Done
-- --------------------------------------------------------------------------

done :: Monad m => m ()
done = return ()


-- --------------------------------------------------------------------------
-- Method Application
-- --------------------------------------------------------------------------

( # ) :: a -> (a -> b) -> b
o # f = f o

        
-- --------------------------------------------------------------------------
-- IOError Handling
-- --------------------------------------------------------------------------

tryM :: MonadError m => m a -> m (Answer a) 
tryM p = trap (p >>= (return . Right)) (return . Left)
        
finally ::  MonadError m => m a -> m b -> m b
finally c1 c2 = tryM c1 >> c2

catchall ::  MonadError m => m a -> m a -> m a
catchall c1 c2 = trap c1 (\_ -> c2)

propagate :: MonadError m => (Answer a) -> m a
propagate (Left e) = raise e
propagate (Right v) = return v

tryUntilOK :: MonadError m => m a -> m a 
tryUntilOK c = catchall c (tryUntilOK c)

eitherM :: MonadError m => m a -> (IOError -> m b) -> (a -> m b) -> m b
eitherM c f s = do {ans <- tryM c; either f s ans}


-- --------------------------------------------------------------------------
-- Derived Control Abstractions: Sequence
-- --------------------------------------------------------------------------

(@@) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f @@ g  = \a -> (f a) >>= g


-- --------------------------------------------------------------------------
-- Derived Control Abstractions: Iteration
-- --------------------------------------------------------------------------

forever :: Monad m => m a -> m ()
forever c  = sequence_ (repeat c)     -- x >> forever x

foreach :: Monad m => [a] -> (a -> m b) -> m ()
foreach el c = sequence_ (map c el)   -- mapM c el



-- --------------------------------------------------------------------------
-- Derived Control Abstractions: Selection
-- --------------------------------------------------------------------------

incase :: Maybe a -> (a -> IO b) -> IO ()
incase Nothing f = done
incase (Just a) f = do {f a; done}

-- --------------------------------------------------------------------------
-- Loops
-- --------------------------------------------------------------------------

while :: Monad m => m a -> (a -> Bool) -> m a
while c p = c >>= \x -> if (p x) then while c p else return x 


-- --------------------------------------------------------------------------
-- Configuration Options 
-- -------------------------------------------------------------------------- 

type Config w = w -> IO w

configure :: w -> [Config w] -> IO w
configure w [] = return w
configure w (c:cl) = do {w' <- c w; configure w' cl}

config :: IO () -> Config w
config f w = f >> return w


-- --------------------------------------------------------------------------
-- New-style configuration
-- Where HasConfig is defined you can type
--     option1  $$ option2 $$ ... $$ initial_configuration
-- -------------------------------------------------------------------------- 

class HasConfig option configuration where
   ($$) :: option -> configuration -> configuration

   configUsed :: option -> configuration -> Bool
   -- In some implementations (EG a text-only 
   -- implementation of the GraphDisp interface)
   -- we may create default configurations in which $$ simply
   -- ignores the option.  In such cases configUsed should return
   -- False.

infixr 0 $$
-- This makes $$ have fixity like $. 

-- --------------------------------------------------------------------------
-- Logfile Commands
-- --------------------------------------------------------------------------

setLogFile :: Maybe Handle -> IO ()
setLogFile Nothing   =  setLogCommand (const done)
setLogFile (Just ch) =  setLogCommand (\s -> hPutStr ch s >> hPutStr ch "\n")

setLogCommand :: (String -> IO ()) -> IO ()
setLogCommand cmd = do {
        swapMVar logfile cmd;
        done
        }       

writeLog :: String -> IO ()
writeLog str = do {
        cmd <- readMVar logfile;
        cmd str 
        }


logfile :: MVar (String -> IO ())
logfile = unsafePerformIO(newMVar (const done))


-- --------------------------------------------------------------------------
-- Error Handler Commands
-- --------------------------------------------------------------------------

setErrorHandler :: (IOError -> IO ()) -> IO ()
setErrorHandler cmd = 
   do
      swapMVar errhandler cmd
      done    

getErrorHandler :: IO (IOError -> IO ())
getErrorHandler = readMVar errhandler

errhandler :: MVar (IOError -> IO ())
errhandler = unsafePerformIO(newMVar raise)

