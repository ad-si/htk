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

        done,

        ( # ), -- reverse of application

        -- exceptions and handlers
        propagate,
        try, -- in IO
        tryUntilOK,
        raise,

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

        -- Results with error messages attached.
        -- Error messages
        WithError,

        hasError, -- :: String -> WithError a
        -- pass on an error

        hasValue, -- :: a -> WithError a
        -- pass on a value

        fromWithError, -- :: WithError a -> Either String a
        -- unpack a WithError

        mapWithError, -- :: (a -> b) -> WithError a -> WithError b
        mapWithError', -- :: (a -> WithError b) -> WithError a -> WithError b
        mapWithErrorIO',
        -- :: (a -> IO (WithError b)) -> WithError a -> IO (WithError b)
        pairWithError, -- :: WithError a -> WithError b -> WithError (a,b)
        -- we concatenate the errors, inserting a newline between them if 
        -- there are two.
        coerceWithError, -- :: WithError a -> a
        -- get out result or throw error.

        concatWithError, -- :: [WithError a] -> WithError [a]
        -- like pair but using lists.

        swapIOWithError, -- :: WithError (IO a) -> IO (WithError a)
        -- Intended for use on result of mapWithError, for example.
        ) 
where

import IO hiding (try,catch)
import Maybes -- load this from util if Hugs, in GHC it's part of the 
              -- standard library
import Monad

import Concurrent
import Exception

import Debug(debug)

infixr 2 #


-- --------------------------------------------------------------------------
-- Type Definitions
-- --------------------------------------------------------------------------

type Answer a = Either Exception a

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
-- IOError and Exception Handling
-- --------------------------------------------------------------------------

raise :: IOError -> IO a
raise e = 
   do
      debug ("RAISED EXCP: " ++ (show e) ++ "\n")
      ioError e 

propagate :: Answer a -> IO a
propagate (Left e) = throw e
propagate (Right v) = return v

catchall :: IO a -> IO a -> IO a
catchall c1 c2 = Exception.catch c1 (\ _ -> c2)

tryUntilOK :: IO a -> IO a 
tryUntilOK c = catchall c (tryUntilOK c)

-- --------------------------------------------------------------------------
-- Values paired with error messages
-- --------------------------------------------------------------------------

data WithError a = 
      Error String 
   |  Value a -- error or result

hasError :: String -> WithError a
hasError str = Error str

hasValue :: a -> WithError a
hasValue a = Value a

fromWithError :: WithError a -> Either String a
fromWithError (Error s) = Left s
fromWithError (Value a) = Right a

mapWithError :: (a -> b) -> WithError a -> WithError b
mapWithError f (Error e) = Error e
mapWithError f (Value x) = Value (f x)

mapWithError' :: (a -> WithError b) -> WithError a -> WithError b
mapWithError' f (Error e) = Error e
mapWithError' f (Value a) = f a

mapWithErrorIO' :: (a -> IO (WithError b)) -> WithError a -> IO (WithError b)
mapWithErrorIO' f (Error e) = return (Error e)
mapWithErrorIO' f (Value a) = f a

pairWithError :: WithError a -> WithError b -> WithError (a,b)
-- we concatenate the errors, inserting a newline between them if there are two.
pairWithError (Value a) (Value b) = Value (a,b)
pairWithError (Error e) (Value b) = Error e
pairWithError (Value a) (Error f) = Error f
pairWithError (Error e) (Error f) = Error (e++"\n"++f)

-- coerce or raise error
coerceWithError :: WithError a -> a
coerceWithError (Value a) = a
coerceWithError (Error err) = error err

concatWithError :: [WithError a] -> WithError [a]
concatWithError withErrors =
   foldl
      (\ wEsf wE -> mapWithError (uncurry (:)) (pairWithError wE wEsf))  
      (Value [])
      withErrors

swapIOWithError :: WithError (IO a) -> IO (WithError a)
swapIOWithError (Error e) = return (Error e)
swapIOWithError (Value act) =
   do
      v <- act
      return (Value v)

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

