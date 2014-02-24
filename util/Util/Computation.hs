{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description : Miscellaneous Monads, in particular 'Computation.WithError'.
module Util.Computation (
        Answer,

        done,

        ( # ), -- reverse of application

        -- * exceptions and handlers
        propagate,
        try, -- re-export from Control.Exception
        tryUntilOK,
        raise,

        -- * selectors
        when, -- re-export from Control.Monad
        unless, -- re-export from Control.Monad
        incase,

        -- * iterators
        forever, -- re-export from Control.Monad
        foreverUntil,
        foreach,
        while,

        -- * configure command
        Config,
        configure,
        config,

        -- * The new-style configuration command
        HasConfig(..),

        -- * Returning results or error messages.
        WithError,

        hasError, -- :: String -> WithError a
        -- pass on an error

        hasValue, -- :: a -> WithError a
        -- pass on a value

        fromWithError, -- :: WithError a -> Either String a
        -- unpack a WithError
        fromWithError1, -- :: a -> WithError a -> a
        -- simpler form.
        toWithError, -- :: Either String a -> WithError a
        -- pack a WithError
        isError, -- :: WithError a -> Bool
        -- returns True if this value indicates an error.

        mapWithError, -- :: (a -> b) -> WithError a -> WithError b
        mapWithError', -- :: (a -> WithError b) -> WithError a -> WithError b
        mapWithErrorIO,
        -- :: (a -> IO b) -> WithError a -> IO (WithError b)
        mapWithErrorIO',
        -- :: (a -> IO (WithError b)) -> WithError a -> IO (WithError b)
        pairWithError, -- :: WithError a -> WithError b -> WithError (a,b)
        -- we concatenate the errors, inserting a newline between them if
        -- there are two.
        listWithError, -- :: [WithError a] -> WithError [a]
        coerceWithError, -- :: WithError a -> a
        coerceWithErrorIO, -- :: WithError a -> IO a
        -- get out result or throw error.
        -- The second throws the error immediately.
        coerceWithErrorStringIO, -- :: String -> WithError a -> IO a
        -- Like coerceWithErrorIO but also takes a String, which will
        -- be included in the eventual error message.

        coerceWithErrorOrBreakIOPrefix,
           -- :: String -> (String -> a) -> WithError a -> IO a
        coerceWithErrorOrBreakPrefix,
           -- :: String -> (String -> a) -> WithError a -> a

        MonadWithError(..),
        -- newtype which wraps a monadic action returning a WithError a.
        -- This is itself an instance of Monad, allowing functions defined
        -- on monads, such as mapM, work on them.
        monadifyWithError, -- :: Monad m => WithError a -> MonadWithError m a
        toMonadWithError, -- :: Monad m => m a -> MonadWithError m a

        coerceWithErrorOrBreak, -- :: (String -> a) -> WithError a -> a
        -- coerce or use the supplied break function (to be used with
        -- ExtendedPrelude.addFallOut)

        coerceWithErrorOrBreakIO, -- :: (String -> a) -> WithError a -> IO a
        -- coerce or use the supplied break function (to be used with
        -- ExtendedPrelude.addFallOut)
        -- The value is evaluated immediately.

        concatWithError, -- :: [WithError a] -> WithError [a]
        -- like pair but using lists.

        swapIOWithError, -- :: WithError (IO a) -> IO (WithError a)
        -- Intended for use on result of mapWithError, for example.

        exceptionToError,
        -- :: (Exception -> Maybe String) -> IO a -> IO (WithError a)
        -- Exception wrapper that turns those exceptions which map to
        -- (Just message) into an error.
        )
where

import Control.Applicative
import Control.Monad

import Control.Exception

import Util.Debug(debug)

infixr 2 #


-- --------------------------------------------------------------------------
-- Type Definitions
-- --------------------------------------------------------------------------

type Answer a = Either SomeException a

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
catchall c1 c2 = Control.Exception.catch c1 (\ (_ :: SomeException) -> c2)

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

toWithError :: Either String a -> WithError a
toWithError (Left s) = Error s
toWithError (Right a) = Value a

isError :: WithError a -> Bool
isError (Error _) = True
isError (Value _) = False

fromWithError :: WithError a -> Either String a
fromWithError (Error s) = Left s
fromWithError (Value a) = Right a

fromWithError1 :: a -> WithError a -> a
fromWithError1 _ (Value a) = a
fromWithError1 a (Error _) = a

mapWithError :: (a -> b) -> WithError a -> WithError b
mapWithError f (Error e) = Error e
mapWithError f (Value x) = Value (f x)

mapWithError' :: (a -> WithError b) -> WithError a -> WithError b
mapWithError' f (Error e) = Error e
mapWithError' f (Value a) = f a


mapWithErrorIO :: (a -> IO b) -> WithError a -> IO (WithError b)
mapWithErrorIO f (Error e) = return (Error e)
mapWithErrorIO f (Value a) =
   do
      b <- f a
      return (Value b)

mapWithErrorIO' :: (a -> IO (WithError b)) -> WithError a -> IO (WithError b)
mapWithErrorIO' f (Error e) = return (Error e)
mapWithErrorIO' f (Value a) = f a

pairWithError :: WithError a -> WithError b -> WithError (a,b)
-- we concatenate the errors, inserting a newline between them if there are two.
pairWithError (Value a) (Value b) = Value (a,b)
pairWithError (Error e) (Value b) = Error e
pairWithError (Value a) (Error f) = Error f
pairWithError (Error e) (Error f) = Error (e++"\n"++f)

listWithError :: [WithError a] -> WithError [a]
listWithError awes =
   foldr
      (\ awe awes ->
         mapWithError
            (\ (a,as) -> a:as)
            (pairWithError awe awes)
         )
      (hasValue [])
      awes

-- coerce or raise error
coerceWithError :: WithError a -> a
coerceWithError (Value a) = a
coerceWithError (Error err) = error err

coerceWithErrorIO :: WithError a -> IO a
coerceWithErrorIO (Value a) = return a
coerceWithErrorIO (Error err) = error err

coerceWithErrorStringIO :: String -> WithError a -> IO a
coerceWithErrorStringIO _ (Value a) = return a
coerceWithErrorStringIO mess (Error err) =
   error ("coerceWithErrorString " ++ mess ++ ": " ++ err)

-- | coerce or use the supplied break function (to be used with
-- 'ExtendedPrelude.addFallOut')
-- The value is evaluated immediately.
coerceWithErrorOrBreakIO :: (String -> a) -> WithError a -> IO a
coerceWithErrorOrBreakIO = coerceWithErrorOrBreakIOPrefix ""

-- | coerce or use the supplied break function (to be used with
-- 'ExtendedPrelude.addFallOut')
--
-- The first argument is prepended to any error message.
-- The value is evaluated immediately.
coerceWithErrorOrBreakIOPrefix
   :: String -> (String -> a) -> WithError a -> IO a
coerceWithErrorOrBreakIOPrefix errorPrefix breakFn aWe =
   do
      let
         a = coerceWithErrorOrBreakPrefix errorPrefix breakFn aWe
      seq a (return a)

-- | coerce or use the supplied break function (to be used with
-- 'ExtendedPrelude.addFallOut')
coerceWithErrorOrBreak :: (String -> a) -> WithError a -> a
coerceWithErrorOrBreak = coerceWithErrorOrBreakPrefix ""


-- | coerce or use the supplied break function (to be used with
-- 'ExtendedPrelude.addFallOut')
--
-- The first argument is prepended to any error message.
coerceWithErrorOrBreakPrefix :: String -> (String -> a) -> WithError a -> a
coerceWithErrorOrBreakPrefix errorPrefix breakFn (Value a) = a
coerceWithErrorOrBreakPrefix errorPrefix breakFn (Error s)
   = breakFn (errorPrefix ++ s)

concatWithError :: [WithError a] -> WithError [a]
concatWithError withErrors =
   foldr
      (\ wE wEsf -> mapWithError (uncurry (:)) (pairWithError wE wEsf))
      (Value [])
      withErrors

swapIOWithError :: WithError (IO a) -> IO (WithError a)
swapIOWithError (Error e) = return (Error e)
swapIOWithError (Value act) =
   do
      v <- act
      return (Value v)

exceptionToError :: Exception e => (e -> Maybe String) -> IO a -> IO (WithError a)
exceptionToError testFn action =
   catchJust
      testFn
      (do
          val <- action
          return (hasValue val)
      )
      (\ str -> return (hasError str))

instance Functor WithError where
   fmap aToB aWE = case aWE of
      Value a -> Value (aToB a)
      Error e -> Error e

instance Applicative WithError where
   pure = return
   (<*>) = ap

instance Monad WithError where
   return v = hasValue v
   (>>=) aWE toBWe =
      mapWithError' toBWe aWE
   fail s = hasError s

newtype MonadWithError m a = MonadWithError (m (WithError a))

instance Monad m => Functor (MonadWithError m) where
   fmap f (MonadWithError a) = MonadWithError $ liftM (fmap f) a

instance Monad m => Applicative (MonadWithError m) where
   pure = return
   (<*>) = ap

instance Monad m => Monad (MonadWithError m) where
   return v = MonadWithError (return (Value v))
   (>>=) (MonadWithError act1) getAct2 =
      MonadWithError (
         do
            valWithError <- act1
            case valWithError of
               Value v ->
                  let
                     (MonadWithError act2) = getAct2 v
                  in
                     act2
               Error s -> return (Error s)
         )
   fail s = MonadWithError (return (Error s))

monadifyWithError :: Monad m => WithError a -> MonadWithError m a
monadifyWithError we = MonadWithError (return we)

toMonadWithError :: Monad m => m a -> MonadWithError m a
toMonadWithError act = MonadWithError (
   do
      a <- act
      return (hasValue a)
   )

-- --------------------------------------------------------------------------
-- Derived Control Abstractions: Iteration
-- --------------------------------------------------------------------------

foreverUntil :: Monad m => m Bool -> m ()
foreverUntil act =
   do
      stop <- act
      if stop
         then
            done
         else
            foreverUntil act

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
