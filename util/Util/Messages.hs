-- |
-- Description: Outputting Messages
--
-- This module contains the hooks for displaying messages to the user
-- (errors, alerts, warnings and the like) and getting yes\/no responses.
--
-- The idea is that these are by default textual, and go via
-- 'stdin', 'stdout' and 'stderr' .  However if the DialogWin function
-- 'useHTk' is invoked, windows will pop up.
module Util.Messages(
   -- Functions for displaying messages
   alertMess, -- :: String -> IO ()
   errorMess, -- :: String -> IO ()
   warningMess, -- :: String -> IO ()
   confirmMess, -- :: String -> IO Bool
   messageMess, -- :: String -> IO ()

   -- Miscellaneous
   htkPresent,
      -- :: IO Bool
      -- If True, indicates that the flag corresponding to a graphical mode
      -- has been set.  This is used occasionally for deciding whether to
      -- ask the user something on stdout, stdin or via a window.

   textQuery,
      -- :: String -> IO String
      -- queries the user on stdout getting the answer from stdin.
      -- Leading and trailing spaces are trimmed from the result.

   errorMess2,
      -- :: String -> IO ()
      -- Attempt to reduce the number of error messages displayed by the
      -- imports stuff.

   -- Interface used by HTk for setting a graphical mode

   MessFns(..),  -- versions of the above functions
   setMessFns, -- :: MessFns -> IO ()
   ) where

import System.IO
import Data.Char
import qualified Data.List as List

import qualified Data.Set as Set
import Control.Concurrent.MVar
import System.IO.Unsafe

import Util.Computation(done)
import Util.ExtendedPrelude

-- ------------------------------------------------------------------------
-- Displaying Messages & Miscellaneous
-- ------------------------------------------------------------------------

-- | Display an alert
alertMess :: String -> IO ()
alertMess = getMessFn alertFn

-- | Display an error
errorMess :: String -> IO ()
errorMess = getMessFn errorFn

-- | Display a warning message
warningMess :: String -> IO ()
warningMess = getMessFn warningFn

-- | Confirm something with the user.
confirmMess :: String -> IO Bool
confirmMess = getMessFn confirmFn

-- | Display some informational message.
messageMess :: String -> IO ()
messageMess = getMessFn messageFn

-- | If True, indicates that the flag corresponding to a graphical mode
-- has been set.  This is used occasionally for deciding whether to
-- ask the user something on stdout, stdin or via a window.
htkPresent :: IO Bool
htkPresent = getMessValue htkPres

-- | queries the user on stdout getting the answer from stdin.
-- Leading and trailing spaces are trimmed from the result.
textQuery :: String -> IO String
textQuery query =
   do
      putStrLn query
      reply <- getLine
      return (trimSpaces reply)

-- ------------------------------------------------------------------------
-- MessFns
-- ------------------------------------------------------------------------

data MessFns = MessFns {
   alertFn :: String -> IO (),
   errorFn :: String -> IO (),
   warningFn :: String -> IO (),
   confirmFn :: String -> IO Bool,
   messageFn :: String -> IO (),
   htkPres :: Bool
   }

messFnsMVar :: MVar MessFns
messFnsMVar = unsafePerformIO (newMVar defaultMessFns)
{-# NOINLINE messFnsMVar #-}

setMessFns :: MessFns -> IO ()
setMessFns messFns =
   do
      takeMVar messFnsMVar
      putMVar messFnsMVar messFns

getMessFn :: (MessFns -> (String -> IO a)) -> (String -> IO a)
getMessFn toFn str =
   do
      messFns <- getMessValue id
      (toFn messFns) str

getMessValue :: (MessFns -> a) -> IO a
getMessValue toA =
   do
      messFns <- readMVar messFnsMVar
      return (toA messFns)

-- ------------------------------------------------------------------------
-- The default messFns
-- ------------------------------------------------------------------------

defaultMessFns :: MessFns
defaultMessFns = MessFns {
   alertFn = defaultAlert,
   errorFn = defaultError,
   warningFn = defaultWarning,
   confirmFn = defaultConfirm,
   messageFn = defaultMessage,
   htkPres = False
   }

defaultAlert :: String -> IO ()
defaultAlert str = putStrLn ("Alert: " ++ str)

defaultError :: String -> IO ()
defaultError str = hPutStrLn stderr ("Error: " ++ str)

defaultWarning :: String -> IO ()
defaultWarning str = putStrLn ("Warning: " ++ str)

defaultConfirm :: String -> IO Bool
defaultConfirm str =
   do
      putStrLn str
      putStrLn ("O[K] or C[ancel]?")
      let
         getOC :: IO Bool
         getOC =
            do
               oc <- readOC
               case oc of
                  Just c -> return c
                  Nothing ->
                     do
                        putStrLn ("Type O (or some prefix of OK) or C "
                           ++ "(or some prefix of CANCEL)")
                        getOC

         readOC :: IO (Maybe Bool)
         readOC =
            do
               result0 <- getLine
               let
                  result1 = fmap toUpper (trimSpaces result0)

               case (result1,isPrefix result1 "OK",isPrefix result1 "CANCEL")
                     of
                  ("",_,_) -> return Nothing
                  (_,Just _,_) -> return (Just True)
                  (_,_,Just _) -> return (Just False)
                  (_,Nothing,Nothing) -> return Nothing
      getOC

defaultMessage :: String -> IO ()
defaultMessage = putStrLn

-- ------------------------------------------------------------------------
-- Reducing the number of error messages.
-- ------------------------------------------------------------------------

pendingErrorMessagesMVar :: MVar [String]
pendingErrorMessagesMVar = unsafePerformIO (newMVar [])
{-# NOINLINE pendingErrorMessagesMVar #-}

-- | Display a series of one-line messages, separated by newline characters,
-- attempting to combine them together and eliminate duplicates as much as
-- possible.  If other identical messages come in while the error message
-- is being delayed, we throw them away.
errorMess2 :: String -> IO ()
errorMess2 message0 =
   do
      let
         messages1 = reverse (lines message0)

      modifyMVar_ pendingErrorMessagesMVar
         (\ messages -> return (messages1 ++ messages))
      clearPendingErrorMessages

clearPendingErrorMessages :: IO ()
clearPendingErrorMessages = cpe Set.empty
   where
      cpe :: Set.Set String -> IO ()
      cpe alreadyDisplayedSet0 =
         do
            messages0 <- readMVar pendingErrorMessagesMVar
            putStrLn (show (messages0,Set.toList alreadyDisplayedSet0))
            let
               messages1 = List.filter
                  (\ message -> not (Set.member message alreadyDisplayedSet0))
                  messages0

               messages2 = uniqOrdOrder messages1

            case messages2 of
               [] -> done
               _ ->
                  do
                     errorMess (unlines (reverse messages2))

                     let
                        alreadyDisplayedSet1 =
                           Set.union alreadyDisplayedSet0
                                  (Set.fromList messages2)

                     cpe alreadyDisplayedSet1

