{- This module contains basic code for constructing Emacs commands -}

module EmacsCommands(
   HasEmacsCommand(..),
   execEmacs,
   evalEmacs,
   Prin(..),
   ) where

import CommandStringSub(emacsEscape)

import EmacsBasic

-- -------------------------------------------------------------------------
-- The class of what corresponds to an Emacs command.
-- -------------------------------------------------------------------------

-- Classes of commands
class HasEmacsCommand emacsCommand where
   toEmacsString :: emacsCommand -> String

execEmacs :: HasEmacsCommand emacsCommand 
   => EmacsSession -> emacsCommand -> IO ()
execEmacs emacsSession emacsCommand 
   = execEmacsString emacsSession (toEmacsString emacsCommand)

evalEmacs :: HasEmacsCommand emacsCommand 
   => EmacsSession -> emacsCommand -> IO String
evalEmacs emacsSession emacsCommand 
   = evalEmacsString emacsSession (toEmacsString emacsCommand)

-- -------------------------------------------------------------------------
-- Instances of Emacs command
-- -------------------------------------------------------------------------

---
-- A call to a function with a number of arguments to be passed as 
-- Emacs Strings.
instance HasEmacsCommand (String,[String]) where
   toEmacsString (funName,args) =
         "("++funName++concatArgs++")"
      where
         concatArgs = concatMap
            (\ arg -> " \""++emacsEscape arg++"\"")
            args

-- This wraps a command where we want the output printed.
data Prin x = Prin x

instance HasEmacsCommand x => HasEmacsCommand (Prin x) where
   toEmacsString (Prin x) = 
      "(uni-prin "++toEmacsString x++")"



  