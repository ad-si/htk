{- This module contains basic code for constructing Emacs commands -}

module EmacsCommands(
   HasEmacsCommand(..),
   execEmacs,
   evalEmacs,
   evalEmacsQuick,
   Prin(..),
   Literal(..),

   Multi,
   multi,
   ) where

import ExtendedPrelude
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

evalEmacsQuick :: HasEmacsCommand emacsCommand 
   => EmacsSession -> emacsCommand -> IO String
evalEmacsQuick emacsSession emacsCommand 
   = evalEmacsStringQuick emacsSession (toEmacsString emacsCommand)

-- -------------------------------------------------------------------------
-- Instances of Emacs command
-- -------------------------------------------------------------------------

-- | A call to a function with no arguments.
instance HasEmacsCommand String where
   toEmacsString funName = "("++funName++")"

-- | A call to a function with one String argument.
instance HasEmacsCommand (String,String) where
   toEmacsString (funName,arg) = toEmacsString (funName,[arg])  

-- | A call to a function with one Int argument.
instance HasEmacsCommand (String,Int) where
   toEmacsString (funName,arg) = "(" ++ funName ++ " " ++ show arg ++ ")"

-- | A call to a function with a number of arguments to be passed as 
-- Emacs Strings.
instance HasEmacsCommand (String,[String]) where
   toEmacsString (funName,args) =
         "("++funName++concatArgs++")"
      where
         concatArgs = concatMap
            (\ arg -> " \""++emacsEscape arg++"\"")
            args

-- This wraps a command where we want the output printed.
-- For these it is safe to use evalEmacsQuick
newtype Prin x = Prin x

instance HasEmacsCommand x => HasEmacsCommand (Prin x) where
   toEmacsString (Prin x) = 
      "(uni-prin "++toEmacsString x++")"


-- This is where we really know what the command should be
newtype Literal = Literal String

instance HasEmacsCommand Literal where
   toEmacsString (Literal s) = s

-- -------------------------------------------------------------------------
-- How to group several commands together
-- -------------------------------------------------------------------------

newtype Multi = Multi String

multi :: HasEmacsCommand emacsCommand => emacsCommand -> Multi
multi emacsCommand = Multi (toEmacsString emacsCommand)

instance HasEmacsCommand [Multi] where
   toEmacsString commands = 
      "(" ++ unsplitByChar ' ' ("progn":(map (\ (Multi s) -> s) commands)) 
         ++ ")"
