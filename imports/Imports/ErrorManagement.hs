-- |
-- Description: Manage errors for "Imports".
--
-- The code in this module manages errors.  The main task is to reduce the
-- huge flood of errors that occur during large updates to those that
-- actually occur.
--
-- The strategy is that
--    (1) we bracket large updates with a delayer, and postpone updates until
--        the delayer is finished.
--    (2) since a lot of the imports stuff using "Sink"s 'parallelExec'
--        mechanism, we acquire a global lock on the parallelExecVSem,
--        before reporting messages, to assure that all such parallel
--        actions are completed before we look for messages (or complete).
--    (3) while we are doing updates, we don't actually display any messages.
--        Instead we record where errors have occurred.  Then when we
--        come to report messages, we look at those places and look to
--        see if there is still a problem.  Only then do we report it.
--
-- ErrorManagement itself uses an MSem.  This prevents conflicting
-- updates in different threads, but more importantly allows us to
-- use bracketForImportErrors1 generously without worrying about
-- it being at some point used multiple times.
module Imports.ErrorManagement(
   ErrorLocation(..),
   ErrorManagementState,

   newErrorManagementState,
      -- :: Ord node => Delayer -> (ErrorLocation node -> IO (Maybe String))
      -- -> IO (ErrorManagementState node)
   recordError,
      -- :: ErrorManagementState node -> ErrorLocation node -> IO ()
   bracketForImportErrors1,
      -- :: Ord node => ErrorManagementState node -> IO a -> IO a

   ) where

import Control.Monad
import Data.Maybe

import Util.Computation
import Util.Messages
import Util.Registry
import Util.ExtendedPrelude
import Util.Delayer
import Util.Sink
import Util.VSem
import Reactor.MSem

import Imports.EntityNames

-- -----------------------------------------------------------------------
-- Datatypes
-- -----------------------------------------------------------------------

data ErrorManagementState node = ErrorManagementState {
   currentErrors :: Registry (ErrorLocation node) (),
   confirmError :: ErrorLocation node -> IO (Maybe String),
   delayer :: Delayer,
   managementLock :: MSem
   }

data ErrorLocation node =
      GlobalError node
   |  LocalError node
   |  SearchError node EntitySearchName
   deriving (Eq,Ord)
-- the significance of these is that given a confirmed error for a GlobalNode,
-- we needn't bother looking for a LocalNode error (since that will have
-- the same error); given either a GlobalNode or LocalNode error, we needn't
-- bother looking for Search error.
--
-- NB.  We rely on the fact that GlobalError < LocalError < SearchError in
-- implementing this.

-- -----------------------------------------------------------------------
-- Creating a new ErrorManagementState
-- -----------------------------------------------------------------------

newErrorManagementState
   :: Ord node => Delayer -> (ErrorLocation node -> IO (Maybe String))
   -> IO (ErrorManagementState node)
newErrorManagementState delayer1 confirmError1 =
   do
      currentErrors1 <- newRegistry
      managementLock <- newMSem
      let
         errorManagementState = ErrorManagementState {
            currentErrors = currentErrors1,
            confirmError = confirmError1,
            delayer = delayer1,
            managementLock = managementLock
            }
      return errorManagementState

-- -----------------------------------------------------------------------
-- Recording that an error has occurred
-- -----------------------------------------------------------------------

recordError
   :: Ord node => ErrorManagementState node -> ErrorLocation node -> IO ()
recordError errorManagementState errorLocation =
   setValue (currentErrors errorManagementState) errorLocation ()

-- -----------------------------------------------------------------------
-- Doing the bracketting
-- -----------------------------------------------------------------------

bracketForImportErrors1
   :: Ord node => ErrorManagementState node -> IO a -> IO a
bracketForImportErrors1 errorManagementState act =
   synchronizeWithChoice (managementLock errorManagementState)
      (\ isAlreadyBracketted ->
         if isAlreadyBracketted
            then
               act -- don't bother bracketting again
            else
               bracketForImportErrors1inner errorManagementState act
         )


bracketForImportErrors1inner
   :: Ord node => ErrorManagementState node -> IO a -> IO a
bracketForImportErrors1inner
      (errorManagementState :: ErrorManagementState node) act =
   do
      let currentErrors1 = currentErrors errorManagementState

      (currentErrors2 :: [ErrorLocation node]) <- listKeys currentErrors1
      case currentErrors2 of
         [] -> done
         _ -> putStrLn ("ErrorManagement: " ++ show (length currentErrors2)
                 ++ " unreported errors occurred")
      emptyRegistry currentErrors1

      result <- delay (delayer errorManagementState) (try act)

      synchronizeGlobal parallelExecVSem (reportErrors errorManagementState)
      propagate result

-- -----------------------------------------------------------------------
-- Reporting the errors
-- -----------------------------------------------------------------------

reportErrors :: Ord node => ErrorManagementState node -> IO ()
reportErrors (errorManagementState :: ErrorManagementState node) =
   do
      let
         currentErrors1 = currentErrors errorManagementState

      (errorLocs :: [ErrorLocation node]) <- listKeys currentErrors1
      let
         senior (GlobalError _) = []
         senior (LocalError node) = [GlobalError node]
         senior (SearchError node _) = [GlobalError node,LocalError node]

      (errorMessages1 :: [Maybe String]) <- mapM
         (\ errorLoc ->
            do
               -- check if any has a confirmed error message which would
               -- make this one redundant.
               let
                  seniorLocs = senior errorLoc
               (seniorStatus :: [Maybe ()])
                  <- mapM (getValueOpt currentErrors1) seniorLocs
               let
                  seniorError = any isJust seniorStatus
               if seniorError
                  then
                     return Nothing
                  else
                     do
                        confirmedErrorOpt
                           <- confirmError errorManagementState errorLoc
                        case confirmedErrorOpt of
                           Nothing -> deleteFromRegistry currentErrors1
                              errorLoc
                           _ -> done
                        return confirmedErrorOpt
               )
         errorLocs
      let
         errorMessages2 :: [String]
         errorMessages2 = catMaybes errorMessages1

         errorMessages3 :: [[String]]
         errorMessages3 = map lines errorMessages2
            -- split each message into its component lines
            -- (we are basically assuming these are independent messages)

         errorMessages4 :: [String]
         errorMessages4 = concat errorMessages3

         errorMessages5 :: [String]
         errorMessages5 = uniqOrdOrder errorMessages4
            -- remove duplicate lines.

      emptyRegistry currentErrors1

      case errorMessages5 of
         [] -> done
         _ -> errorMess ("Error(s) detected processing imports\n"
                 ++ unlines errorMessages5)



