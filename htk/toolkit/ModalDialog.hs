{- #########################################################################

MODULE        : ModalDialog
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines a simple behavioural abstraction for modal dialogs 


   ######################################################################### -}


module ModalDialog (
        modalDialog,
        modalInteraction

        ) where

import Computation (try)
import Interaction()
import HTk
import Mouse
import Debug(debug)
import qualified Selective


-- --------------------------------------------------------------------------
-- Basic Behaviours for modelling Modals Dialogs
-- --------------------------------------------------------------------------           
modalDialog :: Window -> IO a -> IO a
modalDialog win beh = do {
        modality <- getModal win;
        maybeModalDialog True modality win beh
        }


maybeModalDialog :: Bool -> Bool -> Window -> IO a -> IO a
maybeModalDialog destr True win beh = doModalDialog destr win beh
maybeModalDialog destr False win beh = 
   do
      debug "mMD1"
      ans <- try beh
      debug "mMD2"
      when destr (destroy win)
      debug "mMD3"
      propagate ans                                                            

doModalDialog :: Bool -> Window -> IO a -> IO a
doModalDialog destr win beh = 
   do
      gw <- getCurrentGrab
      tp <- getToplevel win
      grabLocal tp
      ans <- try beh
      try(releaseGrab tp)
      when destr (do {try (destroy win);done})
      returnGrab gw
      propagate ans


-- --------------------------------------------------------------------------
--  Modals Interaction
-- --------------------------------------------------------------------------           
modalInteraction :: Window -> Bool -> IA a -> IO a
modalInteraction win destr ev = 
   do
      modality <- getModal win
      result <- maybeModalDialog destr modality win (sync ev)
      return result

