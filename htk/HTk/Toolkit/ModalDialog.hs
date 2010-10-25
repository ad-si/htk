module HTk.Toolkit.ModalDialog (

  modalDialog,
  modalInteraction

) where

import Control.Exception

import HTk.Toplevel.HTk

-- -----------------------------------------------------------------------
-- Basic Behaviours for modelling Modal Dialogs
-- -----------------------------------------------------------------------

{-
This function should acctually find out if the window allows modality or not,
but since this not possible at the moment, the function gets an extra parameter
to decide. There the supplied window is always destroyed after the event occoured.
 -}
modalDialog :: Toplevel -> Bool -> Event a -> IO a
modalDialog win modality ev =
 do
  --modality <- getModal win --old way
  maybeModalDialog True modality win ev

{-

 -}
maybeModalDialog :: Bool -> Bool -> Toplevel -> Event a -> IO a
maybeModalDialog destr True win ev = doModalDialog destr win ev
maybeModalDialog destr False win ev =
 do
  ans <- sync ev
  when destr (destroy win)
  return ans

doModalDialog :: Bool -> Toplevel -> Event a -> IO a
doModalDialog destr win ev =
 do
  gw <- getCurrentGrab
  grabLocal win
  ans <- sync ev
  try(releaseGrab win) :: IO (Either SomeException ())
  when destr $ do
    try (destroy win) :: IO (Either SomeException ())
    return ()
  returnGrab gw
  return ans


-- --------------------------------------------------------------------------
--  Modals Interaction
-- --------------------------------------------------------------------------
{-
Same as with modalDialog here, only that the caller decides if the window is
destroyed after the event occoured or not.
 -}
modalInteraction :: Toplevel -> Bool -> Bool -> Event a -> IO a
modalInteraction win destr modality ev =
 do
  --modality <- getModal win
  maybeModalDialog destr modality win ev











