{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Basic dialog window and a couple of predefined abstractions.
module HTk.Toolkit.DialogWin (
        Dialog,

        dialog,

        createAlertWin,
        createErrorWin,
        createWarningWin,
        createConfirmWin,
        createMessageWin,
        createAlertWin',
        createErrorWin',
        createWarningWin',
        createConfirmWin',
        createMessageWin',
        createDialogWin,
        createDialogWin',

        loadHTkImages,

        questionImg,

        useHTk,
        ) where

import Data.Maybe(fromMaybe)

import System.IO.Unsafe

import Util.Messages
import Util.ExtendedPrelude (newFallOut,mkBreakFn)
import Util.Computation

import Events.Events

import HTk.Kernel.Core
import qualified HTk.Toplevel.HTk as HTk (font)
import HTk.Toplevel.HTk hiding (font)
import HTk.Widgets.Space
import HTk.Toolkit.SelectBox
import HTk.Toolkit.ModalDialog
import HTk.Toolkit.MarkupText
import HTk.Toolkit.Separator

-- --------------------------------------------------------------------------
--  Types
-- --------------------------------------------------------------------------

-- | A @Choice@ represents the name of a button (@String@) and the
-- value returned when this button is pressed.
type Choice a = (String,a)

-- | The @Dialog@ datatype.
data Dialog a = Dialog {
                        fWindow    :: Toplevel,
                        fEditor    :: Maybe Editor,  -- we only have
                        fMsg       :: Maybe Message, -- one of these two
                        fLabel     :: Label,
                        fSelectBox :: SelectBox,
                        fEvents    :: (Event a)
                        }

-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------
-- | Internal.
instance GUIObject (Dialog a) where
        toGUIObject dlg = toGUIObject (fWindow dlg)
        cname dlg = cname (fWindow dlg)

-- | A dialog can have an image
instance HasPhoto (Dialog a) where
        photo p dlg = do {fLabel dlg # photo p; return dlg}

-- | The programm message is displayed as @MarkupText@
instance HasMarkupText (Dialog a) where
  new t dlg =
    case fEditor dlg of
      Just e -> do {e # new t; return dlg}
      _      -> return dlg
  insertAt _ _ _ = error
    "HTk.Toolkit.DialogWin.instance HasMarkupText (Dialog a) insertAt"
  clear = error
    "HTk.Toolkit.DialogWin.instance HasMarkupText (Dialog a) clear"


-- | The message displayed as plain text.
instance GUIValue v=> HasText (Dialog a) v where
  text t dlg =
    case fMsg dlg of
      Just l -> do {l # text t; return dlg}
      _      -> return dlg


-- | Returns configuration option for dialogs that displays text using a
-- scrollbox if it is bigger than the default size (currently (60,6).
-- It also returns a Bool which indicates if we are to use createDialogWin
-- (False) or createDialogWin' (True).

scrollText :: String -> (Config (Dialog a),Bool)
scrollText = scrollText1 (60,6)


-- | Configuration option for dialogs that displays text using a scrollbox
-- if it is bigger than the given size.
--
-- NB.  The argument (39,6) to scrollMarkupText cannot be increased without
-- increasing the size of the message window, which is hardcoded into this
-- module.
scrollText1 :: Size -> String -> (Config (Dialog a),Bool)
scrollText1 size str =
   if biggerThan size str
      then
         (new [scrollMarkupText (39,6) [prose str]],True)
      else
         (text str,False)
   where
      biggerThan (xMax,yMax) str =
         let
            strl = lines str
         in
            length strl > fromIntegral yMax
               || any (\ line -> length line > fromIntegral xMax) strl

-- --------------------------------------------------------------------------
--  Derived Dialog Window
-- --------------------------------------------------------------------------

-- | Constructs an alert window with the given text
createAlertWin :: String
   -- ^ the text to be displayed
   -> [Config Toplevel]
   -> IO ()
createAlertWin str wol =
      do
         catchDestroyedFallOut (createFn choices Nothing confs (defs ++ wol))
         done
   where
       choices = [("Continue",())]
       defs = [text "Alert Window"]

       (scrollConf,complex) = scrollText str
       confs = [scrollConf,photo warningImg]
       createFn = if complex then createDialogWin' else createDialogWin

-- | Constructs an alert window with the given markuptext
createAlertWin' :: [MarkupText]
   -- ^ the markuptext to be displayed
   -> [Config Toplevel]
   -> IO ()
createAlertWin' str wol =
  do
     catchDestroyedFallOut (
        createDialogWin' choices Nothing (confs++[photo warningImg])
           (defs ++ wol)
        )
     done
  where choices = [("Continue",())]
        defs    = [text "Alert Window"]
        confs   = [new str]

-- | Constructs an error window with the given text
createErrorWin :: String
   -- ^ the text to be displayed
   -> [Config Toplevel]
   -> IO ()
createErrorWin str wol =
    do
       catchDestroyedFallOut (createFn choices Nothing confs (defs++wol))
       done
    where
       choices = [("Continue",())]
       defs    = [text "Error Message"]

       (scrollConf,complex) = scrollText str
       confs = [scrollConf,photo errorImg]
       createFn = if complex then createDialogWin' else createDialogWin

-- | Constructs an error window with the given markuptext
createErrorWin' :: [MarkupText]
   -- ^ the markuptext to be displayed
   -> [Config Toplevel]
   -> IO ()
createErrorWin' str wol =
   do
      catchDestroyedFallOut (
         createDialogWin' choices Nothing (confs++[photo errorImg]) (defs++wol)
         )
      done
 where choices = [("Continue",())]
       defs = [text "Error Message"]
       confs = [new str]


-- | Constructs an warning window with the given text
createWarningWin :: String
   -- ^ the text to be displayed
   -> [Config Toplevel]
   -> IO ()
createWarningWin str confs = createAlertWin str (text "Warning Message": confs)

-- | Constructs an warning window with the given markuptext
createWarningWin' :: [MarkupText]
   -- ^ the markuptext to be displayed
   -> [Config Toplevel]
   -> IO ()
createWarningWin' str confs =
  createAlertWin' str (text "Warning Message": confs)

-- | Constructs an confirm window with the given text
createConfirmWin :: String
   -- ^ the text to be displayed
   -> [Config Toplevel]
   -> IO Bool
   -- ^ True(Ok) or False(Cancel)
createConfirmWin str wol =
    do
       bOpt <- catchDestroyedFallOut (
          createFn choices (Just 0) confs (defs ++ wol)
          )
       return (fromMaybe False bOpt)
    where
       choices = [("Ok",True),("Cancel",False)]
       defs    = [text "Confirm Window"]

       (scrollConf,complex) = scrollText str
       confs = [scrollConf,photo questionImg]
       createFn = if complex then createDialogWin' else createDialogWin

-- | Constructs an confirm window with the given markuptext
createConfirmWin' :: [MarkupText]
   -- ^ the markuptext to be displayed
   -> [Config Toplevel]
   -> IO Bool
   -- ^ True(Ok) or False(Cancel)
createConfirmWin' str wol =
   do
      bOpt <- catchDestroyedFallOut (
         createDialogWin' choices (Just 0) (confs++[photo questionImg])
            (defs ++ wol)
         )
      return (fromMaybe False bOpt)
 where choices = [("Ok",True),("Cancel",False)]
       defs    = [text "Confirm Window"]
       confs   = [new str]

-- | Constructs a message (info) window with the given markuptext
createMessageWin' :: [MarkupText]
   -- ^ the markup text to be displayed
   -> [Config Toplevel]
   -> IO ()
   -- ^ ()
createMessageWin' str wol =
   do
      catchDestroyedFallOut (
         createDialogWin' [("Dismiss", ())] Nothing
            [new str, photo infoImg]
            (text "Information": wol)
         )
      done



-- | Constructs a message (info) window with the given string.
createMessageWin :: String
   -- ^ the string to be displayed
   -> [Config Toplevel]
   -> IO ()
   -- ^ ()
createMessageWin str wol =
   do
      catchDestroyedFallOut (
         createFn [("Dismiss", ())] Nothing confs (text "Information": wol)
         )
      done
   where
      (scrollConf,complex) = scrollText str
      confs = [scrollConf,photo infoImg]
      createFn = if complex then createDialogWin' else createDialogWin


-- | Constructs a new dialogue window for plain text
createDialogWin :: [Choice a]
   -- ^ the available buttons in this window
   -> Maybe Int
   -- ^ default button
   -> [Config (Dialog a)]
   -- ^ the list of configuration options for this separator
   -> [Config Toplevel]
   -- ^ the list of configuration options for the window
   -> IO a
   -- ^
createDialogWin choices def confs wol =
   do
      dlg <- dialog True choices def confs wol
      result <- modalInteraction (fWindow dlg) True True (fEvents dlg)
      return result

-- | Constructs a new dialow window for markup text
createDialogWin' :: [Choice a]
   -- ^ the available buttons in this window
   -> Maybe Int
   -- ^ default button
   -> [Config (Dialog a)]
   -- ^ the list of configuration options for this separator
   -> [Config Toplevel]
   -- ^ the list of configuration options for the window
   -> IO a
   -- ^
createDialogWin' choices def confs wol =
   do
      dlg <- dialog False choices def confs wol
      result <- modalInteraction (fWindow dlg) True True (fEvents dlg)
      return result


-- --------------------------------------------------------------------------
--  Base Dialog Window
-- --------------------------------------------------------------------------
-- | Creates a new dialogue with its label, text and buttons.
dialog :: Bool
   -- ^ the available button in this window
   -> [Choice a]
   -- ^ true if we just want a label to display message, false if we want a fancy read-only text editor
   -> Maybe Int
   -- ^ default button
   -> [Config (Dialog a)]
   -- ^ the list of configuration options for this separator
   -> [Config Toplevel]
   -- ^ the list of configuration options for the window
   -> IO (Dialog a)
   -- ^ a dialog
dialog plain choices def confs tpconfs =
   do
      (tp, emsg, lmsg, lbl, sb, ev) <- delayWish $
         do
            tp <- createToplevel tpconfs
            pack tp [Expand On, Fill Both]

            b <- newVBox tp []
            pack b [Expand On, Fill Both]

            b2 <- newHBox b []
            pack b2 [Expand On, Fill Both]

            lbl <- newLabel b2 []
            pack lbl [Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]

            (lmsg, emsg) <-
              if plain then
                 do l <- newMessage b2 [borderwidth 0,
                                        justify JustCenter,
                                        aspect 750,
                                        HTk.font (Helvetica, Roman, 18::Int)]
                    pack l [Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]
                    return (Just l, Nothing)
                 else do msg <- newEditor b2 [size (30,5), borderwidth 0,
                                              state Disabled, wrap WordWrap,
                                              HTk.font (Helvetica, Roman, 18::Int)]
                         pack msg [Expand On, Fill Both, PadX (cm 0.5),
                                                         PadY (cm 0.5)]
                         return (Nothing, Just msg)

            sp1 <- newSpace b (cm 0.15) []
            pack sp1 [Expand Off, Fill X, Side AtBottom]

            newHSeparator b

            sp2 <- newSpace b (cm 0.15) []
            pack sp2 [Expand Off, Fill X, Side AtBottom]

            sb <- newSelectBox b Nothing []
            pack sb [Expand Off, Fill X, Side AtBottom]

            events0 <- mapM (createChoice sb) choices
            let ev0 = choose events0

            -- Arrange for escape when the user destroys the window
            (destroyEvent,unbindAction)  <- bindSimple tp Destroy

            let
               ev =
                     (do
                         result <- ev0
                         always unbindAction
                         return result
                     )
                  +> (do
                         destroyEvent
                         always unbindAction
                         destroyedFallOut
                     )

            return (tp, emsg, lmsg, lbl,sb,ev)
      dlg <- configure (Dialog tp emsg lmsg lbl sb ev) confs
      return dlg
 where createChoice :: SelectBox -> Choice a -> IO (Event a)
       createChoice sb (str,val) =
        do
         but <- addButton sb [text str] [Expand On, Side AtRight]
         clickedbut <- clicked but
         return (clickedbut >> (always (return val)))


destroyedFallOutPair :: (ObjectID,IO a -> IO (Either String a))
destroyedFallOutPair = unsafePerformIO newFallOut
{-# NOINLINE destroyedFallOutPair #-}

destroyedFallOut :: a
destroyedFallOut = mkBreakFn (fst destroyedFallOutPair) "DESTROYED"

catchDestroyedFallOut :: IO a -> IO (Maybe a)
catchDestroyedFallOut act =
   do
      strOrA <- (snd destroyedFallOutPair) act
      return (case strOrA of
         Left "DESTROYED" -> Nothing
         Right a -> Just a
         )

-- --------------------------------------------------------------------------
-- The useHTk function
-- --------------------------------------------------------------------------

htkMessFns :: MessFns
htkMessFns = MessFns {
   alertFn = (\ mess -> createAlertWin mess []),
   errorFn = (\ mess -> createErrorWin mess []),
   warningFn = (\ mess -> createWarningWin mess []),
   confirmFn = (\ mess -> createConfirmWin mess []),
   messageFn = (\ mess -> createMessageWin mess []),
   htkPres = True
   }

useHTk :: IO ()
useHTk = setMessFns htkMessFns

-- Deprecate the alternative
{-# DEPRECATED createAlertWin,createErrorWin,createWarningWin,
   createConfirmWin,createMessageWin
   "Please use the functions in util/Messages instead"
   #-}

-- --------------------------------------------------------------------------
-- Images for the various Dialog Windows
-- --------------------------------------------------------------------------

loadHTkImages :: ()
loadHTkImages = foldr seq () [errorImg,warningImg,questionImg,infoImg]

-- It's important that we create these with unsafePerformIO, this lets
-- Tk reuse the definition-- otherwise we send the whole image data
-- over to the wish again every time somebody opens one of these windows.

errorImg :: Image
errorImg = unsafePerformIO (newImage [imgData GIF "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"])
{-# NOINLINE errorImg #-}

warningImg :: Image
warningImg = unsafePerformIO (newImage [imgData GIF "R0lGODlhMAAwAPU3AAAAABYKAhoUARQUFCcIBiUdAz0MCCsiAzUpBSsrK0YNCE0QDFIOB1wSC0U1BlE/B2kTC3QVDXgYEVJAB2hRCnNZC3thC4gYDYcbEpQbD5weFKQdDqEdEbMfD7IfELkgD7khEatCFrFAELpbEr9vFpJPSMkjEdYlE+opFPUqE8JtFMd+FbmTDr2TEsyHFMObD8ebE9GKE9uqFt+xEuCuEuCyE4CAgP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAFkADcALAAAAAAwADAAAAb+wNvtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73YC32+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrsBb7fb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12A95ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Qa83W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9v+7Xa73W632+12u91ut9vtdrvdbrfbDXi73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7oVKmjO12u91utxvwdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7oVKnzQUAuN1ut9vtdrvdbrfb7Xa73W434O12u91ut9vtdrvdbrfb7Xa7bVKnzSYDAQBut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbsDb7Xa7bVKoz2bDuRAGgNvtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7pVKdzQa0yUCAAMDtdrvdbrfb7Xb+u91ut9vtdrvdbrfb7Xa73W632+12u21Sps1mRNpsMgYA4Ha73W632+12uwFvt9vtdrvdbrfb7Xa73W632+12u21Sp81mM5ttNhsJAAC43W632+12u91ut9vtdrvdbrfb7XYD3m632+12u91QqM5mM5vNYhtOZgEA3G632+12u91ut9vtdrvdbrfb7Xa73W632+12u21Sps1GBZzNZjMOhxMBAAC32+12u91ut9vtdrvdbrfb7Xa73W632+12u21SqM1mQ5uxZDLXRpNZAAC32w14u91ut9vtdrvdbrfb7Xa73W632+12u91Qqc5mE3sBAo6ZbLPRSAAAwO12u93+brfb7Xa73W63G/B2u91ut9vtdrvdNqnTZrOZWQABwIzm2mgwCwDgdrvdbrfb7Xa73W632+12u91ut9vtdrvdUCigZ7OZzQ4AgGBGk2k2GgkAALjdbrfb7Xa73W632+12u91ut9vtdrvdNinTZhOj0Q4AQIEmk7k2QI1mAQDcbrfb7Xa73W632+12u91ut9vtdrvdNqnTZrOh0WgFgMAhk8lkGo1GAgAAbrfb7Xa73YC32+12u91ut9vtdrvdbhFUZ7OJ0Wi0B8DhkMlkMpdGg1kAALfb7Xa73W632+12u91ut9vtdrsBb5vUabPZ0Gi0mQBQcMhoMplMo9FAAAD+wO12u91ut9vtdrvdbrfb7Xa73W4oFGizqdVmslkAEAAWZDKZTObSaCQEAOB2u91ut9vtdrvdbrfb7Xa73W4b1GmzidFqspkMEAAgZDKZTCbTaDQNwAAAvN1ut9vtdrvdbrfb7Xa73W43FArE2cxqsplsBggAJjKZTCaTuTQayQAAuN1ut9vtdrvdbrfbDXi73W63DerE4cRmNBlNNkNMBBWZTCaTyWQaDWYBANxut9vtdrvdbrfb7Xa73W43FOqz2cxmMxlwJpO1KIeWTCaTyWQyl0YjAQAAt9vtdrvdbrfb7Xa73W63Deq02cRmMtlMJpPJZDKZTCaTyWQyoAz+o8EsAIDb7Xa73W632+12u91utxsKBdJsZjIaTSaTyWCHQksmk8lkMpmMpNE0AADA7Xa73W7A2+12u91ut9sGddpsYjSajCaTyWQPAIAik8lkMplM5tJoJAQA4Ha73W632+12u91ut1sJBeJsgDJajSaTyWQyBCBQkclkMplMJpNpNJgFAHC73W632+12u91ut9sGddpwXDVaTSaTyWSyFsIikwFlMplMJpPJSBoNBDAA3G632+12u91ut9stggJtOBsOh8PhjFQul0wmk8lkMplMJpPBSCWNhAAAJm632+12u91ut9vtpjBtOBwOh8PhcDQcDkej0Wg0Go1Go9H+aDQajQazAAAAt9vtdrvdbrfb7QYMSBqRSOSCyWg0Go1Go+FoNBqNRqPRaDQajUajaQAAgNvtdrvdbrfb7XYDAACAAIFgWDQaDYgEKJFgMBgMBoPBSCQYDAYjkSwAA8Dtdrvdbrfb7Xa7AQAAAAAAAAAAAECAQCAQFAaDQmEwJAwGA9BgMBAGAADgdrvdbrfb7Xa73QAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAGAAAAwAAADgdrvdbjfg7Xa73W43AAAAGAQAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAcLvdbrfb7Xa73W632+12AwAAQAAAAAAAAIPAAAAAAAD+AAAAAAAAAAAAAAC43W632+12u91ut9vtdrvdbrfb7Xa7AQAAAAAAAACAAAAAAAAAAAAAAHC73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7AW+32+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdgPebrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+0GvN1ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9sT7Xa73W632w14u91ut9vtdrsFAQA7"])
{-# NOINLINE warningImg #-}

questionImg :: Image
questionImg = unsafePerformIO (newImage [imgData GIF "R0lGODlhMAAwAPUxAAAAABkVDRYWFiIcEjk5OSs9YTVFZGVPJG1VJ3FYKHNfO3xiLX9pQWNjY35+f4FlLopsMZZ2N6J/O5J8UqeDPKaHSKSMWbONQL2URL6bU7+iZ76icsCXRcOfVsmkW9eqTtisWtixX8WmaMmsc9e0a9m6effDWvfJbffPfqqqqti+htzElPfUjPfYmfferffiuPflwP///////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAFkADIALAAAAAAwADAAAAb+QJlMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyYAymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpMBZTKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmA8pkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTAaUyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMpn+TCaTyWQymUwmk8lkMplMJpPJZDKZDCiTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMhlQJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZLIAAAAAAAAAAEAmk8lkMplMJpPJZDKZTCaTyWQyoEwmk8lkMplMJpPJZDIAAEDBfCKUBQAAAMhkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZEAA4NNqvVqlUilDAQAAMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZACA6QVrsUwmE6pEoiCAAIBMJpPJZDKZTCb+k8lkMplMJpPJZDKZTCaTyWQBgMnlMlUiEYrJhCKFKAAAQCaTyWQymUwmkwFlMplMJpPJZDKZTCaTyWQygKb1MkUAAAAgYjKdQqEHACCQyWQymUwmk8lkMplMJpPJZDKZTCYDymSyAOD0QkUAAEABAOiYUJ8QBQAAyGQymUwmk8lkMplMJpPJZDKZTCaTyWQygIX1+gAAAIPDAQRYTKfQhwIAAGIymUwmk8lkMplMJpPJZDKZTCaTyWQyGeDDMlEAAIMjFgNYTifS5wEAAFIxmQwok8lkMplMJpPJZDKZTCaTyWQymQxgYpkoAIAjFpMBLCYS6XMAAACpmEwmk8n+ZDKZTCaTyWQyGVAmk8lkMplMBiCRMA8AwBGTAQCiU+lTAQAAjlRMJpPJZDKZTCaTyWQymUwmk8lkMplMBgAAAAAgAOCIyQAQkir0SQAAAEcqJpPJZDKZTCaTyWQymUwmk8lkMplMJgMAAAAAQOCIAQAYlurDAQAAQIAjJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJABOHxeFwAACXk+rzAQAAAEcqJpPJZDKZTCaTyYAymUwmk8lkMplMJpPJZDKAw+FwxAAAE4sEigAAAEEqJpPJZDKZTCaTyWQymUwmk8lkMplMJpMBZTKZTCaTyQCIzqo0EgUAAIEjFZPJZDL+mUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8kADxLJIwJOAACAIxWTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkgEiJBNoAAABHKiaTyWQymUwGlMlkMplMJpPJZDKZTCaTyWQymUwmk8lkMgCnRAJNAACAIyaTyWQymUwmk8lkMplMJpPJZDKZDCiTyWQymUwmk8lkMhngoyJ9FACAIxWTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMhkQkCl9MAAAIBWTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMpkMUKlQFgAAIBWTyWQyoEz+JpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTAYQEAAAACCVislkMplMJpPJZDKZTCaTyWRAmUwmk8lkMplMJpPJZDKZTCYDAAAAAADgiMlkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJgDKZTCaTyQAWR8PhSMVkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyQYAAAAAiAFlMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyQAlFAsDAABkMplMJpPJZDKZTCYDymQymUwmk8lkMplMJpPJZDKZTCaTyWQAVKsFAgAAMplMJpP+yWQymUwmk8lkMplMJpPJZDKZTAaUyWQymUwmk8lkMgCLFcIAAIBUTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMgALSPpgAABAKiaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMhngQ+IkAABAKiaTyWQyGVAmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMBgAAAAAAIBWTyWQymUwmk8lkMplMJpPJZDKgTCaTyWQymUwmk8lkMplMJgMAAAAAwJGKyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkQJlMJpPJAAAAIOVIxWT+MplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQAh8ORismAMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTAWUymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJgPKZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwGlMlkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMpkTTCaTyWQymQwok8lkMplMJpMFAQA7"])
{-# NOINLINE questionImg #-}

infoImg :: Image
infoImg = unsafePerformIO (newImage [imgData GIF "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"])
{-# NOINLINE infoImg #-}
