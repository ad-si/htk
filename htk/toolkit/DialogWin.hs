-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

---
-- Basic dialog window and a couple of predefined abstractions!
module DialogWin (
        Dialog,

        dialog,

        createAlertWin,
        createErrorWin,
        createWarningWin,
        createConfirmWin,
        createAlertWin',
        createErrorWin',
        createWarningWin',
        createConfirmWin',
        createDialogWin,
        ) where

import Core
import HTk
import Space
import SelectBox
import ModalDialog
import MarkupText
import Separator

-- --------------------------------------------------------------------------
--  Types 
-- --------------------------------------------------------------------------

---
-- A <code>Choice</code> represents the name of a button (<code>String</code>) and the
-- value returned when this button is pressed.
type Choice a = (String,a)

---
-- The <code>Dialog</code> datatype.
data Dialog a = Dialog {
                        fWindow    :: Toplevel,
                        fMessage   :: Editor,
                        fLabel     :: Label,
                        fSelectBox :: SelectBox,
                        fEvents    :: (Event a)
                        } deriving Eq

-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------            
---
-- Internal.
instance GUIObject (Dialog a) where
---
-- Internal.
        toGUIObject dlg = toGUIObject (fWindow dlg)
---
-- Internal.
        cname dlg = cname (fWindow dlg)

---
-- A dialog can have an image
instance HasPhoto (Dialog a) where
        photo p dlg = do {
                configure (fLabel dlg) [photo p];
                return dlg
                }

---
-- The programm message is displayed as <code>MarkupText</code>
instance HasMarkupText (Dialog a) where
         new t dlg = do {
                 configure (fMessage dlg) [new t];
                 return dlg
                 }

-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------
---
-- Constructs an alert window with the given text
-- @param str     - the text to be displayed
createAlertWin :: String -> [Config Toplevel] -> IO ()
createAlertWin str wol = createAlertWin' (strToMarkup str) wol

---
-- Constructs an alert window with the given markuptext
-- @param str     - the markuptext to be displayed
createAlertWin' :: [MarkupText] -> [Config Toplevel] -> IO ()
createAlertWin' str wol = 
 do
  warningImg' <- warningImg
  createDialogWin choices Nothing (confs++[photo warningImg']) (defs ++ wol)
 where choices = [("Continue",())]
       defs = [text "Alert Window"]
       confs = [new str]

---
-- Constructs an error window with the given text
-- @param str     - the text to be displayed
createErrorWin :: String -> [Config Toplevel] -> IO ()
createErrorWin str wol = createErrorWin' (strToMarkup str) wol

---
-- Constructs an error window with the given markuptext
-- @param str     - the markuptext to be displayed
createErrorWin' :: [MarkupText] -> [Config Toplevel] -> IO ()
createErrorWin' str wol = 
 do
  errorImg' <- errorImg
  createDialogWin choices Nothing (confs++[photo errorImg']) (defs++wol)
 where choices = [("Continue",())]
       defs = [text "Error Message"]
       confs = [new str]
       

---
-- Constructs an warning window with the given text
-- @param str     - the text to be displayed
createWarningWin :: String -> [Config Toplevel] -> IO ()
createWarningWin str confs = createAlertWin str ([text "Warning Message"] ++ confs)

---
-- Constructs an warning window with the given markuptext
-- @param str     - the markuptext to be displayed
createWarningWin' :: [MarkupText] -> [Config Toplevel] -> IO ()
createWarningWin' str confs = createAlertWin' str ([text "Warning Message"] ++ confs)

---
-- Constructs an confirm window with the given text
-- @param str     - the text to be displayed
-- @return result - True(Ok) or False(Cancel)
createConfirmWin :: String -> [Config Toplevel] -> IO Bool
createConfirmWin str wol = createConfirmWin' (strToMarkup str) wol

---
-- Constructs an confirm window with the given markuptext
-- @param str     - the markuptext to be displayed
-- @return result - True(Ok) or False(Cancel)
createConfirmWin' :: [MarkupText] -> [Config Toplevel] -> IO Bool
createConfirmWin' str wol = 
 do
  questionImg' <- questionImg 
  createDialogWin choices (Just 0) (confs++[photo questionImg']) (defs ++ wol)
 where choices = [("Ok",True),("Cancel",False)]
       defs = [text "Confirm Window"]
       confs = [new str]

---
-- Constructs a new dialow window
-- @param choices     - the available button in this window
-- @param def         - default button
-- @param confs       - the list of configuration options for this separator
-- @param wol         - the list of configuration options for the window
-- @return result     - 
createDialogWin :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Toplevel] -> IO a
createDialogWin choices def confs wol = 
   do 
      dlg <- dialog choices def confs wol
      result <- modalInteraction (fWindow dlg) True True (fEvents dlg)
      return result

-- --------------------------------------------------------------------------
--  Base Dialog Window 
-- --------------------------------------------------------------------------
---
-- Creates a new dialog with its label, text and buttons.
-- @param choices     - the available button in this window
-- @param def         - default button
-- @param confs       - the list of configuration options for this separator
-- @param tpconfs     - the list of configuration options for the window
-- @return result     - a dialog
dialog :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Toplevel] -> IO (Dialog a)
dialog choices def confs tpconfs =
   do
      (tp,msg,lbl,sb,ev) <- delayWish (
         do
            tp <- createToplevel tpconfs
            pack tp [Expand On, Fill Both]
          
            b <- newVBox tp []
            pack b [Expand On, Fill Both]
          
            b2 <- newHBox b []
            pack b2 [Expand On, Fill Both]
          
            lbl <- newLabel b2 []
            pack lbl [Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]
          
            msg <- newEditor b2 [size (30,5), borderwidth 0, state Disabled, wrap WordWrap, HTk.font fmsg]
            pack msg[Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]
          
            sp1 <- newSpace b (cm 0.15) []
            pack sp1 [Expand Off, Fill X, Side AtBottom]
          
            newHSeparator b
          
            sp2 <- newSpace b (cm 0.15) []
            pack sp2 [Expand Off, Fill X, Side AtBottom]
          
            sb <- newSelectBox b Nothing []
            pack sb [Expand Off, Fill X, Side AtBottom]
          
            events <- mapM (createChoice sb) choices
            let ev = choose events
            return (tp,msg,lbl,sb,ev)
         )
      dlg <- configure (Dialog tp msg lbl sb ev) confs
      return dlg
 where fmsg = xfont { family = Just Courier, weight = Just Bold, points = (Just 18) }
       createChoice :: SelectBox -> Choice a -> IO (Event a)
       createChoice sb (str,val) = 
        do
         but <- addButton sb [text str] [Expand On, Side AtRight]
         clickedbut <- clicked but
         return (clickedbut >> (always (return val)))

-- --------------------------------------------------------------------------
-- String to MarkupText
-- --------------------------------------------------------------------------
strToMarkup :: String -> [MarkupText]
strToMarkup str = [bold [prose str]]

-- --------------------------------------------------------------------------
-- Images for the various Dialog Windows
-- --------------------------------------------------------------------------
errorImg = newImage NONE [imgData GIF "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"]

warningImg = newImage NONE [imgData GIF "R0lGODlhMAAwAPU3AAAAABYKAhoUARQUFCcIBiUdAz0MCCsiAzUpBSsrK0YNCE0QDFIOB1wSC0U1BlE/B2kTC3QVDXgYEVJAB2hRCnNZC3thC4gYDYcbEpQbD5weFKQdDqEdEbMfD7IfELkgD7khEatCFrFAELpbEr9vFpJPSMkjEdYlE+opFPUqE8JtFMd+FbmTDr2TEsyHFMObD8ebE9GKE9uqFt+xEuCuEuCyE4CAgP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAFkADcALAAAAAAwADAAAAb+wNvtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73YC32+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrsBb7fb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12A95ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Qa83W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9v+7Xa73W632+12u91ut9vtdrvdbrfbDXi73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7oVKmjO12u91utxvwdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7oVKnzQUAuN1ut9vtdrvdbrfb7Xa73W434O12u91ut9vtdrvdbrfb7Xa7bVKnzSYDAQBut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbsDb7Xa7bVKoz2bDuRAGgNvtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7pVKdzQa0yUCAAMDtdrvdbrfb7Xb+u91ut9vtdrvdbrfb7Xa73W632+12u21Sps1mRNpsMgYA4Ha73W632+12uwFvt9vtdrvdbrfb7Xa73W632+12u21Sp81mM5ttNhsJAAC43W632+12u91ut9vtdrvdbrfb7XYD3m632+12u91QqM5mM5vNYhtOZgEA3G632+12u91ut9vtdrvdbrfb7Xa73W632+12u21Sps1GBZzNZjMOhxMBAAC32+12u91ut9vtdrvdbrfb7Xa73W632+12u21SqM1mQ5uxZDLXRpNZAAC32w14u91ut9vtdrvdbrfb7Xa73W632+12u91Qqc5mE3sBAo6ZbLPRSAAAwO12u93+brfb7Xa73W63G/B2u91ut9vtdrvdNqnTZrOZWQABwIzm2mgwCwDgdrvdbrfb7Xa73W632+12u91ut9vtdrvdUCigZ7OZzQ4AgGBGk2k2GgkAALjdbrfb7Xa73W632+12u91ut9vtdrvdNinTZhOj0Q4AQIEmk7k2QI1mAQDcbrfb7Xa73W632+12u91ut9vtdrvdNqnTZrOh0WgFgMAhk8lkGo1GAgAAbrfb7Xa73YC32+12u91ut9vtdrvdbhFUZ7OJ0Wi0B8DhkMlkMpdGg1kAALfb7Xa73W632+12u91ut9vtdrsBb5vUabPZ0Gi0mQBQcMhoMplMo9FAAAD+wO12u91ut9vtdrvdbrfb7Xa73W4oFGizqdVmslkAEAAWZDKZTObSaCQEAOB2u91ut9vtdrvdbrfb7Xa73W4b1GmzidFqspkMEAAgZDKZTCbTaDQNwAAAvN1ut9vtdrvdbrfb7Xa73W43FArE2cxqsplsBggAJjKZTCaTuTQayQAAuN1ut9vtdrvdbrfbDXi73W63DerE4cRmNBlNNkNMBBWZTCaTyWQaDWYBANxut9vtdrvdbrfb7Xa73W43FOqz2cxmMxlwJpO1KIeWTCaTyWQyl0YjAQAAt9vtdrvdbrfb7Xa73W63Deq02cRmMtlMJpPJZDKZTCaTyWQyoAz+o8EsAIDb7Xa73W632+12u91utxsKBdJsZjIaTSaTyWCHQksmk8lkMpmMpNE0AADA7Xa73W7A2+12u91ut9sGddpsYjSajCaTyWQPAIAik8lkMplM5tJoJAQA4Ha73W632+12u91ut1sJBeJsgDJajSaTyWQyBCBQkclkMplMJpNpNJgFAHC73W632+12u91ut9sGddpwXDVaTSaTyWSyFsIikwFlMplMJpPJSBoNBDAA3G632+12u91ut9stggJtOBsOh8PhjFQul0wmk8lkMplMJpPBSCWNhAAAJm632+12u91ut9vtpjBtOBwOh8PhcDQcDkej0Wg0Go1Go9H+aDQajQazAAAAt9vtdrvdbrfb7QYMSBqRSOSCyWg0Go1Go+FoNBqNRqPRaDQajUajaQAAgNvtdrvdbrfb7XYDAACAAIFgWDQaDYgEKJFgMBgMBoPBSCQYDAYjkSwAA8Dtdrvdbrfb7Xa7AQAAAAAAAAAAAECAQCAQFAaDQmEwJAwGA9BgMBAGAADgdrvdbrfb7Xa73QAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAGAAAAwAAADgdrvdbjfg7Xa73W43AAAAGAQAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAcLvdbrfb7Xa73W632+12AwAAQAAAAAAAAIPAAAAAAAD+AAAAAAAAAAAAAAC43W632+12u91ut9vtdrvdbrfb7Xa7AQAAAAAAAACAAAAAAAAAAAAAAHC73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa7AW+32+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdgPebrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+0GvN1ut9vtdrvdbrfb7Xa73W632+12u91ut9vtdrvdbrfb7Xa73W632+12u91ut9sT7Xa73W632w14u91ut9vtdrsFAQA7"]

questionImg = newImage NONE [imgData GIF "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mUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMgALSPpgAABAKiaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMhngQ+IkAABAKiaTyWQyGVAmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMBgAAAAAAIBWTyWQymUwmk8lkMplMJpPJZDKgTCaTyWQymUwmk8lkMplMJgMAAAAAwJGKyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkQJlMJpPJAAAAIOVIxWT+MplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQAh8ORismAMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTAWUymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJgPKZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwGlMlkMplMJpPJZDKZTCaTyWQymUwmk8lkMplMJpPJZDKZTCaTyWQymUwmk8lkMpkTTCaTyWQymQwok8lkMplMJpMFAQA7"]
