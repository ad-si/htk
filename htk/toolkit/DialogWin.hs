{- #########################################################################

MODULE        : DialogWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic dialog window and a couple of predefined 
                abstractions!


   ######################################################################### -}


module DialogWin (
        Dialog,

        dialog,

        newAlertWin,
        newErrorWin,
        newWarningWin,
        newConfirmWin,
        newDialogWin,

--        forkDialog


        ) where

import HTk
import Message
import Label
import BitMap
import Keyboard
import Button
import Font
import Space
import SelectBox
import ModalDialog
import Toplevel
import MarkupText

import Core
-- --------------------------------------------------------------------------
--  Types 
-- --------------------------------------------------------------------------            
type Choice a = (String,a)

data Dialog a = Dialog {
                        fWindow    :: Toplevel,
                        fMessage   :: (Editor String),
                        fLabel     :: (Label Image),
                        fSelectBox :: (SelectBox String),
                        fEvents    :: (Event a)
                        } deriving Eq

-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------            
instance GUIObject (Dialog a) where
        toGUIObject dlg = toGUIObject (fWindow dlg)
        cname dlg = cname (fWindow dlg)

--instance HasBitMap (Dialog a) where
--        bitmap bm dlg = do {
--                configure (fLabel dlg) [bitmap bm];
--                try(configure (fLabel dlg) []);
--                        pad Horizontal (cm 0.5), pad Vertical (cm 0.5)]);
--                return dlg
--                }
--        getBitMap dlg = getBitMap (fLabel dlg)

instance HasPhoto (Dialog a) where
        photo p dlg = do {
                configure (fLabel dlg) [photo p];
                return dlg
                }

instance HasMarkupText (Dialog a) where
         new t dlg = do {
                 configure (fMessage dlg) [new t];
                 return dlg
                 }

-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------           
--forkDialog :: Reactive m a => m a -> (a -> IO b) -> IO ()
--forkDialog o h =
--        forkIOquiet "HTk.forkDialog" (sync (
--                triggered o >>>= \v -> do {h v; done}
--          )) >>= (return . (const ()))

-- --------------------------------------------------------------------------
-- Images for the various Dialog Windows
-- --------------------------------------------------------------------------
-- wie ist der string zu interpretieren und wie wird er erzeugt?
upImg = newImage NONE [imgData GIF "R0lGODlhFAAUAKEAAP//////AAAAAP///yH5BAEAAAMALAAAAAAUABQAAAJAnI+py+0Po1Si2iiC3gLZn21iN4TiWXGdeWqfu7bqW5WyG6RZvbOjyculWkOhTQh6wY7I5I95Q5GSVNChWp0oCgA7"]

-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------
newAlertWin :: [MarkupText] -> [Config Toplevel] -> IO ()
newAlertWin str wol = newDialogWin choices Nothing [new str] (defs ++ wol)
 where choices = [("Continue",())]
       defs = [text "Alert Window"]


newErrorWin :: [MarkupText] -> [Config Toplevel] -> IO ()
newErrorWin str wol = newDialogWin choices Nothing [new str] (defs++wol)
 where choices = [("Continue",())]
       defs = [text "Error Message"]
       

newWarningWin :: [MarkupText] -> [Config Toplevel] -> IO ()
newWarningWin str confs = newAlertWin str ([text "Warning Message"] ++ confs)

newConfirmWin :: [MarkupText] -> [Config Toplevel] -> IO Bool
newConfirmWin str wol = 
 do
  let choices = [("Ok",True),("Cancel",False)]
  let defs = [text "Confirm Window"]
  upImg' <- upImg
  let confs = [new str, photo upImg']
  newDialogWin choices (Just 0) confs (defs ++ wol)

newDialogWin :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Toplevel] -> IO a
newDialogWin choices def confs wol = 
   do 
      dlg <- dialog choices def confs wol
      result <- modalInteraction (fWindow dlg) True True (fEvents dlg)
      return result

-- --------------------------------------------------------------------------
--  Base Dialog Window 
-- --------------------------------------------------------------------------
dialog :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Toplevel] -> IO (Dialog a)
dialog choices def confs tpconfs =
 do
  tp <- createToplevel tpconfs
  pack tp [Expand On, Fill Both]

  b <- newVBox tp [relief Groove, borderwidth (cm 0.05)]
  pack b [Expand On, Fill Both]

  b2 <- newHBox b []
  pack b2 [Expand On, Fill Both]

  lbl <- newLabel b2 []
  pack lbl [Expand On, Fill Both]

  msg <- newEditor b2[size (30,5), borderwidth 0, state Disabled, wrap WordWrap, HTk.font fmsg] :: IO (Editor String)
  pack msg[Expand On, Fill Both, PadX (cm 1), PadY (cm 0.5)]

  sb <- newSelectBox b Nothing [relief Ridge, borderwidth (cm 0.05)]
  pack sb [Expand Off, Fill X, Side AtBottom]

  events <- mapM (createChoice sb) choices
  let ev = choose events

  dlg <- configure (Dialog tp msg lbl sb ev) confs;
  return dlg
  where fmsg = xfont { family = Just Courier, weight = Just Bold, points = (Just 18) }
	createChoice :: SelectBox String -> Choice a -> IO (Event a)
        createChoice sb (str,val) = 
         do
          but <- addButton sb [text str] [Expand On, Side AtRight] :: IO (Button String)
          clickedbut <- clicked but
          return (clickedbut >> (always (return val)))





