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

import Core
-- --------------------------------------------------------------------------
--  Types 
-- --------------------------------------------------------------------------            
type Choice a = (String,a)

data Dialog a = Dialog {
                        fWindow    :: Toplevel,
                        fMessage   :: (Message String),
                        fLabel     :: (Label BitMap),
                        fSelectBox :: (SelectBox a),
                        fEvents    :: (Event a)
                        } deriving Eq

-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------            
instance GUIObject (Dialog a) where
        toGUIObject dlg = toGUIObject (fWindow dlg)
        cname dlg = cname (fWindow dlg)

instance HasText (Dialog a) String where
        text t dlg = do {
                configure (fMessage dlg) [text t];
                return dlg
                }
        getText dlg = getText (fMessage dlg)

instance HasBitMap (Dialog a) where
        bitmap bm dlg = do {
                configure (fLabel dlg) [bitmap bm];
                try(configure (fLabel dlg) []);
--                        pad Horizontal (cm 0.5), pad Vertical (cm 0.5)]);
                return dlg
                }
        getBitMap dlg = getBitMap (fLabel dlg)

---
-- out
--instance Reactive Dialog a where
--       triggered dlg = (fEvents dlg)

--instance HasTrigger Dialog a where
--        getTrigger = return . triggered

--instance Synchronized (Dialog a) where
--        synchronize dlg = synchronize (toGUIObject dlg)



-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------           
--forkDialog :: Reactive m a => m a -> (a -> IO b) -> IO ()
--forkDialog o h =
--        forkIOquiet "HTk.forkDialog" (sync (
--                triggered o >>>= \v -> do {h v; done}
--          )) >>= (return . (const ()))


-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------           
newAlertWin :: String -> [Config Toplevel] -> IO ()
newAlertWin str wol = newDialogWin choices Nothing [text str] (defs ++ wol)
 where choices = [("Continue",())]
       defs = [text "Alert Window"]


newErrorWin :: String -> [Config Toplevel] -> IO ()
newErrorWin str confs = newAlertWin str ([text "Error Message"] ++ confs)

newWarningWin :: String -> [Config Toplevel] -> IO ()
newWarningWin str confs = newAlertWin str ([text "Warning Message"] ++ confs)

newConfirmWin :: String -> [Config Toplevel] -> IO Bool
newConfirmWin str wol = newDialogWin choices (Just 0) confs (defs ++ wol)
 where choices = [("Ok",True),("Cancel",False)]
       defs = [text "Confirm Window"]
       confs = [text str,bitmap question]

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
  b <- newVBox tp []
  pack b []
  b2 <- newHBox b []
  pack b2 []
  lbl <- newLabel b2 []
  pack lbl []
  msg <- newMessage b2[]
  pack msg[]
  --sep <- newSeparator b []
  --pack sep
  ---
  -- SelectBox replacement until i figured out how it really works
  sb <- newSelectBox b Nothing []
  b3 <- newHBox b []
  pack b3 []  
  events <- mapM (createChoice b3) choices;
  let ev = choose events
  dlg <- configure (Dialog tp msg lbl sb ev) confs;
  return dlg
  where l = length choices
        fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}
        createChoice :: Box -> Choice a -> IO (Event a)
        createChoice b (str,val) = 
	 do
          but <- newButton b [text str] :: IO (Button String)
	  pack but []
	  clickedbut <- clicked but
          return (clickedbut >> (always (return val)))	  

{-
dialog :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Toplevel] -> IO (Dialog a)
dialog choices def confs wol = do {
        b <- newVBox [fill Horizontal, relief Groove, borderwidth (cm 0.05) ];
        b2 <- newHBox [flexible, parent b];
        lbl <- newLabel [parent b2];
        msg <- newMessage [pad Horizontal (cm 1), 
                pad Vertical (cm 0.5), 
                flexible, 
                width (cm 10), 
                justify JustCenter,
                font fmsg,
                parent b2
                ];
        newSeparator [expand On, fill Horizontal,parent b];
        newSpace (cm 0.2) [parent b];
        sb <- newSelectBox def [anchor Center, fill Horizontal, parent b];
        bts <- mapM (createChoice sb) choices;
        newSpace (cm 0.2) [parent b];
        win <- newWindow b ([text "Dialog Window", sizeFrom Program] ++ wol);
        unless (def == Nothing) ( do {
                interactor (\iact ->
                            closed win >>> do {
                                selectDefault sb; stop iact}
                        +>  keyPressed win "Return" >>> do {
                                selectDefault sb; stop iact}
                        );
                done
                });
        ev <- getTrigger sb;
        dlg <- configure (Dialog win msg lbl sb ev) confs;
        renderWindow win;
        return dlg
} where l = length choices
        fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}
        createChoice :: SelectBox a -> Choice a -> IO (Button a)
        createChoice sb (str,val) = 
                newButton [text str,
                        expand On,
                        pad Horizontal (cm 1.0),
                        command (\() -> return val),    
                        pad Vertical (cm 0.5),
                        parent sb 
                        ];
-}




