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

        forkDialog


        ) where

import Concurrency
import HTk
import Message
import Label
import BitMap
import Separator
import Keyboard
import Button
import Font
import Space
import SelectBox
import ModalDialog
import Interaction()
import Debug(debug)

-- --------------------------------------------------------------------------
--  Types 
-- --------------------------------------------------------------------------            
type Choice a = (String,a)

data Dialog a = Dialog {
                        fWindow    :: Window,
                        fMessage   :: (Message String),
                        fLabel     :: (Label BitMap),
                        fSelectBox :: (SelectBox a),
                        fEvents    :: (IA a)
                        }


-- --------------------------------------------------------------------------
--  Instances 
-- --------------------------------------------------------------------------            
instance GUIObject (Dialog a) where
        toGUIObject dlg = toGUIObject (fWindow dlg)
        cname dlg = cname (fWindow dlg)

instance HasText (Dialog a) String where
        text t dlg = do {
                configure (fMessage dlg) [value t];
                return dlg
                }
        getText dlg = getValue (fMessage dlg)

instance HasBitMap (Dialog a) where
        bitmap bm dlg = do {
                configure (fLabel dlg) [bitmap bm];
                try(configure (fLabel dlg) [
                        pad Horizontal (cm 0.5), pad Vertical (cm 0.5)]);
                return dlg
                }
        getBitMap dlg = getBitMap (fLabel dlg)

instance Reactive Dialog a where
        triggered dlg = (fEvents dlg)

instance HasTrigger Dialog a where
        getTrigger = return . triggered

instance Synchronized (Dialog a) where
        synchronize dlg = synchronize (toGUIObject dlg)



-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------           
forkDialog :: Reactive m a => m a -> (a -> IO b) -> IO ()
forkDialog o h =
        forkIOquiet "HTk.forkDialog" (sync (
                triggered o >>>= \v -> do {h v; done}
          )) >>= (return . (const ()))


-- --------------------------------------------------------------------------
--  Derived Dialog Window 
-- --------------------------------------------------------------------------           
newAlertWin :: String -> [Config Window] -> IO ()
newAlertWin str wol = newDialogWin choices Nothing [text str] (defs ++ wol)
 where choices = [("Continue",())]
       defs = [text "Alert Window"]


newErrorWin :: String -> [Config Window] -> IO ()
newErrorWin str confs = newAlertWin str ([text "Error Message"] ++ confs)

newWarningWin :: String -> [Config Window] -> IO ()
newWarningWin str confs = newAlertWin str ([text "Warning Message"] ++ confs)

newConfirmWin :: String -> [Config Window] -> IO Bool
newConfirmWin str wol = newDialogWin choices (Just 0) confs (defs ++ wol)
 where choices = [("Ok",True),("Cancel",False)]
       defs = [text "Confirm Window"]
       confs = [text str,bitmap question]

newDialogWin :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Window ] -> IO a
newDialogWin choices def confs wol = 
   do 
      debug "nDW1"
      dlg <- dialog choices def confs wol 
      debug "nDW2" 
      result <- modalInteraction (fWindow dlg) True (triggered dlg)
      debug "nDW3"   
      return result

-- --------------------------------------------------------------------------
--  Base Dialog Window 
-- --------------------------------------------------------------------------           
dialog :: [Choice a] -> Maybe Int -> [Config (Dialog a)] -> [Config Window] -> IO (Dialog a)
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

