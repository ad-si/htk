{- #########################################################################

MODULE        : InputWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic input window for record values and their fields.
                The third argument to inputWin is a is-well-formedness
                function that is finally called to ensure that the entered
                record value is legal. The legality of the individual fields
                is checked by getValue!

TO BE DONE    : The handling of Returns within a form must be considered.

                In presense of text fields, these should normally be
                flexible rather than the header message!!


   ######################################################################### -}


module InputWin (
--        module InputForm,
--        forkDialog,

        InputWin(..),
        newInputWin,
        newInputDialog,

        wait
        ) where

import Core
import HTk
import Space
import SelectBox
import ModalDialog
import DialogWin

-- ---------------------------------------------------------------------------
-- Data Type
-- ---------------------------------------------------------------------------

data InputWin = InputWin {
                          fWindow :: Toplevel,
			  fForm   :: Box,
			  fEvents :: (Event Bool)
			 }

-- ---------------------------------------------------------------------------
-- Instantiations
-- ---------------------------------------------------------------------------

instance GUIObject (InputWin) where
        toGUIObject iwin = toGUIObject (fWindow iwin)
        cname iwin = cname (fWindow iwin)

-- ---------------------------------------------------------------------------
-- Dialog
-- ---------------------------------------------------------------------------

newInputDialog :: String -> [Config Toplevel] -> IO (InputWin)
newInputDialog  str tpconf = do
        newInputWin str tpconf

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

newInputWin :: String -> [Config Toplevel] -> IO (InputWin)
newInputWin str tpconfs =
 do
  tp <- createToplevel (tpconfs++[text "Input Form Window"])
  pack tp [Expand On, Fill Both]
 
  b <- newVBox tp [relief Raised, borderwidth (cm 0.05)]
  pack b [Expand On, Fill Both]
 
  msg <- newMessage b [text str, font fmsg] :: IO (Message String)
  pack msg [Expand Off, Fill X, PadX (cm 0.5), Side AtTop]

  sp1 <- newSpace b (cm 0.3) []
  pack sp1 [Expand Off, Fill X]
 
  sp2 <- newSpace b (cm 0.3) []
  pack sp2 [Expand Off, Fill X]

  form <- newVBox b []
  pack form [Expand On, Fill Both, PadX (cm 0.5)]

  sp3 <- newSpace b (cm 0.3) []
  pack sp3 [Expand Off, Fill X]
 
  sp4 <- newSpace b (cm 0.3) []
  pack sp4 [Expand Off, Fill X]

  sb <- newSelectBox b Nothing [relief Ridge, borderwidth (cm 0.05)]
  pack sb [Expand Off, Fill X, Side AtBottom]

  but1 <- addButton sb [text "Ok"] [Expand On, Side AtRight] :: IO (Button String)
  but2 <- addButton sb [text "Cancel"] [Expand On, Side AtRight] :: IO (Button String)
  
  clickedbut1 <- clicked but1
  clickedbut2 <- clicked but2
  
  let ev = (clickedbut1 >> (always (return True))) +> (clickedbut2 >> (always (return False)))

  sp5 <- newSpace b (cm 0.3) [];
  pack sp5 [Fill X]

  return (InputWin tp form ev)
  where fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}


-- ---------------------------------------------------------------------------
-- Additional Funcitons
-- ---------------------------------------------------------------------------
wait :: InputWin -> Bool -> IO (Bool)
wait form@(InputWin tp _ ev) modality = modalInteraction tp True modality ev
