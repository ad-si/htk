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
import InputForm
import ReferenceVariables

-- ---------------------------------------------------------------------------
-- Data Type
-- ---------------------------------------------------------------------------

data InputWin a = InputWin {
                            fWindow :: Toplevel,
		  	    fForm   :: InputForm a,
			    fEvents :: (Event Bool)  
			    }

-- ---------------------------------------------------------------------------
-- Instantiations
-- ---------------------------------------------------------------------------

instance GUIObject (InputWin a) where
        toGUIObject iwin = toGUIObject (fWindow iwin)
        cname iwin = cname (fWindow iwin)

-- ---------------------------------------------------------------------------
-- Dialog
-- ---------------------------------------------------------------------------

newInputDialog :: String -> Maybe a -> [Config Toplevel] -> IO (InputWin a)
newInputDialog  str val tpconf = do
        newInputWin str val tpconf

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

newInputWin :: String -> Maybe a -> [Config Toplevel] -> IO (InputWin a)
newInputWin str val tpconfs =
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

  formbox <- newVBox b []
  pack formbox [Expand On, Fill Both, PadX (cm 0.5)]

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

  sp5 <- newSpace b (cm 0.3) []
  pack sp5 [Fill X]

  form <- newInputForm formbox val []

  case val of
   Nothing -> return (InputWin tp form ev)
   Just val' -> do
                 return (InputWin tp form ev)
  where fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}


-- ---------------------------------------------------------------------------
-- Additional Funcitons
-- ---------------------------------------------------------------------------
wait :: InputWin a -> Bool -> IO (Maybe a)
wait win@(InputWin tp form@(InputForm b e) ev) modality = do
 -- before we can question a user we should fill all the fields with
 -- their initial values (to be done automatically)
 fst <- getRef e
 initiate form (fFormValue fst)
 ans <- modalInteraction tp False modality ev
 case ans of
  False -> do 
            destroy win
            return Nothing
  True  -> do 
            res <- try (getFormValue form)
	    case res of 
	     Left e -> wait win modality
	     Right res' -> do
                            destroy win	    
                            return (Just res')

initiate :: InputForm a -> Maybe a -> IO ()
initiate form Nothing = done
initiate form (Just val) = setFormValue form val
