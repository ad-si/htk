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

---
-- Basic input window for record values and their fields.
module InputWin (
        module InputForm,

        InputWin,
        newInputWin,

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
import Separator

-- ---------------------------------------------------------------------------
-- Data Type
-- ---------------------------------------------------------------------------

---
-- The <code>InputWin</code> datatype.
data InputWin a = InputWin {
                            fWindow :: Toplevel,
		  	    fForm   :: InputForm a,
			    fEvents :: (Event Bool)  
			    }

-- ---------------------------------------------------------------------------
-- Instantiations
-- ---------------------------------------------------------------------------

---
-- Internal.
instance GUIObject (InputWin a) where
---
-- Internal.
        toGUIObject iwin = toGUIObject (fWindow iwin)
---
-- Internal.
        cname iwin = cname (fWindow iwin)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------
---
-- Create an <code>InputWindow</code>.
-- @param  str      - message to be displayed in the window
-- @param  ifun     - the <code>InputForm</code>-function
-- @return result   - the <code>InputWindow</code> and <code>InputForm</code>
newInputWin :: String -> (Box -> IO (InputForm a)) -> [Config Toplevel] -> IO (InputWin a, InputForm a)
newInputWin str ifun tpconfs =
 do
  tp <- createToplevel (tpconfs++[text "Input Form Window"])
  pack tp [Expand On, Fill Both]
 
  b <- newVBox tp []
  pack b [Expand On, Fill Both]
 
  msg <- newEditor b [value str, size (30,5), borderwidth 0, state Disabled, wrap WordWrap, font fmsg] :: IO (Editor String)
  pack msg[Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]

  sp1 <- newSpace b (cm 0.15) []
  pack sp1 [Expand Off, Fill X, Side AtTop]
 
  newHSeparator b
  
  sp2 <- newSpace b (cm 0.15) []
  pack sp2 [Expand Off, Fill X, Side AtTop]

  formbox <- newVBox b []
  pack formbox [Expand On, Fill Both, PadX (cm 0.5)]

  form <- ifun formbox
  
  sp3 <- newSpace b (cm 0.15) []
  pack sp3 [Expand Off, Fill X, Side AtBottom]
 
  newHSeparator b
  
  sp4 <- newSpace b (cm 0.15) []
  pack sp4 [Expand Off, Fill X, Side AtBottom]

  sb <- newSelectBox b Nothing []
  pack sb [Expand Off, Fill X, Side AtBottom]

  but1 <- addButton sb [text "Ok"] [Expand On, Side AtRight] :: IO (Button String)
  but2 <- addButton sb [text "Cancel"] [Expand On, Side AtRight] :: IO (Button String)
  
  clickedbut1 <- clicked but1
  clickedbut2 <- clicked but2
  
  let ev = (clickedbut1 >> (always (return True))) +> (clickedbut2 >> (always (return False)))

  sp5 <- newSpace b (cm 0.3) []
  pack sp5 [Fill X]

  --form <- newInputForm formbox val []

  --case val of
  -- Nothing -> return (InputWin tp form ev)
  -- Just val' -> do
  return ((InputWin tp form ev), form)
 where fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}

-- ---------------------------------------------------------------------------
-- Additional Funcitons
-- ---------------------------------------------------------------------------
---
-- Wait for the user to end the dialog.
-- @param win       - the <code>InputWindow</code> to wait for
-- @param modality  - grep focus
-- @return result   - Nothing or Just (the data stored in the <code>IputForm</code>)
wait :: InputWin a -> Bool -> IO (Maybe a)
wait win@(InputWin tp form@(InputForm b e) ev) modality = do
 -- before we can question a user we should fill all the fields with
 -- their initial values (to be done automatically)
 fst <- getRef e
 initiate form (fFormValue fst)
 internalWait win modality

internalWait :: InputWin a -> Bool -> IO (Maybe a)
internalWait win@(InputWin tp form ev) modality = do
 ans <- modalInteraction tp False modality ev
 case ans of
  False -> do 
            destroy win
            return Nothing
  True  -> do 
            res <- try (getFormValue form)
	    case res of 
	     Left e -> internalWait win modality
	     Right res' -> do
                            destroy win	    
                            return (Just res')

initiate :: InputForm a -> Maybe a -> IO ()
initiate form Nothing = done
initiate form (Just val) = setFormValue form val
