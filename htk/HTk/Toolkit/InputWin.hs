{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic input window for record values and their fields.
module HTk.Toolkit.InputWin (
        InputWin,
        createInputWin,
        createInputWin',

        wait,
        waitValidate
        ) where

import Control.Exception

import HTk.Kernel.Core
import HTk.Toplevel.HTk
import HTk.Widgets.Space
import HTk.Toolkit.SelectBox
import HTk.Toolkit.ModalDialog
import HTk.Toolkit.InputForm
import Reactor.ReferenceVariables
import HTk.Toolkit.Separator

-- ---------------------------------------------------------------------------
-- Data Type
-- ---------------------------------------------------------------------------

-- | The @InputWin@ datatype.
data InputWin a = InputWin {
                            fWindow :: Toplevel,
                            fForm   :: InputForm a,
                            fEvents :: (Event Bool)
                            }

-- ---------------------------------------------------------------------------
-- Instantiations
-- ---------------------------------------------------------------------------

-- | Internal.
instance GUIObject (InputWin a) where
        toGUIObject iwin = toGUIObject (fWindow iwin)
        cname iwin = cname (fWindow iwin)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------
-- | Create an @InputWindow@ with a generic message box title
createInputWin' ::  String
   -- ^ the message for the headline
   -> [Config Message]
   -- ^ configs for the header message, e.g. font or aspect
   -> (Box -> IO (InputForm a))
   -- ^ the @InputForm@-function
   -> [Config Toplevel]
   -- ^ configuration for the toplevel (e.g. title)
   -> IO (InputWin a, InputForm a)
   -- ^ the @InputWindow@ and @InputForm@
createInputWin' str hdconfs ifun tpconfs =
 delayWish $ do
  tp <- createToplevel ([text "Input Form Window"]++tpconfs)
  pack tp [Expand On, Fill Both]

  b <- newVBox tp []
  pack b [Expand On, Fill Both]

  let fmsg = xfont {family = Just Times, weight = Just Bold,
                    slant = Just Roman, points = (Just 180)}
  msg <- newMessage b ([text str, borderwidth 0, aspect 750, font fmsg]++
                        hdconfs)

  pack msg [Expand On, Fill Both, PadX (cm 0.5), PadY (cm 0.5)]

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

  but1 <- addButton sb [text "Ok"] [Expand On, Side AtRight]
  but2 <- addButton sb [text "Cancel"] [Expand On, Side AtRight]

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


-- Create an <code>InputWindow</code>, title given as a string
createInputWin :: String
   -- ^ the title
   -> (Box -> IO (InputForm a))
   -- ^ the @InputForm@-function
   -> [Config Toplevel]
   -- ^ configuration for the toplevel
   -> IO (InputWin a, InputForm a)
   -- ^ the @InputWindow@ and @InputForm@
createInputWin str =  createInputWin' str []


-- ---------------------------------------------------------------------------
-- Additional Funcitons
-- ---------------------------------------------------------------------------
-- | Wait for the user to end the dialog.
wait :: InputWin a
   -- ^ the @InputWindow@ to wait for
   -> Bool
   -- ^ grep focus
   -> IO (Maybe a)
   -- ^ Nothing or Just (the data stored in the @IputForm@)
wait win@(InputWin tp form@(InputForm b e) ev) modality = do
 -- before we can question a user we should fill all the fields with
 -- their initial values (to be done automatically)
 fst <- getRef e
 initiate form (fFormValue fst)
 internalWait win (const (return True)) modality

-- | Wait for the user to end the dialog, and validate the result
waitValidate :: InputWin a
   -- ^ the @InputWindow@ to wait for
   -> (a-> IO Bool)
   -- ^ grep focus
   -> Bool
   -- ^ check the value of the form
   -> IO (Maybe a)
   -- ^ Nothing or Just (the data stored in the @IputForm@)
waitValidate win@(InputWin tp form@(InputForm b e) ev) validate  modality = do
 -- before we can question a user we should fill all the fields with
 -- their initial values (to be done automatically)
 fst <- getRef e
 initiate form (fFormValue fst)
 internalWait win validate modality

internalWait :: InputWin a -> (a-> IO Bool)-> Bool -> IO (Maybe a)
internalWait win@(InputWin tp form ev) val mod = do
 ans <- modalInteraction tp False mod ev
 case ans of
  False -> do
            destroy win
            return Nothing
  True  -> do
            res <- try (getFormValue form)
            case res of
             Left (e :: SomeException) -> internalWait win val mod
             Right res' -> do
                 chck <- val res'
                 if chck then do
                     destroy win
                     return (Just res')
                   else internalWait win val mod

initiate :: InputForm a -> Maybe a -> IO ()
initiate form Nothing = done
initiate form (Just val) = setFormValue form val
