#define label labxyz 
-- work-around GHC 5.02 bug which causes interface files to barf if you call a
-- type variable "label".

{- This module defines SimpleForm.hs, which is intended as a simple interface
   to filling in forms using HTk.  (Indeed, it is simple enough that it might
   be ported to some other GUI sometime.) -}
module SimpleForm(
   Form, -- This represents a series of input fields.
      -- A (Form x) represents a form yielding a value of type x
      -- Form is an instance of functor, so fmap works for it.
      -- But mapForm is more general.

   newFormEntry, -- :: (FormLabel label,FormValue value) 
      -- => label -> value -> Form x
      -- This creates a new form entry for a single item. 

   (//), -- :: Form value1 -> Form value2 -> Form (value1,value2)
      -- This combines two forms.  They will be displayed with one on top of
      -- the other.

   doForm, -- :: String -> Form x -> IO (Maybe x)
      -- This displays a form.  The first string is the title;
      -- the second the form.  As well as the entries in the form,
      -- "OK" and "Cancel" buttons are displayed.

   mapForm, -- :: (x -> Either String y) -> Form x -> Form y
      -- mapForm changes the type of a form.  When we press OK with doForm,
      -- the supplied function is called.  If it returns Right y, we return y
      -- and close the window; if it returns Left error, the error message is 
      -- displayed, and we continue.
   guardForm, -- :: (x -> Bool) -> String -> Form x -> Form x
      -- guardForm uses mapForm to check the value of x with the supplied
      -- error message.


   FormValue(..), -- This is a class of values which can be read in from a 
     -- simple form.  Instances include Int, String and Bool.
      -- A user friendly way of constructing new instances is to instance
      -- one of the following two classes.
--   FormRadioButton(..), -- This class is used for types which are suitable
      -- for being read with radio buttons, for example a small enumeration.
   FormTextField(..), -- This class is used for types which can be
      -- read in using a text field.

   FormLabel(..), -- This class represents things which can be used for
      -- labels in the form.  Instances include String and Image.

   WrappedFormLabel(..), -- this is an existentially wrapped type around
      -- values of type FormLabel.

   Radio(..), -- type for wrapping round something to use radio buttons.
   HasConfigRadioButton(..), -- for setting fancy configurations for
      -- radio buttons.

   -- Error messages
   WithError, -- type synonym for Either String a
   ) where

import Char

import HTk
--import qualified UtilWin
import DialogWin


-- -------------------------------------------------------------------------
-- The EnteredForm type
-- -------------------------------------------------------------------------

---
-- EnteredForm represents a form entry constructed in a given widget
-- The actions should be performed in the following sequence:
-- packAction
-- 0 or more uses of getFormValue
-- destroyAction
data EnteredForm value = EnteredForm {
   packAction :: IO (), -- packs the form entry into the widget.
   getFormValue :: IO (WithError value), 
      -- extracts value or produces an error message
   destroyAction :: IO () -- does any necessary clean-up. 
   }

mapEnteredForm :: (a -> b) -> EnteredForm a -> EnteredForm b
mapEnteredForm f
   (EnteredForm{packAction = packAction,getFormValue = getFormValue,
      destroyAction = destroyAction}) =
   EnteredForm {packAction = packAction,destroyAction = destroyAction,
      getFormValue = do
         we1 <- getFormValue
         return (mapWithError f we1)
      }

mapEnteredForm' :: (a -> WithError b) -> EnteredForm a -> EnteredForm b
mapEnteredForm' f 
   (EnteredForm{packAction = packAction,getFormValue = getFormValue,
      destroyAction = destroyAction}) =
   EnteredForm {packAction = packAction,destroyAction = destroyAction,
      getFormValue = do
         we1 <- getFormValue
         return (mapWithError' f we1)
      }

-- -------------------------------------------------------------------------
-- Error messages
-- -------------------------------------------------------------------------

type WithError a = Either String a -- error or result

mapWithError :: (a -> b) -> WithError a -> WithError b
mapWithError f (Left e) = Left e
mapWithError f (Right x) = Right (f x)

mapWithError' :: (a -> WithError b) -> WithError a -> WithError b
mapWithError' f (Left e) = Left e
mapWithError' f (Right a) = f a

pairWithError :: WithError a -> WithError b -> WithError (a,b)
-- we concatenate the errors, inserting a newline between them if there are two.
pairWithError (Right a) (Right b) = Right (a,b)
pairWithError (Left e) (Right b) = Left e
pairWithError (Right a) (Left f) = Left f
pairWithError (Left e) (Left f) = Left (e++"\n"++f)

-- -------------------------------------------------------------------------
-- The Form type and (//)
-- -------------------------------------------------------------------------

newtype Form value = Form (Toplevel -> IO (EnteredForm value))

instance Functor Form where
   fmap f (Form getEnteredForm0) =
      let
         getEnteredForm1 topLevel =
            do
               enteredForm1 <- getEnteredForm0 topLevel
               return (mapEnteredForm f enteredForm1)
      in
          Form getEnteredForm1


mapForm :: (x -> WithError y) -> Form x -> Form y
mapForm f (Form getEnteredForm0) =
   let
      getEnteredForm1 topLevel =
         do
            enteredForm1 <- getEnteredForm0 topLevel
            return (mapEnteredForm' f enteredForm1)
   in
       Form getEnteredForm1


infixr //

(//) :: Form value1 -> Form value2 -> Form (value1,value2)
(//) (Form enterForm1) (Form enterForm2) =
   let
      enterForm topLevel =
         do
            enteredForm1 <- enterForm1 topLevel
            enteredForm2 <- enterForm2 topLevel
            let
               enteredForm = EnteredForm {
                  packAction = (
                     do
                        packAction enteredForm1
                        packAction enteredForm2
                     ),
                  getFormValue = (
                     do
                        valueError1 <- getFormValue enteredForm1
                        valueError2 <- getFormValue enteredForm2
                        return (pairWithError valueError1 valueError2)
                     ),
                  destroyAction = (
                     do
                        destroyAction enteredForm1
                        destroyAction enteredForm2
                     )
                  }
            return enteredForm
   in
      Form enterForm

guardForm :: (x -> Bool) -> String -> Form x -> Form x
guardForm test mess =
  mapForm (\x -> if test x then Right x else Left mess)



-- -------------------------------------------------------------------------
-- The doForm action 
-- -------------------------------------------------------------------------

doForm :: String -> Form value -> IO (Maybe value)
doForm title (Form enterForm) =
   do
      (toplevel,enteredForm,okButton,cancelButton) <- delayWish (
         do
            toplevel <- createToplevel [text title]
            enteredForm <- enterForm toplevel
            -- create frame for "OK" and "Cancel" buttons.
            frame <- newFrame toplevel []
            (okButton :: Button String) <- newButton frame [text "OK"]
            (cancelButton :: Button String) <- newButton frame [text "Cancel"]
      
            -- Pack everything
            packAction enteredForm
            pack okButton [Side AtLeft]
            pack cancelButton [Side AtRight]
            pack frame [Side AtTop]
            return (toplevel,enteredForm,okButton,cancelButton)
         )

      -- Monitor ok and cancel buttons
      okEvent <- clicked okButton
      cancelEvent <- clicked cancelButton
      let
         handler =
               (do
                  okEvent
                  always (
                     do
                        valueError <- getFormValue enteredForm
                        case valueError of
                           Right value -> return (Just value)
                           Left error ->
                              do
                                 newErrorWin error []
--                                 UtilWin.error error
                                 sync handler
                     )
               )
            +> (do
                  cancelEvent
                  return Nothing
               )

      valueOpt <- sync handler

      -- finish off
      destroyAction enteredForm
      destroy toplevel

      return valueOpt
 
-- -------------------------------------------------------------------------
-- newFormEntry
-- -------------------------------------------------------------------------

newFormEntry :: (FormLabel label,FormValue value) => label -> value -> Form value
newFormEntry label value =
   let
      enterForm topLevel =
         do
            frame <- newFrame topLevel []
            packLabel <- formLabel frame label
            enteredForm1 <- makeFormEntry frame value
            let
               enteredForm = EnteredForm {
                  packAction = (
                     do
                        packLabel
                        packAction enteredForm1
                        pack frame [Side AtTop,Fill X]
                     ),
                  getFormValue = getFormValue enteredForm1,
                  destroyAction = destroyAction enteredForm1
                  }
            return enteredForm
   in
      Form enterForm

-- -------------------------------------------------------------------------
-- The FormLabel class
-- This is used for labels of fields in the form, and also for labels
-- of radio buttons.
-- -------------------------------------------------------------------------

class FormLabel label where
   formLabel :: Frame -> label -> IO (IO ())
   -- formLabel frame label creates a new label 
   -- (normally at the left of) the frame "frame" with detail label.  The action
   -- returned is the packing action.

instance FormLabel String where
   formLabel frame str =
      do
         label <- newLabel frame [text str,anchor West]
         return (pack label [Side AtLeft,Fill X])

instance FormLabel Image where
   formLabel frame image =
      do
         (label :: Label Image) <- newLabel frame [photo image]
         return (pack label [Side AtLeft])


-- We provide a heterogenous version of this too.
data WrappedFormLabel = forall label . FormLabel label 
   => WrappedFormLabel label

instance FormLabel WrappedFormLabel where
   formLabel frame (WrappedFormLabel label) = formLabel frame label

-- -------------------------------------------------------------------------
-- The FormValue class 
-- -------------------------------------------------------------------------

class FormValue value where
   makeFormEntry :: Frame -> value -> IO (EnteredForm value)
   -- Create a new form entry, given a default value.

-- -------------------------------------------------------------------------
-- Instance #1 - FormTextField's, corresponding to a single line of text.
-- -------------------------------------------------------------------------

class FormTextField value where
   makeFormString :: value -> String
      -- used for computing the initial string from the given default value
   readFormString :: String -> WithError value
      -- readFormString computes the value, or an error message.

-- Two examples

-- strings
instance FormTextField String where
   makeFormString str = str
   readFormString str = Right str

allSpaces :: String -> Bool
allSpaces = all isSpace

-- numbers
instance (Num a,Show a,Read a) => FormTextField a where
   makeFormString value = show value
   readFormString str = case reads str of
      [(value,rest)] | allSpaces rest -> Right value
      _ -> Left (show str ++ " is not a number")

instance FormTextField value => FormValue value where
   makeFormEntry frame defaultVal =
      do
         let defaultString = makeFormString defaultVal
         contentsVariable <- createTkVariable defaultString
         (entry :: Entry String) <- newEntry frame [variable contentsVariable]
         let 
            getFormValue =
               do
                  (contents :: String) <- readTkVariable contentsVariable
                  return (readFormString contents)
         let
            enteredForm = EnteredForm {
               packAction = pack entry [Side AtRight,Fill X],
               getFormValue = getFormValue,
               destroyAction = done
               }
         return enteredForm

-- -------------------------------------------------------------------------
-- Instance #2.   Maybe something that's an instance of FormTextField,
-- so corresponding to Maybe String or Maybe Number.
-- It is possible to nest FormTextField's Maybe(Maybe . . .) but this is
-- not recommended.
-- When reading a null string, this will be parsed as a value rather than
-- Nothing if possible; this happens for example with String.
-- -------------------------------------------------------------------------

instance FormTextField value => FormTextField (Maybe value) where
   makeFormString Nothing = ""
   makeFormString (Just value) = makeFormString value

   readFormString "" = case readFormString "" of
      Left _ -> Right Nothing
      Right x -> Right (Just x)
   readFormString str = mapWithError Just (readFormString str)

-- -------------------------------------------------------------------------
-- Instance #2 - Radio Buttons
-- If "x" is an instance of "Show", "Bounded" and "Enum", "Radio x" will be an
-- instance of FormValue, and will display the buttons in order.
-- But if you don't like this define your own instances of Show or,
-- for pictures, HasConfigRadioButton.
--
-- Radio Int is _not_ recommended.
-- -------------------------------------------------------------------------

data Radio x = Radio x | NoRadio
-- The NoRadio indicates that no radio button is selected.

class HasConfigRadioButton value where
   configRadioButton :: value -> Config (RadioButton Int String)

instance Show value => HasConfigRadioButton value where
   configRadioButton value = text (show value)

instance (HasConfigRadioButton value,Bounded value,Enum value) 
   => FormValue (Radio value) where
   makeFormEntry frame rvalue =
      do
         let
            minB :: value = minBound
            maxB :: value = maxBound

            minBoundInt :: Int
            minBoundInt = fromEnum minB
            maxBoundInt :: Int
            maxBoundInt = fromEnum maxB

            fromRValue :: Radio value -> Int
            fromRValue NoRadio = -1
            fromRValue (Radio x) = fromEnum x - minBoundInt

            toRValue :: Int -> Radio value
            toRValue (-1) = NoRadio
            toRValue i = 
               if i>= 0 && i<= maxBoundInt - minBoundInt
               then
                  Radio (toEnum (i+minBoundInt)) 
               else error 
                  ("SimpleForm.toRValue - radio button with odd number:"++
                     show i)

         radioVar <- createTkVariable (fromRValue rvalue)
         -- Add the radio buttons and get their packing actions.
         packActions <- mapM
            (\ val ->
               do
                  radioButton <- newRadioButton frame [
                     configRadioButton val,
                     variable radioVar,
                     value (fromRValue (Radio val))
                     ]
                  return (pack radioButton [Side AtLeft])
               )
            [minB .. maxB]
         let
            enteredForm = EnteredForm {
               packAction = sequence_ packActions,
               getFormValue = 
                  do
                     valInt <- readTkVariable radioVar
                     return (Right (toRValue valInt)),
               destroyAction = done
               }
         return enteredForm

-- -------------------------------------------------------------------------
-- Instance #3 - Check buttons a.k.a. Bools.
-- -------------------------------------------------------------------------

instance FormValue Bool where
   makeFormEntry frame b =
      do
         boolVar <- createTkVariable b
         checkButton <- newCheckButton frame [variable boolVar]
         let
            enteredForm = EnteredForm {
               packAction = pack checkButton [Side AtLeft],
               getFormValue = (
                  do
                     bool <- readTkVariable boolVar
                     return (Right bool)
                  ),
               destroyAction = done
               }
         return enteredForm

