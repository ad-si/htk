{- This module defines SimpleForm.hs, which is intended as a simple interface
   to filling in forms using HTk.  (Indeed, it is simple enough that it might
   be ported to some other GUI sometime.) -}
module SimpleForm(
   Form, -- This represents a series of input fields.
      -- A (Form x) represents a form yielding a value of type x
      -- Form is an instance of functor, so fmap works for it.
      -- But mapForm is more general.

   newFormEntry, -- :: (FormLabel label,FormValue value) 
      -- => label -> value -> Form value
      -- This creates a new form with a single labelled entry. 
      -- The FormValue class includes text fields and radio buttons.

   emptyForm, -- :: Form ()
      -- The empty form (rather boring).

   newFormMenu, -- :: (FormLabel label) => label -> HTkMenu value 
      -- -> Form (Maybe value)
      -- This creates a new form with a single labelled entry, selected
      -- by a menu.  A value of Nothing indicates that the user did not
      -- click this menu.
      -- The String is used to label the menu button containing the menu.

   (//), -- :: Form value1 -> Form value2 -> Form (value1,value2)
      -- This combines two forms.  They will be displayed with one on top of
      -- the other.

   (\\), -- :: Form value1 -> Form value2 -> Form (value1,value2)
      -- Like //, but combines two forms side-by-side.

   column, -- :: [Form value] -> Form [value]
   row, -- :: [Form value] -> Form [value]
      -- Two other combinators obtained by iterating (//) and (\\)

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
   EmptyLabel(EmptyLabel),
      -- Another instance of FormLabel, which we use if we don't want a label.

   WrappedFormLabel(..), -- this is an existentially wrapped type around
      -- values of type FormLabel.

   Radio(..), -- type for wrapping round something to use radio buttons.
   HasConfigRadioButton(..), -- for setting fancy configurations for
      -- radio buttons.
   ) where

import Char

import IORef

import Computation

import Events
import Channels


import HTk
--import qualified UtilWin
import DialogWin
import MenuButton

import MenuType
import HTkMenu

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
-- The Form type and (//)
-- -------------------------------------------------------------------------

newtype Form value = Form (forall container . Container container 
   => container -> IO (EnteredForm value))

instance Functor Form where
   fmap f (Form getEnteredForm0) =
      let
         getEnteredForm1 container =
            do
               enteredForm1 <- getEnteredForm0 container
               return (mapEnteredForm f enteredForm1)
      in
          Form getEnteredForm1


mapForm :: (x -> WithError y) -> Form x -> Form y
mapForm f (Form getEnteredForm0) =
   let
      getEnteredForm1 container =
         do
            enteredForm1 <- getEnteredForm0 container
            return (mapEnteredForm' f enteredForm1)
   in
       Form getEnteredForm1


infixr 8 // -- so it binds less tightly than \\

(//) :: Form value1 -> Form value2 -> Form (value1,value2)
(//) (Form enterForm1) (Form enterForm2) =
   let
      enterForm container =
         do
            enteredForm1 <- enterForm1 container
            enteredForm2 <- enterForm2 container
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
-- The \\ function
-- -------------------------------------------------------------------------

(\\) :: Form x -> Form y -> Form (x,y)
(\\) (Form enterForm1) (Form enterForm2) =
   let
      enterForm container =
         -- This is somewhat clumsy as we can't specify the pack action
         -- of the internal forms, so have to wrap them in two further forms.
         do
            frame <- newFrame container []
            frame1 <- newFrame frame []
            enteredForm1 <- enterForm1 frame1
            frame2 <- newFrame frame []
            enteredForm2 <- enterForm2 frame2
            let 
               enteredForm = EnteredForm {
                  packAction = (
                     do
                        packAction enteredForm1
                        pack frame1 [Side AtLeft]
                        packAction enteredForm2
                        pack frame2 [Side AtLeft]
                        pack frame []
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

infixr 9 \\ -- so it binds more tightly than //
      

-- -------------------------------------------------------------------------
-- emptyForm, column and row
-- -------------------------------------------------------------------------

emptyForm :: Form ()
emptyForm = Form (\ container ->
   return (EnteredForm {
      packAction = done,
      getFormValue = return (Right ()),
      destroyAction = done
      })
   )

emptyFormList :: Form [a]
emptyFormList = fmap (const []) emptyForm

column :: [Form value] -> Form [value]
column forms =
   foldr
      (\ form listForm -> fmap (uncurry (:)) (form // listForm))
      emptyFormList
      forms

row :: [Form value] -> Form [value]
row forms =
   foldr
      (\ form listForm -> fmap (uncurry (:)) (form // listForm))
      emptyFormList
      forms

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
            okButton <- newButton frame [text "OK"]
            cancelButton <- newButton frame [text "Cancel"]
      
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
                                 createErrorWin error []
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

newFormEntry :: (FormLabel label,FormValue value) => label -> value 
   -> Form value
newFormEntry label value =
   let
      enterForm container =
         do
            frame <- newFrame container []
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
-- newFormMenu
-- -------------------------------------------------------------------------

newFormMenu :: FormLabel label => label -> HTkMenu value -> Form (Maybe value)
newFormMenu label htkMenu =
   let
      enterForm container =
         do
            frame <- newFrame container []
            packLabel <- formLabel frame label
            enteredForm1 <- makeFormMenuEntry frame htkMenu
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


makeFormMenuEntry :: Frame -> HTkMenu value -> IO (EnteredForm (Maybe (value)))
makeFormMenuEntry frame htkMenu =
   do
      (menuButton,menuEvent) <- compileHTkMenu frame htkMenu
      -- Set up things for the thread which watches for menu events so that
      -- it picks up the last one.
      resultRef <- newIORef Nothing -- put the result here!
      killChannel <- newChannel -- terminate watcher thread here!
      let
         menuEventThread =
               (do
                  menuClick <- menuEvent
                  always (writeIORef resultRef (Just menuClick))
                  menuEventThread
               )
            +> receive killChannel

      spawnEvent menuEventThread

      return (EnteredForm{
         packAction = pack menuButton [],
         getFormValue = ( 
            do
               valueOpt <- readIORef resultRef
               return (Right valueOpt)
            ),
         destroyAction = sync (send killChannel ())
         })

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
         label <- newLabel frame [photo image]
         return (pack label [Side AtLeft])


-- We provide a heterogenous version of this too.
data WrappedFormLabel = forall label . FormLabel label 
   => WrappedFormLabel label

instance FormLabel WrappedFormLabel where
   formLabel frame (WrappedFormLabel label) = formLabel frame label

-- Finally, a label which actually does nothing at all.
data EmptyLabel = EmptyLabel

instance FormLabel EmptyLabel where
   formLabel _ _ = return done


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
   configRadioButton :: value -> Config (RadioButton Int)

instance Show value => HasConfigRadioButton value where
   configRadioButton value = text (show value)

instance (HasConfigRadioButton value,Bounded value,Enum value) 
   => FormValue (Radio value) where
   makeFormEntry frame rvalue =
      do
         let
            minB :: value
            minB = minBound
            maxB :: value
            maxB = maxBound

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

