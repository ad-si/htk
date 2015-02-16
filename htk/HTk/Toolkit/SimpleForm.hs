{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

-- |
-- Description: Graphical Form Input
--
-- This module defines 'SimpleForm's, a simple interface
-- to filling in forms using HTk.  (Indeed, it is simple enough that it might
-- be ported to some other GUI sometime.)
module HTk.Toolkit.SimpleForm(
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

   nullForm, -- :: FormLabel label => label -> Form ()
      -- also pretty boring; just displays the label but doesn't provide
      -- any interaction.

   newFormMenu, -- :: (FormLabel label) => label -> HTkMenu value
      -- -> Form (Maybe value)
      -- This creates a new form with a single labelled entry, selected
      -- by a menu.  A value of Nothing indicates that the user did not
      -- click this menu.
      -- The String is used to label the menu button containing the menu.

   newFormOptionMenu, -- :: (GUIValue a) => [a] -> Form a
      -- This creates an "option menu" button.  The
      -- advantage this has over a normal menu is that the value is shown.
      -- The first value in the list functions as a default value.

   newFormOptionMenu2, -- :: (GUIValue a) => [(a,b)] -> Form b
      -- Like newFormOptionMenu2 but returns the corresponding b value.


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
   doFormMust, -- :: String -> Form value -> IO value
      -- Like doForm, but the user is not provided with a cancel button.

   doFormList,
      -- :: String -> [(Form x,String)] -> IO (Event (WithError x),IO ())
      -- Display a sequence of forms, horizontally, one after another.
      -- To the right of each form is a button, with text given by the
      -- accompanying String.
      -- Clicking this button causes an event to be generated, carrying
      --    the accompanying form's value, or if invalid the error
      --    message.
      -- The first argument is the title of the window.  The
      -- IO () action returned closes the window.

   mapForm, -- :: (x -> WithError y) -> Form x -> Form y
      -- mapForm changes the type of a form.  When we press OK with doForm,
      -- the supplied function is called.  If it returns a y, we return y
      -- and close the window; if it returns an error message,
      -- the error message is
      -- displayed, and we continue.
   mapFormIO, -- :: (x -> IO (WithError y)) -> Form x -> Form y
      -- IO'based version of mapForm.

   guardForm, -- :: (x -> Bool) -> String -> Form x -> Form x
      -- guardForm uses mapForm to check the value of x with the supplied
      -- error message.
   guardFormIO, -- :: (x -> IO Bool) -> String -> Form x -> Form x
      -- IO'based version of guardForm.
   guardNothing, -- :: String -> Form (Maybe x) -> Form x
      -- Checks that Nothing is not returned, with the attached error
      -- message.

   FormValue(..), -- This is a class of values which can be read in from a
      -- simple form.  Instances include Int, String and Bool and ().
      -- (() just does nothing and is useful if you want a label without
      -- anything on it.)
      -- A user friendly way of constructing new instances is to instance
      -- one of the following two classes.

   mapMakeFormEntry,
   -- :: FormValue value2
   -- => (value1 -> value2) -> (value2 -> value1)
   -- -> (Frame -> value1 -> IO (EnteredForm value1))
    -- Function for creating one instance of FormValue from another.

--   FormRadioButton(..), -- This class is used for types which are suitable
      -- for being read with radio buttons, for example a small enumeration.
   FormTextField(..), -- This class is used for types which can be
      -- read in using a text field.
   FormTextFieldIO(..), -- Slightly more general version allowing IO actions.

   Password(..),
      -- newtype alias which specifies that the given FormTextField(IO)
      -- instance should not be displayed on the screen, but replaced by
      -- '.' characters.
   FormLabel(..), -- This class represents things which can be used for
      -- labels in the form.  Instances include String and Image.
   EmptyLabel(EmptyLabel),
      -- Another instance of FormLabel, which we use if we don't want a label.

   WrappedFormLabel(..), -- this is an existentially wrapped type around
      -- values of type FormLabel.

   Radio(..), -- type for wrapping round something to use radio buttons.
   HasConfigRadioButton(..), -- for setting fancy configurations for
      -- radio buttons.

   editableTextForm, -- :: [Config Editor] -> Form String
      -- A form for typing (possibly several lines of) editable text.
   editableTextForm0, -- :: [Config Editor] -> Form String
      -- Like 'editableTextForm' but no scrollbars are displayed.

   ) where

import Data.Char

import Data.IORef
import Data.Typeable

import Util.ExtendedPrelude
import Util.BinaryAll(HasBinary(..),mapWrite,mapRead)
import Util.Messages

import Util.Computation

import Events.Events
import Events.Channels

import HTk.Toplevel.HTk
import HTk.Toolkit.HTkMenu

-- -------------------------------------------------------------------------
-- The EnteredForm type
-- -------------------------------------------------------------------------

-- | EnteredForm represents a form entry constructed in a given widget
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

mapEnteredFormIO' :: (a -> IO (WithError b)) -> EnteredForm a
   -> EnteredForm b
mapEnteredFormIO' f
   (EnteredForm{packAction = packAction,getFormValue = getFormValue,
      destroyAction = destroyAction}) =
   EnteredForm {packAction = packAction,destroyAction = destroyAction,
      getFormValue = do
         we1 <- getFormValue
         mapWithErrorIO' f we1
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

mapFormIO :: (x -> IO (WithError y)) -> Form x -> Form y
mapFormIO f (Form getEnteredForm0) =
   let
      getEnteredForm1 container =
         do
            enteredForm1 <- getEnteredForm0 container
            return (mapEnteredFormIO' f enteredForm1)
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
  mapForm (\x -> if test x then hasValue x else hasError mess)

guardFormIO :: (x -> IO Bool) -> String -> Form x -> Form x
guardFormIO test mess =
  mapFormIO (\ x ->
     do
        res <- test x
        return (if res then hasValue x else hasError mess)
     )

guardNothing :: String -> Form (Maybe x) -> Form x
guardNothing mess =
   mapForm (\ xOpt ->
      case xOpt of
      Nothing -> hasError mess
      Just x -> hasValue x
      )


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
-- emptyForm, nullForm, column and row
-- -------------------------------------------------------------------------

emptyForm :: Form ()
emptyForm = Form (\ container ->
   return (EnteredForm {
      packAction = done,
      getFormValue = return (hasValue ()),
      destroyAction = done
      })
   )

nullForm :: FormLabel label => label -> Form ()
nullForm label = newFormEntry label ()

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

doFormMust :: String -> Form value -> IO value
doFormMust title form =
   do
      (Just value) <- doForm1 False title form
      return value

doForm :: String -> Form value -> IO (Maybe value)
doForm = doForm1 True

doForm1 :: Bool -> String -> Form value -> IO (Maybe value)
doForm1 canCancel title (Form enterForm) =
   do
      (toplevel,enteredForm,okEvent,cancelEvent) <- delayWish (
         do
            toplevel <- createToplevel [text title]
            enteredForm0 <- enterForm toplevel
            -- create frame for "OK" and "Cancel" buttons.
            frame <- newFrame toplevel []
            okButton <- newButton frame [text "OK"]
            okEvent <- clicked okButton

            packAction enteredForm0
            pack okButton [Side AtLeft]

            cancelEvent <-
               if canCancel
                  then
                     do
                        cancelButton <- newButton frame [text "Cancel"]
                        pack cancelButton [Side AtRight]
                        clicked cancelButton
                  else
                     return never

            (destroyEvent,cancelBind) <- bindSimple toplevel Destroy

            let
               enteredForm =
                  enteredForm0 {
                     destroyAction =
                        do
                           destroyAction enteredForm0
                           cancelBind
                        }

            pack frame [Side AtTop]
            return (toplevel,enteredForm,okEvent,cancelEvent +> destroyEvent)
         )

      let
         handler =
               (do
                  okEvent
                  always (
                     do
                        valueError <- getFormValue enteredForm
                        case fromWithError valueError of
                           Right value -> return (Just value)
                           Left err ->
                              do
                                 errorMess err
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

doFormList :: String -> [(Form x,String)] -> IO (Event (WithError x),IO ())
doFormList title (formList :: [(Form x,String)]) =
   do
      let
         doOneForm :: Toplevel -> (Form x,String)
            -> IO (Event (WithError x),IO ())
         doOneForm toplevel (Form enterForm,buttonName) =
            do
               frame <- newFrame toplevel []
               leftFrame <- newFrame frame []

               enteredForm <- enterForm leftFrame
               button <- newButton frame [text buttonName,anchor East]
               clickEvent <- clicked button

               packAction enteredForm
               pack leftFrame [Side AtLeft,Anchor West]
               pack button [Side AtRight,Anchor East]
               pack frame [Side AtTop,Fill X]

               let
                  handler = clickEvent >>> getFormValue enteredForm
               return (handler,done)
      (toplevel,enterResults) <- delayWish (
         do
            toplevel <- createToplevel [text title]
            enterResults <- mapM (doOneForm toplevel) formList
            return (toplevel,enterResults)
         )
      (destroyEvent,unbind) <- bindSimple toplevel Destroy

      let
         event0 = choose (map fst enterResults)

         event1 = event0
            +> (do
               destroyEvent
               return (fail "Window destroyed")
               )

         destroyWindow :: IO ()
         destroyWindow =
            do
               mapM_ snd enterResults
               unbind
               destroy toplevel
      return (event1,destroyWindow)


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

      _ <- spawnEvent menuEventThread

      return (EnteredForm{
         packAction = pack menuButton [],
         getFormValue = (
            do
               valueOpt <- readIORef resultRef
               return (hasValue valueOpt)
            ),
         destroyAction = sync (send killChannel ())
         })

-- -------------------------------------------------------------------------
-- newFormOptionMenu
-- -------------------------------------------------------------------------

newFormOptionMenu :: (GUIValue a) => [a] -> Form a
newFormOptionMenu options =
   let
      enterForm container =
         do
            optionMenu <- newOptionMenu container options []
            return (EnteredForm {
               packAction = pack optionMenu [],
               getFormValue = (
                  do
                     val <- getValue optionMenu
                     return (hasValue val)
                  ),
               destroyAction = done
               })
   in
      Form enterForm


newFormOptionMenu2 :: (Eq a,GUIValue a) => [(a,b)] -> Form b
newFormOptionMenu2 options =
   let
      form1 = newFormOptionMenu (map fst options)
   in
      fmap
         (\ a0 -> case findJust
               (\ (a1,b1) -> if a1 == a0 then Just b1 else Nothing)
               options
            of
               Nothing -> error (
                  "SimpleForm.newFormOptionMenu2: HTk returned strange value")
               Just b -> b
            )
         form1

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

mapMakeFormEntry :: FormValue value2
   => (value1 -> value2) -> (value2 -> value1)
   -> (Frame -> value1 -> IO (EnteredForm value1))
mapMakeFormEntry toValue2 fromValue2 frame value1 =
   do
      enteredForm <- makeFormEntry frame (toValue2 value1)
      return (mapEnteredForm fromValue2 enteredForm)

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
   readFormString str = hasValue str

allSpaces :: String -> Bool
allSpaces = all isSpace

-- numbers
instance (Num a,Show a,Read a) => FormTextField a where
   makeFormString value = show value
   readFormString str = case reads str of
      [(value,rest)] | allSpaces rest -> hasValue value
      _ -> hasError (show str ++ " is not a number")

instance FormTextField value => FormTextFieldIO value where
   makeFormStringIO value = return (makeFormString value)
   readFormStringIO value = return (readFormString value)

-- -------------------------------------------------------------------------
-- Instance #1A - FormTextFieldIO's, where IO actions are allowed
-- -------------------------------------------------------------------------

class FormTextFieldIO value where
   makeFormStringIO :: value -> IO String
   readFormStringIO :: String -> IO (WithError value)

instance FormTextFieldIO value => FormValue value where
   makeFormEntry frame defaultVal =
      do
         defaultString <- makeFormStringIO defaultVal
         contentsVariable <- createTkVariable defaultString
         (entry :: Entry String) <- newEntry frame [variable contentsVariable]
         let
            getFormValue =
               do
                  (contents :: String) <- readTkVariable contentsVariable
                  readFormStringIO contents
         let
            enteredForm = EnteredForm {
               packAction = pack entry [Side AtRight,Fill X],
               getFormValue = getFormValue,
               destroyAction = done
               }
         return enteredForm

-- -------------------------------------------------------------------------
-- Instance #1B - A variation of the former, for a text field where the
-- characters are not displayed as typed in, but replaced by '.'
-- -------------------------------------------------------------------------

newtype Password value = Password value

instance FormTextFieldIO value => FormValue (Password value) where
   makeFormEntry frame (Password defaultVal) =
      do
         defaultString <- makeFormStringIO defaultVal
         contentsVariable <- createTkVariable defaultString
         (entry :: Entry String)
            <- newEntry frame [showText '.',variable contentsVariable]
         let
            getFormValue =
               do
                  (contents :: String) <- readTkVariable contentsVariable
                  valueWE <- readFormStringIO contents
                  return (mapWithError Password valueWE)
         let
            enteredForm = EnteredForm {
               packAction = pack entry [Side AtRight,Fill X],
               getFormValue = getFormValue,
               destroyAction = done
               }
         return enteredForm

-- -------------------------------------------------------------------------
-- Instance #2B.   Maybe something that's an instance of FormTextFieldIO
-- so corresponding to Maybe String or Maybe Number.
-- It is possible to nest FormTextFieldIO's Maybe(Maybe . . .) but this is
-- not recommended.
-- When reading a null string, this will be parsed as a value rather than
-- Nothing if possible; this happens for example with String.
-- -------------------------------------------------------------------------

instance FormTextFieldIO value => FormTextFieldIO (Maybe value) where
   makeFormStringIO Nothing = return ""
   makeFormStringIO (Just value) = makeFormStringIO value

   readFormStringIO "" =
      do
         null <- readFormStringIO ""
         return (case fromWithError null of
            Left _ -> hasValue Nothing
            Right x -> hasValue (Just x)
            )
   readFormStringIO str =
      do
         xWE <- readFormStringIO str
         return (mapWithError Just xWE)

-- -------------------------------------------------------------------------
-- Instance #2C - Radio Buttons
-- If "x" is an instance of "Show", "Bounded" and "Enum", "Radio x" will be an
-- instance of FormValue, and will display the buttons in order.
-- But if you don't like this define your own instances of Show or,
-- for pictures, HasConfigRadioButton.
--
-- Radio Int is _not_ recommended.
-- -------------------------------------------------------------------------

data Radio x = Radio x | NoRadio deriving (Typeable)
-- The NoRadio indicates that no radio button is selected.

class HasConfigRadioButton value where
   configRadioButton :: value -> Config (RadioButton Int)

-- instance Show value => HasConfigRadioButton value where
--    configRadioButton value = text (show value)

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
                     return (hasValue (toRValue valInt)),
               destroyAction = done
               }
         return enteredForm

-- We need elsewhere in the workbench a Binary instance for Radio
instance (Monad m,HasBinary x m) => HasBinary (Radio x) m where
   writeBin = mapWrite (\ radio -> case radio of
      Radio x -> Just x
      NoRadio -> Nothing
      )
   readBin = mapRead (\ xOpt -> case xOpt of
      Just x -> Radio x
      Nothing -> NoRadio
      )

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
                     return (hasValue bool)
                  ),
               destroyAction = done
               }
         return enteredForm

-- -------------------------------------------------------------------------
-- ()
-- -------------------------------------------------------------------------

instance FormValue () where
   makeFormEntry frame () =
      return (
         EnteredForm {
            packAction = done,
            getFormValue = return (hasValue ()),
            destroyAction = done
            }
         )


-- -------------------------------------------------------------------------
-- An editable text window as a form entry.
-- -------------------------------------------------------------------------

-- | An editable text window as a form entry
-- Useful config options:
--   (value String) to set initial contents
--   (height i), (width i) to set the height and width in characters.
--   (background s) to set the background colour to s.
editableTextForm :: [Config Editor] -> Form String
editableTextForm configs =
   Form (\ container ->
      do
         editorFrame <- newFrame container []

         editor <- newEditor editorFrame (configs ++ [wrap NoWrap])
         scrollBar1 <- newScrollBar editorFrame [orient Vertical]
         scrollBar2 <- newScrollBar container [orient Horizontal]

         editor # scrollbar Vertical scrollBar1
         editor # scrollbar Horizontal scrollBar2

         return (EnteredForm {
            packAction =
               (do
                  pack editor [Side AtRight]
                  pack scrollBar1 [Side AtRight,Fill Y,Expand On]
                  pack editorFrame []
                  pack scrollBar2 [Side AtTop,Fill X,Expand On]
               ),
            getFormValue = (
               do
                  value <- getValue editor
                  return (hasValue value)
               ),
            destroyAction = done
            })
     )


-- | Like 'editableTextForm' but no scrollbars are displayed.
editableTextForm0 :: [Config Editor] -> Form String
editableTextForm0 configs =
   Form (\ container ->
      do
         editorFrame <- newFrame container []

         editor <- newEditor editorFrame (configs ++ [wrap NoWrap])

         return (EnteredForm {
            packAction =
               (do
                  pack editor [Side AtRight]
                  pack editorFrame []
               ),
            getFormValue = (
               do
                  value <- getValue editor
                  return (hasValue value)
               ),
            destroyAction = done
            })
     )
