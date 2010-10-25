{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | the inputform
module HTk.Toolkit.InputForm (
        InputForm(..),
        newInputForm,

        InputField(..),
        FormState(fFormValue),

        EntryField,
        newEntryField,

        NumEntryField,
        newNumEntryField,

        CheckboxField,
        newCheckboxField,

        EnumField,
        newEnumField,

        TextField,
        newTextField,

        getFormValue,
        setFormValue,

        RecordField,
        newRecordField,

       undefinedFormValue

        )
where

import Control.Exception

import Util.Messages
import HTk.Kernel.Core
import qualified HTk.Toplevel.HTk as HTk (font)
import HTk.Toplevel.HTk hiding (font)
import HTk.Toolkit.SpinButton
import HTk.Toolkit.ScrollBox
import Reactor.ReferenceVariables

-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------
class InputField f where
        selector :: GUIValue b => (a -> b) -> Config (f a b)
        modifier :: GUIValue b => (a -> b -> a) -> Config (f a b)

class Variable a b where
        setVar :: a -> b -> IO ()
        getVar :: a -> IO b

-- --------------------------------------------------------------------------
-- InputForm Type
-- --------------------------------------------------------------------------
-- | The @InputForm@ datatype.
data InputForm a = InputForm Box (Ref (FormState a))

data FormState a = FormState {
        fFormValue      :: Maybe a,
        fFormBg         :: Maybe Colour,
        fFormFg         :: Maybe Colour,
        fFormFont       :: Maybe Font,
        fFormCursor     :: Maybe Cursor,
        fFormState      :: Maybe State,
        fRecordFields   :: [FieldInf a]
        }

data FieldInf a  = FieldInf {
        fSetField       :: a -> IO (),
        fUpdField       :: a -> IO a,
        fSetBgColour    :: Colour -> IO (),
        fSetFgColour    :: Colour -> IO (),
        fSetFont        :: Font -> IO (),
        fSetCursor      :: Cursor -> IO (),
        fSetState       :: State -> IO ()
        }

-- --------------------------------------------------------------------------
-- Commands
-- --------------------------------------------------------------------------
-- | Creates a new @InputForm@
newInputForm :: Box
   -- ^ parent container in which the form is embedded
   -> Maybe a
   -- ^ the datatype which contains the initial field values and the results
   -> [Config (InputForm a)]
   -- ^ list of configuration options for this form
   -> IO (InputForm a)
   -- ^ a @InputForm@
newInputForm par val ol = do {
        em <- newRef (FormState val Nothing Nothing Nothing Nothing Nothing []);
        configure (InputForm par em) ol
}

-- --------------------------------------------------------------------------
-- InputForm Instances
-- --------------------------------------------------------------------------
-- | Internal.
instance Eq (InputForm a) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject (InputForm a) where
        toGUIObject (InputForm b e) = toGUIObject b
        cname _ = "InputForm"


instance HasColour (InputForm a) where
        legalColourID _ "foreground" = True
        legalColourID _ "background" = True
        legalColourID _ _ = False
        setColour form@(InputForm b e) "background" c = synchronize form (do
               {
                configure b [bg c];
                setFormConfig (\fst -> fst{fFormBg = Just c}) form
               })
        setColour form@(InputForm b e) "foreground" c = synchronize form (do {
                configure b [fg c];
                setFormConfig (\fst -> fst{fFormFg = Just c}) form
                })
        setColour form _ _ = return form
        getColour form "background" = getFormConfig form fFormBg
        getColour form "foreground" = getFormConfig form fFormFg
        getColour _ _ = return cdefault

instance HasFont (InputForm a) where
        font f form@(InputForm b e) = synchronize form (
                setFormConfig (\fst -> fst{fFormFont = Just (toFont f)}) form
                )
        getFont form    = getFormConfig form fFormFont

instance HasEnable (InputForm a) where
        state s form@(InputForm b e) = synchronize form (
                setFormConfig (\fst -> fst{fFormState = Just s}) form
                )
        getState form   = getFormConfig form fFormState

instance Widget (InputForm a) where
        cursor c form@(InputForm b e) = synchronize form ( do {
                configure b [cursor c];
                setFormConfig (\fst -> fst{fFormCursor = Just (toCursor c)}) form
                })
        getCursor form  = getFormConfig form fFormCursor

instance Container (InputForm a)

instance HasSize (InputForm a)

instance HasBorder (InputForm a)

instance Synchronized (InputForm a) where
        synchronize w = synchronize (toGUIObject w)

instance Variable (InputForm a) a where
        setVar form val = setFormValue form val
        getVar form  = getFormValue form

-- --------------------------------------------------------------------------
--  Auxiliary
-- --------------------------------------------------------------------------
getFormValue :: InputForm a -> IO a
getFormValue form@(InputForm b e) = synchronize form (do {
        fst <- getRef e;
        case fFormValue fst of
                Nothing -> raise undefinedFormValue
                (Just val) -> updValue (fRecordFields fst) val
        })
 where  updValue [] val = return val
        updValue (fei:fel) val = do {
                                     val' <- (fei # fUpdField) val;
                                     updValue fel val'
                                     }


setFormValue :: InputForm a -> a -> IO ()
setFormValue form @ (InputForm b e) val = synchronize form (do {
        fst <- getRef e;
        setRef e (fst{fFormValue = Just val});
        foreach (fRecordFields fst) (\fei -> (fSetField fei) val)
        })

setFormConfig :: (FormState a -> FormState a) -> Config (InputForm a)
setFormConfig trans form@(InputForm b e) = do {
        changeRef e trans;
        fst <- getRef e;
        foreach (fRecordFields fst) (setDefaultAttrs fst);
        return form
        }

getFormConfig :: GUIValue b => InputForm a -> (FormState a -> Maybe b) -> IO b
getFormConfig form@(InputForm b e) fetch = do {
        mv <- withRef e fetch;
        case mv of
                Nothing -> return cdefault
                (Just c) -> return c
        }

-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------
undefinedFormValue :: IOError
undefinedFormValue = userError "form value is not defined"


-- --------------------------------------------------------------------------
--  Entry Fields
-- --------------------------------------------------------------------------
-- | The @EntryField@ datatype.
data EntryField a b = EntryField (Entry b) Label (Ref (FieldInf a))

-- | Add a new @EntryField@ to the form
newEntryField :: GUIValue b => InputForm a
   -- ^ the form to which the field is added
   -> [Config (EntryField a b)]
   -- ^ a list of configuration options for this field
   -> IO (EntryField a b)
   -- ^ a @EntryField@
newEntryField form@(InputForm box field) confs = do {
        b <- newHBox box [];
        pack b [Expand On, Fill X];
        lbl <- newLabel b [];
        pack lbl [Expand Off, Fill X];
        pr <- newEntry b [];
        pack pr [Fill X, Expand On];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) pr; done})
                (\c -> do {fg (toColour c) pr; done})
                (\f -> do {HTk.font (toFont f) pr; done})
                (\c -> do {cursor (toCursor c) pr; done})
                (\s -> do {state s pr; done});
        configure (EntryField pr lbl pv) confs;
        addNewField form pr pv;
        return (EntryField pr lbl pv)
    }

instance Eq (EntryField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EntryField a b) where
        toGUIObject (EntryField pr _ _) = toGUIObject pr
        cname _ = "EntryField"

instance Widget (EntryField a b) where
        cursor c fe@(EntryField pr _ _) = do {cursor c pr; return fe}
        getCursor (EntryField pr _ _) = getCursor pr

instance HasColour (EntryField a b) where
        legalColourID _ _ = True
        setColour fe@(EntryField pr lbl _) cid c = do {
                setColour pr cid c; setColour lbl cid c; return fe}
        getColour (EntryField pr _ _) cid = getColour pr cid

instance HasBorder (EntryField a b)

instance HasSize (EntryField a b)  where
        width w fe @ (EntryField pr _ _)  = do {width w pr; return fe}
        getWidth (EntryField pr _ _)      = getWidth pr
        height h fe @ (EntryField pr _ _) = do {height h pr; return fe}
        getHeight fe @ (EntryField pr _ _)= getHeight pr

instance HasFont (EntryField a b)

instance HasEnable (EntryField a b) where
        state v f@(EntryField pr _ _) = do {state v pr; return f}
        getState (EntryField pr _ _) = getState pr

instance (GUIValue b,GUIValue c) => HasText (EntryField a b) c where
        text v f@(EntryField pr lbl _) = do {text v lbl; return f}
        getText (EntryField pr lbl _) = getText lbl

instance Synchronized (EntryField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b => Variable (EntryField a b) b where
        setVar f@(EntryField pr _ _) val = do {value val pr; done}
        getVar (EntryField pr _ _) = getValue pr

instance InputField EntryField where
        selector f fe@(EntryField pr lbl pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) pr; done}
        modifier f fe@(EntryField pr lbl pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do
                          ans <- try (getVar fe)
                          case ans of
                                  Left (e :: SomeException) -> do
                                          txt <- getText lbl
                                          errorMess (txt++" legal field value")
                                          raise illegalGUIValue
                                  Right val -> return (f r val)

-- --------------------------------------------------------------------------
--  Numeric Entry Fields
-- --------------------------------------------------------------------------
-- | The @NumEntryField@ datatype.
data NumEntryField a b = NumEntryField (Entry b) Label SpinButton
                                       (Ref (FieldInf a))

-- | Add a new @NumEntryField@ to the form
newNumEntryField :: (Ord b, Num b, GUIValue b) => InputForm a
   -- ^ the form to which the field is added
   -> (b, b)
   -- ^ upper and lower bound (for the spin only)
   -> b
   -- ^ increment\/decrement for the spin button
   -> [Config (NumEntryField a b)]
   -- ^ a list of configuration options for this field
   -> IO (NumEntryField a b)
   -- ^ a @NumEntryField@
newNumEntryField form@(InputForm box field) (min, max) delta confs =
     do let spin Up v   = if v+ delta <= max then v+delta else v
            spin Down v = if v- delta >= min then v-delta else v
        b <- newHBox box []
        pack b [Expand On, Fill X]
        lbl <- newLabel b []
        pack lbl [Expand Off, Fill X]
        pr <- newEntry b []
        pack pr [Fill X, Expand Off]
        sp <- newSpinButton b
          (\sp-> do
             tv <- try (getValue pr);
             case tv of
               Right v -> pr # value (spin sp v)
               Left (_ :: SomeException)  -> return pr) []
        pack sp [Expand Off]
        pv <- newFieldInf
                (\c -> do {bg (toColour c) pr; done})
                (\c -> do {fg (toColour c) pr; done})
                (\f -> do {HTk.font (toFont f) pr; done})
                (\c -> do {cursor (toCursor c) pr; done})
                (\s -> do {state s pr; done})
        configure (NumEntryField pr lbl sp pv) confs
        addNewField form pr pv
        return (NumEntryField pr lbl sp pv)

instance Eq (NumEntryField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (NumEntryField a b) where
        toGUIObject (NumEntryField pr _ _ _) = toGUIObject pr
        cname _ = "NumEntryField"

instance Widget (NumEntryField a b) where
        cursor c fe@(NumEntryField pr _ _ _) = do {cursor c pr; return fe}
        getCursor (NumEntryField pr _ _ _) = getCursor pr

instance HasColour (NumEntryField a b) where
        legalColourID _ _ = True
        setColour fe@(NumEntryField pr lbl sp _) cid c = do {
                setColour pr cid c; setColour lbl cid c; setColour sp cid c;
                return fe}
        getColour (NumEntryField pr _ _ _) cid = getColour pr cid

instance HasBorder (NumEntryField a b)

instance HasSize (NumEntryField a b)  where
        width w fe @ (NumEntryField pr _ _ _)  = do {width w pr; return fe}
        getWidth (NumEntryField pr _ _ _)      = getWidth pr
        height h fe @ (NumEntryField pr _ _ _) = do {height h pr; return fe}
        getHeight fe @ (NumEntryField pr _ _ _)= getHeight pr

instance HasFont (NumEntryField a b)

instance HasEnable (NumEntryField a b) where
        state v f@(NumEntryField pr _ sp _) = do {state v pr; state v sp; return f}
        getState (NumEntryField pr _ _ _) = getState pr

instance (GUIValue b,GUIValue c) => HasText (NumEntryField a b) c where
        text v f@(NumEntryField pr lbl _ _) = do {text v lbl; return f}
        getText (NumEntryField pr lbl _ _) = getText lbl

instance Synchronized (NumEntryField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b => Variable (NumEntryField a b) b where
        setVar f@(NumEntryField pr _ _ _) val = do {value val pr; done}
        getVar (NumEntryField pr _ _ _) = getValue pr

instance InputField NumEntryField where
        selector f fe@(NumEntryField pr lbl _ pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) pr; done}
        modifier f fe@(NumEntryField pr lbl _ pv) =
                synchronize fe $ do {
                setReplacorCmd pv cmd;
                return fe
                } where cmd r = do
                          ans <- try (getVar fe)
                          case ans of
                                  Left (e :: SomeException) -> do
                                          txt <- getText lbl
                                          errorMess ("Illegal field value for "++ txt)
                                          raise illegalGUIValue
                                  Right val -> return (f r val) {- do
                                          num <- try ((readIO val) :: IO b)
                                          case num of
                                            Left _ -> do txt <- getText lbl
                                                         createErrorWin
                                                           ("Not a numeric " ++
                                                            "value for field "
                                                            ++ txt) []
                                            Right _ -> return (f r val) -}

-- --------------------------------------------------------------------------
--  Checkbox Fields
-- --------------------------------------------------------------------------
-- | The @CheckboxField@ datatype.
data CheckboxField a b = CheckboxField (CheckButton b) Label (TkVariable b) (Ref (FieldInf a))

-- | Add a new @CheckboxField@ to the form
newCheckboxField :: GUIValue b=> InputForm a
   -- ^ the form to which the field is added
   -> b
   -- ^ initial value
   -> [Config (CheckboxField a b)]
   -- ^ a list of configuration options for this field
   -> IO (CheckboxField a b)
   -- ^ a @CheckbuttonField@
newCheckboxField form@(InputForm box field) init confs = do {
        b <- newHBox box [];
        pack b [Expand On, Fill X];
        lbl <- newLabel b [];
        pack lbl [Expand Off, Fill X];
        cbvar <- createTkVariable init;
        pr <- newCheckButton b [variable cbvar];
        pack pr [Expand Off]; -- , Side AtRight];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) pr; done})
                (\c -> do {fg (toColour c) pr; done})
                (\f -> do {HTk.font (toFont f) pr; done})
                (\c -> do {cursor (toCursor c) pr; done})
                (\s -> do {state s pr; done});
        configure (CheckboxField pr lbl cbvar pv) confs;
        addNewField form pr pv;
        return (CheckboxField pr lbl cbvar pv)
    }

instance Eq (CheckboxField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (CheckboxField a b) where
        toGUIObject (CheckboxField pr _ _ _) = toGUIObject pr
        cname _ = "CheckboxField"

instance Widget (CheckboxField a b) where
        cursor c fe@(CheckboxField pr _ _ _) = do {cursor c pr; return fe}
        getCursor (CheckboxField pr _ _ _) = getCursor pr

instance HasColour (CheckboxField a b) where
        legalColourID _ _ = True
        setColour fe@(CheckboxField pr lbl _ _) cid c = do {
                setColour pr cid c; setColour lbl cid c; return fe}
        getColour (CheckboxField pr _ _ _) cid = getColour pr cid

instance HasBorder (CheckboxField a b)

instance HasSize (CheckboxField a b)  where
        width w fe @ (CheckboxField pr _ _ _)  = do {width w pr; return fe}
        getWidth (CheckboxField pr _ _ _)      = getWidth pr
        height h fe @ (CheckboxField pr _ _ _) = do {height h pr; return fe}
        getHeight fe @ (CheckboxField pr _ _ _)= getHeight pr

instance HasFont (CheckboxField a b)

instance HasEnable (CheckboxField a b) where
        state v f@(CheckboxField pr _ _ _) = do {state v pr; return f}
        getState (CheckboxField pr _ _ _) = getState pr

instance (GUIValue b, GUIValue c) => HasText (CheckboxField a b) c where
        text v f@(CheckboxField pr lbl _ _) = do {text v lbl; return f}
        getText (CheckboxField pr lbl _ _) = getText lbl

instance Synchronized (CheckboxField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b=> Variable (CheckboxField a b) b where
        setVar f@(CheckboxField pr _ cbv _) val = setTkVariable cbv val
        getVar (CheckboxField pr _ cbv _) = readTkVariable cbv

instance InputField CheckboxField where
        selector f fe@(CheckboxField pr lbl cbv pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {setTkVariable cbv (f r)}
        modifier f fe@(CheckboxField pr lbl cbv pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do
                          ans <- try (getVar fe)
                          case ans of
                                  Left (e :: SomeException) -> do
                                          txt <- getText lbl
                                          errorMess (txt++" legal field value")
                                          raise illegalGUIValue
                                  Right val -> return (f r val)

-- --------------------------------------------------------------------------
--  Text Fields
-- --------------------------------------------------------------------------
-- | The @TextField@ datatype.
data TextField a b = TextField Editor Label (Ref (FieldInf a))

-- | Add a new @TextField@ to the form
newTextField :: GUIValue b => InputForm a
   -- ^ the form to which the field is added
   -> [Config (TextField a b)]
   -- ^ a list of configuration options for this field
   -> IO (TextField a b)
   -- ^ a @TextField@
newTextField form@(InputForm box field) confs =
 do
  b <- newVBox box []
  pack b [Expand On, Fill Both, PadX (cm 0.1), PadY (cm 0.1)]
  lbl <- newLabel b [anchor West]
  pack lbl [Expand Off, Fill Both]
  let edit p = newEditor p []
  (sb, tp) <- newScrollBox b edit []
  pack sb [Expand On, Fill Both]
  pv <- newFieldInf
          (\c -> do {done})
          (\c -> do {done})
          (\f -> do {done})
          (\c -> do {done})
          (\s -> do {state s tp; done})
  configure (TextField tp lbl pv) confs
  addNewField form tp pv
  return (TextField tp lbl pv)


instance Eq (TextField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (TextField a b) where
        toGUIObject (TextField tp _ _) = toGUIObject tp
        cname _ = "TextField"

instance Synchronized (TextField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance HasColour (TextField a b) where
        legalColourID _ _ = True
        setColour fe@(TextField ed lbl _) cid c = do {setColour ed cid c; setColour ed cid c; return fe}
        getColour (TextField ed _ _) cid = getColour ed cid

instance HasBorder (TextField a b)

instance HasSize (TextField a b) where
        width w fe @ (TextField ed _ _) = do {width w ed; return fe}
        getWidth (TextField ed _ _)    = getWidth ed
        height h fe @ (TextField ed _ _) = do {height h ed; return fe}
        getHeight fe @ (TextField ed _ _)= getHeight ed

instance HasFont (TextField a b) where
        font f fe@(TextField ed _ _) = do {HTk.font f ed; return fe}
        getFont (TextField ed _ _) = getFont ed

instance HasEnable (TextField a b) where
        state v f@(TextField ed _ _) = do {state v ed; return f}
        getState (TextField ed _ _) = getState ed

instance (GUIValue b,GUIValue c) => HasText (TextField a b) c where
        text v f@(TextField pr lbl _) = do {text v lbl; return f}
        getText (TextField pr lbl _) = getText lbl

instance GUIValue b => Variable (TextField a b) b where
        setVar fe @ (TextField tp _ _) t = do {value t tp; done}
        getVar (TextField tp _ _) = getValue tp

instance InputField TextField where
        selector f fe@(TextField tp lbl pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) tp; done}
        modifier f fe@(TextField tp lbl pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do
                          ans <- try (getVar fe)
                          case ans of
                            Left (err :: SomeException) -> do
                                   txt <- getText lbl
                                   errorMess (txt++" legal field value")
                                   raise illegalGUIValue
                            Right val -> return (f r val)


-- --------------------------------------------------------------------------
--  Enumeration Fields
-- --------------------------------------------------------------------------
-- | The @EnumField@ datatype.
data EnumField a b = EnumField (OptionMenu b) Label (Ref (FieldInf a))

-- | Add a new @EnumField@ to the form
newEnumField :: GUIValue b => InputForm a
   -- ^ the form to which the field is added
   -> [b]
   -- ^ the list of choices in this field
   -> [Config (EnumField a b)]
   -- ^ a list of configuration options for this field
   -> IO (EnumField a b)
   -- ^ a @EnumField@
newEnumField form@(InputForm box field) choices confs =
 do
  b <- newHBox box []
  pack b [Expand On, Fill X, PadX (cm 0.1), PadY (cm 0.1)]
  lbl <- newLabel b []
  pack lbl [Expand Off, Fill Both]
  mn <- newOptionMenu b choices []
  pack mn [Expand Off, Fill Both]
  pv <- newFieldInf
          (\c -> do {bg (toColour c) mn; done})
          (\c -> do {fg (toColour c) mn; done})
          (\f -> do {HTk.font (toFont f) mn; done})
          (\c -> do {cursor (toCursor c) mn; done})
          (\s -> do {state s mn; done})
  configure (EnumField mn lbl pv) confs
  addNewField form mn pv
  return (EnumField mn lbl pv)


instance Eq (EnumField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EnumField a b) where
        toGUIObject (EnumField mn lbl pv) = toGUIObject mn
        cname _ = "EnumField"

instance Widget (EnumField a b) where
        cursor c fe@(EnumField mn _ _) = do {cursor c mn; return fe}
        getCursor (EnumField mn _ _) = getCursor mn

instance HasColour (EnumField a b) where
        legalColourID _ _ = True
        setColour fe@(EnumField mn lbl _) cid c = do {setColour mn cid c; setColour lbl cid c; return fe}
        getColour (EnumField mn lbl _) cid = getColour mn cid

instance HasBorder (EnumField a b)

instance HasSize (EnumField a b)

instance HasFont (EnumField a b) where
        font f fe@(EnumField mn _ _) = do {HTk.font f mn; return fe}
        getFont (EnumField mn _ _) = getFont mn

instance HasEnable (EnumField a b) where
        state v f@(EnumField mn _ _) = do {state v mn; return f}
        getState (EnumField mn _ _) = getState mn

instance GUIValue c => HasText (EnumField a b) c where
        text v fe @ (EnumField mn lbl pv) = do {text v lbl; return fe}
        getText fe@(EnumField mn lbl pv) = getText lbl

instance Synchronized (EnumField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b => Variable (EnumField a b) b where
        setVar fe@(EnumField mn lbl pv) v = do {value v mn; done}
        getVar fe@(EnumField mn lbl pv) = getValue mn

instance InputField EnumField where
        selector f fe@(EnumField mn lbl pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) mn; done}
        modifier f fe@(EnumField mn lbl pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {val <- getValue mn;return (f r val)}

-- --------------------------------------------------------------------------
--  Record Fields
-- --------------------------------------------------------------------------
data RecordField a b =
        RecordField (InputForm b) Label (Ref (FieldInf a))

newRecordField :: InputForm a -> (Box -> IO (InputForm b)) -> [Config (RecordField a b)] -> IO (RecordField a b, InputForm b)
newRecordField form@(InputForm box e) newform confs =
 do
  b <- newVBox box []
  pack b [Expand On, Fill Both, PadX (cm 0.1), PadY (cm 0.1)]
  lbl <- newLabel b []
  pack lbl [Expand Off, Fill X]
  cf <- newform b
  pv <- newFieldInf
          (\c -> do {bg (toColour c) cf; bg (toColour c) lbl; done})
          (\c -> do {fg (toColour c) cf; fg (toColour c) lbl; done})
          (\f -> do {HTk.font (toFont f) cf; HTk.font (toFont f) lbl; done})
          (\c -> do {cursor (toCursor c) cf; cursor (toCursor c) lbl;  done})
          (\s -> do {state s cf; done})
  configure (RecordField cf lbl pv) confs
  addNewField form cf pv
  return (RecordField cf lbl pv, cf)


instance Eq (RecordField a b) where
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (RecordField a b) where
        toGUIObject (RecordField form lb pv) = toGUIObject lb
        cname _ = "RecordField"

instance Widget (RecordField a b) where
        cursor c fe@(RecordField cf lb _) = synchronize fe (do {
                cursor c lb;
                cursor c cf;
                return fe
                })
        getCursor (RecordField mn lb _) = getCursor lb

instance HasColour (RecordField a b) where
        legalColourID _ _ = True
        setColour fe@(RecordField cf lb _) cid c = synchronize fe (do {
                setColour cf cid c;
                setColour lb cid c;
                return fe
                })
        getColour (RecordField cf _ _) cid = getColour cf cid

instance HasBorder (RecordField a b)

instance HasSize (RecordField a b)

instance HasFont (RecordField a b) where
        font f fe@(RecordField cf lb _) = synchronize fe (do {
                HTk.font f cf;
                HTk.font f lb;
                return fe
                })
        getFont (RecordField cf _ _) = getFont cf

instance HasEnable (RecordField a b) where
        state v fe@(RecordField cf _ _) = do {state v cf; return fe}
        getState (RecordField cf _ _) = getState cf

instance GUIValue c => HasText (RecordField a b) c where
        text v fe @ (RecordField cf lb pv) = do {text v lb; return fe}
        getText fe@(RecordField cf lb pv) = getText lb

instance Synchronized (RecordField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b => Variable (RecordField a b) b where
        setVar fe@(RecordField cf lb pv) v = setVar cf v
        getVar fe@(RecordField cf lb pv) = getVar cf

instance InputField RecordField where
        selector f fe@(RecordField cf lb pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {setFormValue cf (f r); done}
        modifier f fe@(RecordField cf lb pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {val <- getFormValue cf;return (f r val)}


-- --------------------------------------------------------------------------
--  Auxiliary Computations for Field Information
-- --------------------------------------------------------------------------
type Field a = (Ref (FieldInf a))

newFieldInf :: (Colour -> IO ())
        -> (Colour -> IO ())
        -> (Font -> IO ())
        -> (Cursor -> IO ())
        -> (State -> IO ())
        -> IO (Field a)
newFieldInf setBg setFg setFont setCursor setState = newRef inf
        where inf = FieldInf (const done) return setBg setFg setFont setCursor setState


addNewField :: InputForm a -> w -> Field a -> IO ()
addNewField form@(InputForm b em) w pv = do {
        fei <- getRef pv;
        fst <- getRef em;
        setDefaultAttrs fst fei;
        configure w [];
        changeRef em (\fst -> fst {fRecordFields = (fRecordFields fst) ++ [fei]})
        }

setDefaultAttrs :: FormState a -> FieldInf a -> IO ()
setDefaultAttrs fst fei = do {
        incase (fFormBg fst) (fSetBgColour fei);
        incase (fFormFg fst) (fSetFgColour fei);
        incase (fFormFont fst) (fSetFont fei);
        incase (fFormCursor fst) (fSetCursor fei);
        incase (fFormState fst) (fSetState fei);
        done
        }

setSelectorCmd :: Field a -> (a -> IO ()) -> IO ()
setSelectorCmd pv cmd = do
 changeRef pv (\fei -> fei{fSetField = cmd})


setReplacorCmd :: Field a -> (a -> IO a) -> IO ()
setReplacorCmd pv cmd = do
 changeRef pv (\fei -> fei{fUpdField = cmd})
