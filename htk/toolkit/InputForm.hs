{- #########################################################################

MODULE        : InputForm
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : InputForm Abstraction


   ######################################################################### -}


module InputForm (
        InputForm(..),
        newInputForm,

        InputField(..), 
        FormState(fFormValue),	

        EntryField,
        newEntryField,

        EnumField,
        newEnumField,

        TextField,
        newTextField,

        getFormValue,
	setFormValue
--        RecordField,
--        newRecordField,

--       undefinedFormValue

        ) 
where

import Core
import HTk
import Prompt
import Box
import Label
import DialogWin
import OptionMenu
--import LabelBox
import ScrollBox
import Editor
import Keyboard
--import Separator
import Space
import Debug(debug)
import ReferenceVariables
import MarkupText


-- --------------------------------------------------------------------------
-- Classes 
-- --------------------------------------------------------------------------           
class InputField f where
        selector :: GUIValue b => (a -> b) -> Config (f a b)
        modifier :: GUIValue b => (a -> b -> a) -> Config (f a b)
    
class Variable a b where
        setVar :: a -> b -> IO ()
	getVar :: a -> IO b
	withVar:: a -> (a -> b) -> IO a
	
-- --------------------------------------------------------------------------
-- InputForm Type 
-- --------------------------------------------------------------------------           
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
newInputForm :: Box -> Maybe a -> [Config (InputForm a)] -> IO (InputForm a)
newInputForm par val ol = do {
        em <- newRef (FormState val Nothing Nothing Nothing Nothing Nothing []);
        configure (InputForm par em) ol
}

-- --------------------------------------------------------------------------
-- InputForm Instances 
-- --------------------------------------------------------------------------           
instance Eq (InputForm a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (InputForm a) where 
        toGUIObject (InputForm b e) = toGUIObject b
        cname _ = "InputForm"

instance HasColour (InputForm a) where
        legalColourID _ "foreground" = True
        legalColourID _ "background" = True
        legalColourID _ _ = False
        setColour form@(InputForm b e) "background" c = synchronize form (do {
                configure b [bg c]; 
                setFormConfig (\fst -> fst{fFormBg = Just c}) form;
                })
        setColour form@(InputForm b e) "foreground" c = synchronize form (do {
                configure b [fg c]; 
                setFormConfig (\fst -> fst{fFormFg = Just c}) form;
                })
        setColour form _ _ = return form
        getColour form "background" = getFormConfig form fFormBg
        getColour form "foreground" = getFormConfig form fFormFg
        getColour _ _ = return cdefault

instance HasFont (InputForm a) where
        HTk.font f form@(InputForm b e) = synchronize form (
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

instance HasSize (InputForm a)

instance HasBorder (InputForm a)

instance Synchronized (InputForm a) where
        synchronize w = synchronize (toGUIObject w)
        
instance Variable (InputForm a) a where
        setVar form val = setFormValue form val
        getVar form  = getFormValue form
--        withVar w f = synchronize w (do {v <- getVar w; f v; return w}) 
--        updVar w f = synchronize w (do {
--                v <- getVar w;
--                (v',r) <- f v;
--                setVar w v';
--                return r
--                })

-- --------------------------------------------------------------------------
--  Auxiliary
-- --------------------------------------------------------------------------           
getFormValue :: InputForm a -> IO a
getFormValue form @ (InputForm b e) = synchronize form (do {
        fst <- getRef e;
        case fFormValue fst of
                Nothing -> raise undefinedFormValue
                (Just val) -> updValue (fRecordFields fst) val
        }) 
 where  updValue [] val = return val
        updValue (fei:fel) val = do {putStrLn("updating value");
	                             val' <- (fei # fUpdField) val;
				     updValue fel val'
				     }
 

setFormValue :: InputForm a -> a -> IO ()
setFormValue form @ (InputForm b e) val = synchronize form (do {
        fst <- getRef e;
        setRef e (fst{fFormValue = Just val});
        foreach (fRecordFields fst) (\fei -> (fSetField fei) val);
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
data GUIValue b => EntryField a b = EntryField (Entry b) (Label b) (Ref (FieldInf a))

--FIXME: parent is a parameter now
newEntryField :: GUIValue b => InputForm a -> [Config (EntryField a b)] -> IO (EntryField a b)
newEntryField form@(InputForm box field) confs = do {
        b <- newHBox box [];
	pack b [Expand On, Fill X];
        lbl <- newLabel b []; 
	pack lbl [Expand Off, Fill X];
        pr <- newEntry b [];
        pack pr [Fill X, Expand On];

        -- :: IO (Prompt ?), convert to what when we do getValue
        --pr <- newPrompt b [];
        --pr <- newEntry b [];
	--pack pr [Expand On, Fill X];
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
{-
instance HasColour (EntryField a b) where
        legalColourID _ _ = True
        setColour fe@(EntryField pr _) cid c = do {
                setColour (getPromptEntry pr) cid c; return fe}
        getColour (EntryField pr _) cid = getColour (getPromptEntry pr) cid
-}
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
        --withVar w f = synchronize w (do {v <- getVar w; f v}) 

instance InputField EntryField where
        selector f fe@(EntryField pr lbl pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) pr; done}
        modifier f fe@(EntryField pr lbl pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {
                        ans <- try (getVar fe);
                        case ans of
                                (Left e) -> do {txt <- getText lbl; newErrorWin [prose txt] []; raise illegalGUIValue}
                                (Right val) -> return (f r val) 
                        }


-- --------------------------------------------------------------------------
--  Text Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => TextField a b = 
        TextField (Editor b)    
                  (Ref (FieldInf a))

newTextField :: GUIValue b => InputForm a -> [Config (TextField a b)] -> IO (TextField a b)
newTextField form@(InputForm box field) confs = do {
        -- :: IO (Editor ?)
        tp <- newEditor box [bg "white"];
	pack tp [Expand On, Fill Both];
        pv <- newFieldInf 
                (\c -> do {done})
                (\c -> do {done})
                (\f -> do {done})
                (\c -> do {done})
                (\s -> do {state s tp; done});
        configure (TextField tp pv) confs;
	addNewField form tp pv;
        return (TextField tp pv)
}

instance Eq (TextField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (TextField a b) where 
        toGUIObject (TextField tp _) = toGUIObject tp
        cname _ = "TextField"

instance Synchronized (TextField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance HasColour (TextField a b) where
        legalColourID _ _ = True
        setColour fe@(TextField ed _) cid c = do {setColour ed cid c; return fe}
        getColour (TextField ed _) cid = getColour ed cid

instance HasBorder (TextField a b)

instance HasSize (TextField a b) where
        width w fe @ (TextField ed _) = do {width w ed; return fe}
        getWidth (TextField ed _)    = getWidth ed
        height h fe @ (TextField ed _) = do {height h ed; return fe}
        getHeight fe @ (TextField ed _)= getHeight ed

instance HasFont (TextField a b) where 
        HTk.font f fe@(TextField ed _) = do {HTk.font f ed; return fe}
        getFont (TextField ed _) = getFont ed

instance HasEnable (TextField a b) where 
        state v f@(TextField ed _) = do {state v ed; return f}
        getState (TextField ed _) = getState ed
	
instance GUIValue b => Variable (TextField a b) b where
        setVar fe @ (TextField tp _) t = do {value t tp; done}
        getVar (TextField tp _) = getValue tp
--        withVar w f = synchronize w (do {v <- getVar w; f v}) 

instance InputField TextField where
        selector f fe@(TextField tp pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) tp; done}
	modifier f fe@(TextField tp pv) = synchronize fe (do {
		setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {
			ans <- try (getVar fe);
			case ans of
			 Left err -> raise illegalGUIValue
			 Right val -> return (f r val)
                        }


-- --------------------------------------------------------------------------
--  Enumeration Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => EnumField a b = 
        EnumField (OptionMenu b) (Ref (FieldInf a))


newEnumField :: GUIValue b => InputForm a -> [b] -> [Config (EnumField a b)] -> IO (EnumField a b)
newEnumField form@(InputForm box field) choices confs = do {
        mn <- newOptionMenu box choices [];
	pack mn [Expand On, Fill Both];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) mn; done})
                (\c -> do {fg (toColour c) mn; done})
                (\f -> do {HTk.font (toFont f) mn; done})
                (\c -> do {cursor (toCursor c) mn; done})
                (\s -> do {state s mn; done});
        configure (EnumField mn pv) confs;
        addNewField form mn pv;	
	return (EnumField mn pv)
}

instance Eq (EnumField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EnumField a b) where 
        toGUIObject (EnumField mn pv) = toGUIObject mn
        cname _ = "EnumField"

instance Widget (EnumField a b) where
        cursor c fe@(EnumField mn _) = do {cursor c mn; return fe}
        getCursor (EnumField mn _) = getCursor mn 

instance HasColour (EnumField a b) where
        legalColourID _ _ = True
        setColour fe@(EnumField mn _) cid c = do {setColour mn cid c; return fe}
        getColour (EnumField mn _) cid = getColour mn cid

instance HasBorder (EnumField a b)

instance HasSize (EnumField a b)
        
instance HasFont (EnumField a b) where 
        HTk.font f fe@(EnumField mn _) = do {HTk.font f mn; return fe}
        getFont (EnumField mn _) = getFont mn

instance HasEnable (EnumField a b) where 
        state v f@(EnumField mn _) = do {state v mn; return f}
        getState (EnumField mn _) = getState mn

-- FIXME: label will come back
--instance GUIValue c => HasText (EnumField a b) c where
--        text v fe @ (EnumField mn lb pv) = do {text v lb; return fe} 
--        getText fe@(EnumField mn lb pv) = getText lb

instance Synchronized (EnumField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue b => Variable (EnumField a b) b where
        setVar fe@(EnumField mn pv) v = do {value v mn; done} 
        getVar fe@(EnumField mn pv) = getValue mn
--        withVar w f = synchronize w (do {v <- getVar w; f v}) 

instance InputField EnumField where
        selector f fe@(EnumField mn pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) mn; done}
        modifier f fe@(EnumField mn pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {val <- getValue mn;return (f r val)}



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
 putStrLn("selector set")


setReplacorCmd :: Field a -> (a -> IO a) -> IO ()
setReplacorCmd pv cmd = do
 changeRef pv (\fei -> fei{fUpdField = cmd})
 putStrLn("replacor set")


















{-
-- --------------------------------------------------------------------------
-- Classes 
-- --------------------------------------------------------------------------           
class InputField f where
        selector :: GUIValue b => (a -> b) -> Config (f a b)
        modifier :: GUIValue b => (a -> b -> a) -> Config (f a b)

class Variable a b where
        setVar  :: a -> b -> IO ()
	getVar  :: a -> IO a
	withVar :: a -> a -> IO ()
	updVar  :: a -> b -> IO (a)

-- --------------------------------------------------------------------------
-- InputForm Type 
-- --------------------------------------------------------------------------           
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
-- FIXME:
newInputForm :: Box -> [Config (InputForm a)] -> IO (InputForm a)
newInputForm par ol = do {
        em <- newRef (FormState Nothing Nothing Nothing Nothing Nothing Nothing []);
        configure (InputForm par em) ol
}


-- --------------------------------------------------------------------------
-- InputForm Instances 
-- --------------------------------------------------------------------------           
instance Eq (InputForm a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (InputForm a) where 
        toGUIObject (InputForm b e) = toGUIObject b
        cname _ = "InputForm"

instance Destructible (InputForm a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance HasColour (InputForm a) where
        legalColourID _ "foreground" = True
        legalColourID _ "background" = True
        legalColourID _ _ = False
        setColour form@(InputForm b e) "background" c = synchronize form (do {
                configure b [bg c]; 
                setFormConfig (\fst -> fst{fFormBg = Just c}) form;
                })
        setColour form@(InputForm b e) "foreground" c = synchronize form (do {
                configure b [fg c]; 
                setFormConfig (\fst -> fst{fFormFg = Just c}) form;
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

instance HasSize (InputForm a)

instance HasBorder (InputForm a)

instance Synchronized (InputForm a) where
        synchronize w = synchronize (toGUIObject w)
        
instance Variable (InputForm a) b where
        setVar form val = setFormValue form val
        getVar form  = getFormValue form
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

-- --------------------------------------------------------------------------
--  Auxiliary
-- --------------------------------------------------------------------------           
getFormValue :: InputForm a -> IO a
getFormValue form @ (InputForm b e) = synchronize form (do {
        fst <- getVar e;
        case fFormValue fst of
                Nothing -> raise undefinedFormValue
                (Just val) -> updValue (fRecordFields fst) val
        }) 
 where  updValue [] val = return val
        updValue (fei:fel) val = do {val' <- (fUpdField fei) val;updValue fel val'}
 

setFormValue :: InputForm a -> a -> IO ()
setFormValue form @ (InputForm b e) val = synchronize form (do {
        fst <- getVar e;
        setVar e (fst{fFormValue = Just val});
        foreach (fRecordFields fst) (\fei -> (fSetField fei) val);
        })

--FIXME: where do changeVar' and withVar' come from?
setFormConfig :: (FormState a -> FormState a) -> Config (InputForm a)
setFormConfig trans form@(InputForm b e) = do {
--        changeVar' e trans;
        fst <- getVar e;
        foreach (fRecordFields fst) (setDefaultAttrs fst);
        return form
        } 

getFormConfig :: GUIValue b => InputForm a -> (FormState a -> Maybe b) -> IO b
getFormConfig form@(InputForm b e) fetch = return cdefault
--do { 
--        mv <- withVar' e fetch;
--        case mv of
--                Nothing -> return cdefault
--                (Just c) -> return c
--        }


-- --------------------------------------------------------------------------
--  Misc. Fields  
-- --------------------------------------------------------------------------
--FIXME: parent is no longer used
--instance ParentWidget (InputForm a) Space where
--        parent form@(InputForm b em) w = synchronize form (do {
--                configure w [orient Horizontal, parent b] 
--                })

--FIXME: Separator does not exist
--instance ParentWidget (InputForm a) Separator where
--        parent form@(InputForm b em) w = synchronize form (do {
--                configure w [orient Horizontal, parent b] 
--                })



-- --------------------------------------------------------------------------
--  Entry Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => EntryField a b = EntryField (Prompt b) (Ref (FieldInf a))

--FIXME: parent is a parameter now
newEntryField :: GUIValue b => InputForm a -> [Config (EntryField a b)] -> IO (EntryField a b)
newEntryField (InputForm b field) confs = do {
        pr <- newPrompt b [];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) pr; done})
                (\c -> do {fg (toColour c) pr; done})
                (\f -> do {font (toFont f) pr; done})
                (\c -> do {cursor (toCursor c) pr; done})
                (\s -> do {state s pr; done});
        configure (EntryField pr pv) confs
        addNewField (InputForm b field) pr pv;
}

instance Eq (EntryField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EntryField a b) where 
        toGUIObject (EntryField pr _) = toGUIObject pr
        cname _ = "EntryField"

--FIXME: im out
--instance Interactive (EntryField a b) 

instance Widget (EntryField a b) where
        cursor c fe@(EntryField pr _) = do {cursor c pr; return fe}
        getCursor (EntryField pr _) = getCursor pr 

instance HasColour (EntryField a b) where
        legalColourID _ _ = True
        setColour fe@(EntryField pr _) cid c = do {
                setColour (getPromptEntry pr) cid c; return fe}
        getColour (EntryField pr _) cid = getColour (getPromptEntry pr) cid

instance HasBorder (EntryField a b)

instance HasSize (EntryField a b)  where
        width w fe @ (EntryField pr _)  = do {width w pr; return fe}
        getWidth (EntryField pr _)      = getWidth pr
        height h fe @ (EntryField pr _) = do {height h pr; return fe}
        getHeight fe @ (EntryField pr _)= getHeight pr

        
instance HasFont (EntryField a b) 

instance HasEnable (EntryField a b) where 
        state v f@(EntryField pr _) = do {state v pr; return f}
        getState (EntryField pr _) = getState pr

instance (GUIValue b,GUIValue c) => HasText (EntryField a b) c where
        text v f@(EntryField pr _) = do {text v pr; return f}
        getText (EntryField pr _) = getText pr


instance Synchronized (EntryField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue a => Variable (EntryField a b) a where
        setVar f@(EntryField pr _) v = setVar pr v
        getVar (EntryField pr _) = getVar pr
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })
--FIXME: take a good look at this
instance InputField EntryField where
        selector f fe@(EntryField _ pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) fe; done}
        modifier f fe@(EntryField _ pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {
                        ans <- try (getValue fe);
                        case ans of
                                (Left e) -> do {
                                        nm <- getText fe;
                                        newErrorWin (nm ++ " illegal field value") [];
                                        raise illegalGUIValue
                                        }
                                (Right val) -> return (f r val) 
                        }
-}
{-
-- --------------------------------------------------------------------------
--  Record Fields  
-- --------------------------------------------------------------------------           
data RecordField a b = 
        RecordField (InputForm b) (LabelBox (InputForm b)) (PVar (FieldInf a))

newRecordField :: InputForm b -> [Config (RecordField a b)] -> IO (RecordField a b)
newRecordField cf confs = do {
        lb <- newLabelBox cf [orient Horizontal, fill Horizontal];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) cf; bg (toColour c) lb; done})
                (\c -> do {fg (toColour c) cf; fg (toColour c) lb; done})
                (\f -> do {font (toFont f) cf; font (toFont f) lb; done})
                (\c -> do {cursor (toCursor c) cf; cursor (toCursor c) lb;  done})
                (\s -> do {state s cf; done});
        configure (RecordField cf lb pv) confs
}

instance Eq (RecordField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (RecordField a b) where 
        toGUIObject (RecordField form lb pv) = toGUIObject lb
        cname _ = "RecordField"

instance Interactive (RecordField a b) 

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
                font f cf; 
                font f lb; 
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

instance GUIValue a => Variable (RecordField b) a where
        setVar fe @ (RecordField cf lb pv) v = setVar cf v 
        getVar fe@(RecordField cf lb pv) = getVar cf
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

instance InputField RecordField where
        selector f fe@(RecordField cf lb pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {setFormValue cf (f r); done}
        modifier f fe@(RecordField cf lb pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {val <- getFormValue cf;return (f r val)}

instance ParentWidget (InputForm a) (RecordField a b) where
        parent form fe@(RecordField cf lb pv) = synchronize form (do {
                addNewField form lb pv;
                setFieldValue form pv;
                return fe
                })



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

setFieldValue :: InputForm a -> Field a -> IO ()
setFieldValue (InputForm _ em) pv = do {
        fei <- getVar pv;
        withVar em (\fst -> 
                case fFormValue fst of
                        Nothing -> done
                        (Just val) -> (fSetField fei) val
                )
        }

addNewField :: InputForm a -> w -> Field a -> IO ()
addNewField form@(InputForm b em) w pv = do {
        fei <- getVar pv;
        fst <- getVar em;
        setDefaultAttrs fst fei; 
        configure w []; 
        --changeVar' em (\fst -> fst {fRecordFields = (fRecordFields fst) ++ [fei]})
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
setSelectorCmd pv cmd = return () --changeVar' pv (\fei -> fei{fSetField = cmd})


setReplacorCmd :: Field a -> (a -> IO a) -> IO ()
setReplacorCmd pv cmd = return () --changeVar' pv (\fei -> fei{fUpdField = cmd})



-- --------------------------------------------------------------------------
--  Exceptions
-- --------------------------------------------------------------------------           
undefinedFormValue :: IOError
undefinedFormValue = userError "form value is not defined"
-}
