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
        InputForm,
        newInputForm,

        InputField(..), 

        EntryField,
        newEntryField,

--        EnumField,
--        newEnumField,

--        TextField,
--        newTextField,

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


-- --------------------------------------------------------------------------
-- Classes 
-- --------------------------------------------------------------------------           
class InputField f where
        selector :: GUIValue b => (a -> b) -> Config (f a b)
        modifier :: GUIValue b => (a -> b -> a) -> Config (f a b)
    
class Variable a b where
        setVar :: a -> a -> IO ()
	getVar :: a -> IO a
	withVar:: a -> (a -> b) -> IO b

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

instance HasSize (InputForm a)

instance HasBorder (InputForm a)



-- --------------------------------------------------------------------------
--  Entry Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => EntryField a b = EntryField (Prompt b) (Ref (FieldInf a))

newEntryField :: GUIValue b => InputForm a -> [Config (EntryField a b)] -> IO (EntryField a b)
newEntryField form@(InputForm box field) confs = do {
        pr <- newPrompt box [];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) pr; done})
                (\c -> do {fg (toColour c) pr; done})
                (\f -> do {font (toFont f) pr; done})
                (\c -> do {cursor (toCursor c) pr; done})
                (\s -> do {state s pr; done});
        addNewField form pr pv;
        configure (EntryField pr pv) confs;
}

instance Eq (EntryField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EntryField a b) where 
        toGUIObject (EntryField pr _) = toGUIObject pr
        cname _ = "EntryField"

instance Synchronized (EntryField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance (GUIValue b,GUIValue c) => HasText (EntryField a b) c where
        text v f@(EntryField pr _) = do {text v pr; return f}
        getText (EntryField pr _) = getText pr

--instance GUIValue a => Variable (EntryField a b) a where
--        setVar fe@(EntryField pr pv) t = setVar pr t
--        getVar (EntryField pr pv) = getVar pr
--        withVar w f = synchronize w (do {v <- getVar w; f v}) 
--        updRef w f = synchronize w (do {
--                v <- getRef w;
--                (v',r) <- f v;
--                setRef w v';
--                return r
--                })

--instance InputField EntryField where
--        selector f fe@(EntryField _ pv) = synchronize fe (do {
--                setSelectorCmd pv cmd;
--                return fe
--                }) where cmd r = do {value (f r) fe; done}
--        modifier f fe@(EntryField _ pv) = synchronize fe (do {
--                setReplacorCmd pv cmd;
--                return fe
--                }) where cmd r = do {
--                        ans <- try (getValue fe);
--                        case ans of
--                                (Left e) -> do {
--                                        nm <- getText fe;
--                                        newErrorWin (nm ++ " illegal field value") [];
--                                        raise illegalGUIValue
--                                        }
--                                (Right val) -> return (f r val) 
--                        }


-- --------------------------------------------------------------------------
--  Text Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => TextField a b = 
        TextField (Editor b)    
                  (Ref (FieldInf a))

newTextField :: GUIValue b => InputForm a -> [Config (TextField a b)] -> IO (TextField a b)
newTextField form@(InputForm box field) confs = do {
        tp <- newEditor box [bg "white"];
	pack tp [Expand On, Fill Both];
        pv <- newFieldInf 
                (\c -> do {done})
                (\c -> do {done})
                (\f -> do {done})
                (\c -> do {done})
                (\s -> do {state s tp; done});
        configure (TextField tp pv) confs
}

instance Eq (TextField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (TextField a b) where 
        toGUIObject (TextField tp _) = toGUIObject tp
        cname _ = "TextField"

instance Synchronized (TextField a b) where
        synchronize fe = synchronize (toGUIObject fe)

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
setSelectorCmd pv cmd = changeRef pv (\fei -> fei{fSetField = cmd})


setReplacorCmd :: Field a -> (a -> IO a) -> IO ()
setReplacorCmd pv cmd = changeRef pv (\fei -> fei{fUpdField = cmd})


















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
--FIXME: parentwidget? (synchronize!!)
--instance ParentWidget (InputForm a) (EntryField a b) where
--        parent form fe@(EntryField pr pv) = synchronize form (do {
--                addNewField form pr pv;
--                setFieldValue form pv;
--                return fe
--                })

{-
-- --------------------------------------------------------------------------
--  Enumeration Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => EnumField a b = 
        EnumField (OptionMenu b) (LabelBox (OptionMenu b)) (PVar (FieldInf a))


newEnumField :: GUIValue b => [b] -> [Config (EnumField a b)] -> IO (EnumField a b)
newEnumField choices confs = do {
        mn <- newOptionMenu choices [fill Horizontal];
        lb <- newLabelBox mn [orient Vertical, fill Horizontal];
        pv <- newFieldInf
                (\c -> do {bg (toColour c) lb; done})
                (\c -> do {fg (toColour c) lb; done})
                (\f -> do {font (toFont f) lb; done})
                (\c -> do {cursor (toCursor c) lb; done})
                (\s -> do {state s mn; done});
        configure (EnumField mn lb pv) confs
}

instance Eq (EnumField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (EnumField a b) where 
        toGUIObject (EnumField mn lb pv) = toGUIObject lb
        cname _ = "EnumField"

instance Interactive (EnumField a b) 

instance Widget (EnumField a b) where
        cursor c fe@(EnumField mn lb _) = do {cursor c lb; return fe}
        getCursor (EnumField mn lb _) = getCursor lb 

instance HasColour (EnumField a b) where
        legalColourID _ _ = True
        setColour fe@(EnumField mn _ _) cid c = do {setColour mn cid c; return fe}
        getColour (EnumField mn _ _) cid = getColour mn cid

instance HasBorder (EnumField a b)

instance HasSize (EnumField a b)
        
instance HasFont (EnumField a b) where 
        font f fe@(EnumField mn _ _) = do {font f mn; return fe}
        getFont (EnumField mn _ _) = getFont mn

instance HasEnable (EnumField a b) where 
        state v f@(EnumField mn _ _) = do {state v mn; return f}
        getState (EnumField mn _ _) = getState mn

instance GUIValue c => HasText (EnumField a b) c where
        text v fe @ (EnumField mn lb pv) = do {text v lb; return fe} 
        getText fe@(EnumField mn lb pv) = getText lb

instance Synchronized (EnumField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue a => Variable (EnumField b) a where
        setVar fe @ (EnumField mn lb pv) v = do {value v mn; done} 
        getVar fe@(EnumField mn lb pv) = getVar mn
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

instance InputField EnumField where
        selector f fe@(EnumField mn lb pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) fe; done}
        modifier f fe@(EnumField mn lb pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {val <- getValue fe;return (f r val)}

instance ParentWidget (InputForm a) (EnumField a b) where
        parent form fe@(EnumField mn lb pv) = synchronize form (do {
                addNewField form lb pv;
                setFieldValue form pv;
                return fe
                })



-- --------------------------------------------------------------------------
--  Text Fields  
-- --------------------------------------------------------------------------           
data GUIValue b => TextField a b = 
        TextField (Editor b)    
                  (LabelBox(ScrollBox(Editor b)))
                  (PVar (FieldInf a))


newTextField :: GUIValue b => [Config (TextField a b)] -> IO (TextField a b)
newTextField confs = do {
        tp <- newEditor [bg "white", flexible];
        sb <- newScrollBox tp [flexible];
        lb <- newLabelBox sb [orient Vertical, flexible];
        pv <- newFieldInf 
                (\c -> do {bg (toColour c) lb; bg (toColour c) sb;done})
                (\c -> do {fg (toColour c) lb; done})
                (\f -> do {font (toFont f) lb; done})
                (\c -> do {cursor (toCursor c) lb; cursor (toCursor c) sb;done})
                (\s -> do {state s tp; done});
        configure (TextField tp lb pv) confs
}


instance Eq (TextField a b) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (TextField a b) where 
        toGUIObject (TextField tp lb _) = toGUIObject lb
        cname _ = "TextField"

instance Interactive (TextField a b) 

instance Widget (TextField a b) where
        cursor c fe@(TextField tp lb _) = do {cursor c lb; return fe}
        getCursor (TextField tp lb _) = getCursor lb 

instance HasColour (TextField a b) where
        legalColourID _ _ = True
        setColour fe@(TextField tp _ _) cid c = do {setColour tp cid c; return fe}
        getColour (TextField tp _ _) cid = getColour tp cid

instance HasBorder (TextField a b)

instance HasSize (TextField a b) where
        width w fe @ (TextField tp lb _) = do {width w tp; return fe}
        getWidth (TextField tp lb _)    = getWidth tp
        height h fe @ (TextField tp lb _) = do {height h tp; return fe}
        getHeight fe @ (TextField tp lb _)= getHeight tp

instance HasFont (TextField a b) where 
        font f fe@(TextField tp _ _) = do {font f tp; return fe}
        getFont (TextField tp _ _) = getFont tp

instance HasEnable (TextField a b) where 
        state v f@(TextField tp _ _) = do {state v tp; return f}
        getState (TextField tp _ _) = getState tp

instance (GUIValue b, GUIValue c) => HasText (TextField a b) c where
        text t fe @ (TextField tp lb _) = do {text t lb; return fe}
        getText (TextField tp lb _)     = getText lb

instance Synchronized (TextField a b) where
        synchronize fe = synchronize (toGUIObject fe)

instance GUIValue a => Variable (TextField b) a where
        setVar fe @ (TextField tp lb _) t = setVar tp t
        getVar (TextField tp lb _) = getVar tp
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })

instance InputField TextField where
        selector f fe@(TextField tp lb pv) = synchronize fe (do {
                setSelectorCmd pv cmd;
                return fe
                }) where cmd r = do {value (f r) fe; done}
        modifier f fe@(TextField tp lb pv) = synchronize fe (do {
                setReplacorCmd pv cmd;
                return fe
                }) where cmd r = do {
                        ans <- try (getValue fe);
                        case ans of
                                (Left e) -> do {
                                        nm <- getText fe;
                                        newErrorWin (nm ++ " illegal text field value") [];
                                        raise illegalGUIValue
                                        }
                                (Right val) -> return (f r val) 
                        }

instance ParentWidget (InputForm a) (TextField a b) where
        parent form fe@(TextField tp lb pv) = synchronize form (do {
                addNewField form lb pv;
                setFieldValue form pv;
                return fe
                })



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
