{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A simple prompt (a labelled entry field).
module HTk.Toolkit.Prompt (

  Prompt,
  newPrompt,

  getPromptEntry

) where

import Util.Computation

import Events.Synchronized

import HTk.Kernel.Core
import HTk.Toplevel.HTk

-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @Prompt@ datatype.
data Prompt a = Prompt Box Label (Entry a)


-- -----------------------------------------------------------------------
-- Commands
-- -----------------------------------------------------------------------

-- i had problems creating a TkVariable of any kind here?!?

-- | Construct a new prompt and returns a handler.
newPrompt :: GUIValue a => Box
   -- ^ the parent box.
   -> [Config (Prompt a)]
   -- ^ the list of configuration options for this prompt.
   -> IO (Prompt a)
   -- ^ A prompt.
newPrompt par cnf =  do {
        b <- newHBox par [];
        pack b [Expand On, Fill X];
        lbl <- newLabel b [];
        pack lbl [Expand Off, Fill X];
        ent <- newEntry b [];
        pack ent [Fill X, Expand On];
        configure (Prompt b lbl ent) cnf
}


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq (Prompt a) where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject (Prompt a) where
  toGUIObject (Prompt bx _ _) = toGUIObject bx
  cname _ = "Prompt"

-- | A prompt has standard widget properties
-- (concerning focus, cursor).
instance Widget (Prompt a) where
  -- Sets the mouse cursor for this prompt.
  cursor c pr @ (Prompt bx lbl ent) =
    synchronize pr (do
                      cursor c bx
                      cursor c lbl
                      cursor c ent
                      return pr)

-- | A prompt has a configureable border.
instance HasBorder (Prompt a)

{- not needed ?!?
instance HasSize (Prompt a) where
  height _ w  = return w
  getHeight _ = return 1
-}

-- | A prompt has a configureable foreground and background colour.
instance HasColour (Prompt a) where
  legalColourID _ _ = True
  setColour pr @ (Prompt bx lbl en_) cid c =
    synchronize pr (do
                      setColour bx cid c
                      setColour lbl cid c
                      return pr)

-- | A propt has a configureable font.
instance HasFont (Prompt a) where
  -- Sets the font of the prompt.
  font f pr @ (Prompt bx lbl ent) =
    synchronize pr (do
                      font f lbl
                      return pr)
  -- Gets the font of the prompt.
  getFont (Prompt bx lbl ent) = getFont lbl

-- | A prompt has a configureable text.
instance (GUIValue a, GUIValue b) => HasText (Prompt a) b where
  -- Sets the prompt\'s text.
  text t pr @ (Prompt _ lbl _) = do {text t lbl; return pr}
  -- Gets the prompt\'s text.
  getText (Prompt _ lbl _) = getText lbl

-- | A prompt is a stateful component, it can be enabled or disabled.
instance HasEnable (Prompt a) where
  -- Sets the prompt\'s state.
  state s pr @ (Prompt bx lbl ent) = do {state s ent; return pr}
  -- Gets the prompt\'s state.
  getState (Prompt bx lbl ent) = getState ent

-- | You can synchronize on a prompt object.
instance Synchronized (Prompt a) where
  -- Synchronizes on a prompt object.
  synchronize w = synchronize (toGUIObject w)

-- | A prompt widget has an (entered) value.
instance GUIValue a => HasValue (Prompt a) a where
  -- Sets the prompt\'s value.
  value val p@(Prompt bx lbl ent) = value val p
  -- Gets the prompt\'s value.
  getValue (Prompt bx lbl ent) = getValue ent


-- -----------------------------------------------------------------------
-- Entry Components
-- -----------------------------------------------------------------------

-- | Gets the entry field of the prompt.
getPromptEntry :: Prompt a
   -- ^ the concerned prompt.
   -> Entry a
   -- ^ the prompt\'s entry.
getPromptEntry pr@(Prompt _ _ ent) = ent
