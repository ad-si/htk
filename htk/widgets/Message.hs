-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Message (

  Message,
  newMessage,

  aspect,
  getAspect

) where

import Core
import BaseClasses(Widget)
import Configuration
import Destructible
import Computation
import Synchronized
import Packer
import Tooltip


-- -----------------------------------------------------------------------
-- type Message 
-- -----------------------------------------------------------------------

newtype Message a = Message GUIOBJECT deriving Eq


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

newMessage :: (Container par, GUIValue a) => par ->
                                             [Config (Message a)] ->
                                             IO (Message a)
newMessage par ol =
  do
    w <- createWidget (toGUIObject par) MESSAGE
    configure (Message w) (aspect 150 : ol)


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

---
-- Internal.
instance GUIObject (Message a) where 
---
-- Internal.
  toGUIObject (Message w) = w
---
-- Internal.
  cname _ = "Message"

---
-- A separator can be destroyed.
instance Destroyable (Message a) where
  destroy   = destroy . toGUIObject

instance Widget (Message a)

instance HasBorder (Message a)

instance HasColour (Message a) where
  legalColourID = hasForeGroundColour

instance HasFont (Message a)

instance HasJustify (Message a)

instance HasSize (Message a) where
  height _ w = return w
  getHeight _ = return 1

instance GUIValue b => HasText (Message String) b where
  text t w   = cset w "text" t
  getText w  = cget w "text"

instance Synchronized (Message a) where
  synchronize = synchronize . toGUIObject

---
-- A message widget can have a tooltip.
instance HasTooltip (Message a)

---
-- An message widget has an anchor.
instance HasAnchor (Message a)


-- -----------------------------------------------------------------------
-- configuration options
-- -----------------------------------------------------------------------

aspect :: Int -> Config (Message a)
aspect i w = cset w "aspect" i

getAspect :: Message a -> IO Int
getAspect w = cget w "aspect"
