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

---
-- A spin button widget consisting of two button widgets.
module SpinButton (

  Spin(..),

  SpinButton,
  newSpinButton

)  where

import IOExts(unsafePerformIO)
import Core
import HTk



-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

---
-- The <code>SpinButton</code> datatype.
data SpinButton = 
        SpinButton {
                fContainer :: Box,
                fButtonUp :: Button,
                fButtonDown :: Button,
                fDeath :: IO ()
        }

---
-- The <code>Spin</code> datatype.
data Spin = Down | Up deriving (Eq,Ord)


-- -----------------------------------------------------------------------
-- construction 
-- -----------------------------------------------------------------------

---
-- Constructs a new spin button and returns a handler.
-- @param par     - the parent widget, which has to be a container widget.
-- @param cmd     - the command to execute, when a button is pressed.
-- @param cnf     - the list of configuration options for this spin
--                - button.
-- @return result - A spin button.
newSpinButton :: Container par => par -> (Spin -> IO a) ->
                                  [Config SpinButton] -> IO SpinButton
newSpinButton par cmd cnf =
  do
    b <- newVFBox par []
    bup <- newButton b [photo msUpButtonImg]
    clicked_bup <- clicked bup
    pack bup []
    bdown <- newButton b [photo msDownButtonImg]
    clicked_bdown <- clicked bdown
    pack bdown []
    death <- newChannel
    let listenButtons :: Event ()
        listenButtons = (clicked_bdown >> always (cmd Down) >>
                         listenButtons) +>
                        (clicked_bup >> always (cmd Up) >>
                         listenButtons) +>
                        receive death
    spawnEvent listenButtons
    configure (SpinButton b bup bdown (syncNoWait (send death ())))
              cnf

                
-- -----------------------------------------------------------------------
-- SpinButton instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance Eq SpinButton where 
---
-- Internal.
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

---
-- Internal.
instance GUIObject SpinButton where 
---
-- Internal.
  toGUIObject sb = toGUIObject (fContainer sb)
---
-- Internal.
  cname _ = "SpinButton"

---
-- A spin button can be destroyed.
instance Destroyable SpinButton where
---
-- Destroys a spin button.
  destroy sb = fDeath sb >> destroy (toGUIObject sb)

---
-- A spin button has standard widget properties
-- (concerning focus, cursor).
instance Widget SpinButton

---
-- You can synchronize on a spin button.
instance Synchronized SpinButton where
---
-- Synchronizes on a spin button.
  synchronize = synchronize . toGUIObject

---
-- A spin button has a normal foreground and background colour and an
-- active/disabled foreground and background colour.
instance HasColour SpinButton where 
  legalColourID _ _ = True
  setColour sb cid col =
    do
      setColour (fContainer sb) cid col
      setColour (fButtonUp sb) cid col
      setColour (fButtonDown sb) cid col
      return sb

---
-- A spin button has a configureable border.
instance HasBorder SpinButton


---
-- A spin button is a stateful widget, it can be enabled or disabled.
instance HasEnable SpinButton where 
---
-- Sets the spin button's state.
  state s sb = 
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (state s)
                      return sb)
---
-- Gets the spin button's state.
  getState sb = getState (fButtonUp sb)

---
-- A spin button has a configureable font.
instance HasFont SpinButton where
---
-- Sets the spin button's font.
  font f sb = 
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (font f)
                      return sb)
---
-- Gets the spin button's font.
  getFont sb = getFont (fButtonUp sb)

---
-- A spin button has a configureable size.
instance HasSize SpinButton


-- -----------------------------------------------------------------------
-- The images
-- -----------------------------------------------------------------------

msDownButtonImg :: Image
msDownButtonImg = 
  unsafePerformIO (newImage NONE [imgData# GIF 
     "R0lGODdhCQAGAPAAAP///wAAACwAAAAACQAGAAACC4SPoRvHnRRys5oCADs=\0"#])
{-# NOINLINE msDownButtonImg #-}

msUpButtonImg :: Image
msUpButtonImg = 
  unsafePerformIO (newImage NONE [imgData# GIF 
     "R0lGODdhCQAGAPAAAP///wAAACwAAAAACQAGAAACC4SPF2nh6aKKkp0CADs\0"#])
{-# NOINLINE msUpButtonImg #-}
