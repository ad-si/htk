-- | A spin button widget consisting of two button widgets.
module HTk.Toolkit.SpinButton (

  Spin(..),

  SpinButton,
  newSpinButton

)  where

import System.IO.Unsafe

import Util.Computation

import Events.Events
import Events.Channels
import Events.Synchronized

import HTk.Kernel.Core
import HTk.Toplevel.HTk



-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

-- | The @SpinButton@ datatype.
data SpinButton =
        SpinButton {
                fContainer :: Box,
                fButtonUp :: Button,
                fButtonDown :: Button,
                fDeath :: IO ()
        }

-- | The @Spin@ datatype.
data Spin = Down | Up deriving (Eq,Ord)


-- -----------------------------------------------------------------------
-- construction
-- -----------------------------------------------------------------------

-- | Constructs a new spin button and returns a handler.
newSpinButton :: Container par => par
   -- ^ the parent widget, which has to be a container widget.
   -> (Spin -> IO a)
   -- ^ the command to execute, when a button is pressed.
   ->
   [Config SpinButton]
   -- ^ the list of configuration options for this spin
   -- button.
   -> IO SpinButton
   -- ^ A spin button.
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
    _ <- spawnEvent listenButtons
    configure (SpinButton b bup bdown (syncNoWait (send death ())))
              cnf


-- -----------------------------------------------------------------------
-- SpinButton instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq SpinButton where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject SpinButton where
  toGUIObject sb = toGUIObject (fContainer sb)
  cname _ = "SpinButton"

-- | A spin button can be destroyed.
instance Destroyable SpinButton where
  -- Destroys a spin button.
  destroy sb = fDeath sb >> destroy (toGUIObject sb)

-- | A spin button has standard widget properties
-- (concerning focus, cursor).
instance Widget SpinButton

-- | You can synchronize on a spin button.
instance Synchronized SpinButton where
  -- Synchronizes on a spin button.
  synchronize = synchronize . toGUIObject

-- | A spin button has a normal foreground and background colour and an
-- active\/disabled foreground and background colour.
instance HasColour SpinButton where
  legalColourID _ _ = True
  setColour sb cid col =
    do
      setColour (fContainer sb) cid col
      setColour (fButtonUp sb) cid col
      setColour (fButtonDown sb) cid col
      return sb

-- | A spin button has a configureable border.
instance HasBorder SpinButton


-- | A spin button is a stateful widget, it can be enabled or disabled.
instance HasEnable SpinButton where
  -- Sets the spin button\'s state.
  state s sb =
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (state s)
                      return sb)
  -- Gets the spin button\'s state.
  getState sb = getState (fButtonUp sb)

-- | A spin button has a configureable font.
instance HasFont SpinButton where
  -- Sets the spin button\'s font.
  font f sb =
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (font f)
                      return sb)
  -- Gets the spin button\'s font.
  getFont sb = getFont (fButtonUp sb)

-- | A spin button has a configureable size.
instance HasSize SpinButton


-- -----------------------------------------------------------------------
-- The images
-- -----------------------------------------------------------------------

msDownButtonImg :: Image
msDownButtonImg =
  unsafePerformIO (newImage [imgData GIF
     "R0lGODdhCQAGAPAAAP///wAAACwAAAAACQAGAAACC4SPoRvHnRRys5oCADs="])
{-# NOINLINE msDownButtonImg #-}

msUpButtonImg :: Image
msUpButtonImg =
  unsafePerformIO (newImage [imgData GIF
     "R0lGODdhCQAGAPAAAP///wAAACwAAAAACQAGAAACC4SPF2nh6aKKkp0CADs"])
{-# NOINLINE msUpButtonImg #-}
