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


module SpinButton (

  Spin(..),

  SpinButton,
  newSpinButton

)  where

import Core
import HTk
import Button
import BitMap
import Destructible

import WBFiles


-- -----------------------------------------------------------------------
-- datatype
-- -----------------------------------------------------------------------

data SpinButton = 
        SpinButton {
                fContainer :: Box,
                fButtonUp :: Button,
                fButtonDown :: Button,
                fDeath :: IO ()
        }

data Spin = Down | Up deriving (Eq,Ord)


-- -----------------------------------------------------------------------
-- construction 
-- -----------------------------------------------------------------------

newSpinButton :: Container par => par -> (Spin -> IO a) ->
                                  [Config SpinButton] -> IO SpinButton
newSpinButton par cmd cnf =
  do
    b <- newVFBox par []
    bup <- newButton b [htkbitmap "ms_up_arrow.bm"]
    clicked_bup <- clicked bup
    pack bup []
    bdown <- newButton b [htkbitmap "ms_down_arrow.bm"]
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

instance Eq SpinButton where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject SpinButton where 
  toGUIObject sb = toGUIObject (fContainer sb)
  cname _ = "SpinButton"

instance Destroyable SpinButton where
  destroy sb = fDeath sb >> destroy (toGUIObject sb)

instance Widget SpinButton

instance Synchronized SpinButton where
  synchronize = synchronize . toGUIObject

instance HasColour SpinButton where 
  legalColourID _ _ = True
  setColour sb cid col =
    do
      setColour (fContainer sb) cid col
      setColour (fButtonUp sb) cid col
      setColour (fButtonDown sb) cid col
      return sb

instance HasBorder SpinButton

instance HasEnable SpinButton where 
  state s sb = 
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (state s)
                      return sb)
  getState sb = getState (fButtonUp sb)

instance HasFont SpinButton where
  font f sb = 
    synchronize sb (do
                      foreach [fButtonUp sb, fButtonDown sb] (font f)
                      return sb)
  getFont sb = getFont (fButtonUp sb)

instance HasSize SpinButton


-- -----------------------------------------------------------------------
-- The bitmaps
-- -----------------------------------------------------------------------

htkbitmap :: HasBitMap w => String -> Config w
htkbitmap fnm w =
  do
    path <- getWBImageFilePath fnm
    configure w [bitmap path]
    return w
