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

module Icon (

  module Window,

  Icon(..),
  iconMask,
  getIconMask

) where

import Core
import BitMap
import Computation
import Synchronized
import Window
import Destructible


-- -----------------------------------------------------------------------
-- type icon's
-- -----------------------------------------------------------------------

data Window w => Icon w = Icon w deriving (Eq,Ord)


-- -----------------------------------------------------------------------
-- instantions 
-- -----------------------------------------------------------------------

instance Window w => GUIObject (Icon w) where
  toGUIObject (Icon win) = toGUIObject win
  cname _ = "Icon"
  cset (Icon win) cid val = cset win cid val >> return (Icon win)
  cget (Icon win) cid = cget win cid

instance Window w => Destroyable (Icon w) where
  destroy = destroy . toGUIObject

instance Window w => HasBitMap (Icon w) where
  bitmap s icon   = setBitMapHandle icon "iconbitmap" (toBitMap s) False
  getBitMap icon  = getBitMapHandle icon "iconbitmap"

instance (Window w, GUIValue v) => HasText (Icon w) v where
  text s icon  = cset icon "iconname" s
  getText icon = cget icon "iconname"

instance Window w => HasPosition (Icon w) where
  position p icon = cset icon "iconposition" p
  getPosition icon = cget icon "iconposition"

instance Window w => Synchronized (Icon w) where
  synchronize w = synchronize (toGUIObject w)
                                        

-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

iconMask :: (Window w, BitMapDesignator h) => h -> Config (Icon w)
iconMask s icon =  setBitMapHandle icon "iconmask" (toBitMap s) False

getIconMask :: Window w => Icon w -> IO BitMapHandle
getIconMask icon = getBitMapHandle icon "iconmask"
