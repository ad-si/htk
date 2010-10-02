{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides access to window icons.
module HTk.Components.Icon (
  Icon(..),
  iconMask,
  getIconMask

) where

import HTk.Kernel.Core
import HTk.Kernel.Configuration
import HTk.Components.BitMap
import Util.Computation
import Events.Synchronized
import HTk.Containers.Window

-- -----------------------------------------------------------------------
-- type icon's
-- -----------------------------------------------------------------------

-- | The @Icon@ datatype.
data Window w => Icon w = Icon w deriving (Eq,Ord)


-- -----------------------------------------------------------------------
-- instantions
-- -----------------------------------------------------------------------

-- | Internal.
instance Window w => GUIObject (Icon w) where
  toGUIObject (Icon win) = toGUIObject win
  cname _ = "Icon"
  cset (Icon win) cid val = cset win cid val >> return (Icon win)
  cget (Icon win) cid = cget win cid

{- only destroys the window, so this should not be necessary
instance Window w => Destroyable (Icon w) where
  destroy = destroy . toGUIObject
-}

-- | You can the the corresponding bitmap for an icon.
instance Window w => HasBitMap (Icon w) where
  bitmap s icon   = setBitMapHandle icon "iconbitmap" (toBitMap s) False
  getBitMap icon  = getBitMapHandle icon "iconbitmap"

-- | You can set the name on the icon.
instance (Window w, GUIValue v) => HasText (Icon w) v where
  -- Sets the name on the icon.
  text s icon  = cset icon "iconname" s
  -- Gets the name on the icon.
  getText icon = cget icon "iconname"

-- | You can set the location of an icon.
instance Window w => HasPosition (Icon w) where
  -- Sets the location of the icon.
  position p icon = cset icon "iconposition" p
  -- Gets the location of the icon.
  getPosition icon = cget icon "iconposition"

-- | You can synchronize on an icon object.
instance Window w => Synchronized (Icon w) where
  -- Synchronizes on an icon object.
  synchronize w = synchronize (toGUIObject w)


-- -----------------------------------------------------------------------
-- config options
-- -----------------------------------------------------------------------

-- | Sets the corresponding icon mask.
iconMask :: (Window w, BitMapDesignator h) => h -> Config (Icon w)
iconMask s icon =  setBitMapHandle icon "iconmask" (toBitMap s) False

-- | Gets the corresponding icon mask.
getIconMask :: Window w => Icon w -> IO BitMapHandle
getIconMask icon = getBitMapHandle icon "iconmask"
