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

module Selection (

  HasIndex(..),

  Selection(..),
  HasSelection(..),
  HasSelectionIndex(..),
  HasSelectionBaseIndex(..),
  HasSelectionIndexRange(..),
  HasSelectionBaseIndexRange(..)

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Colour(toColour)
import Index
import Computation


-- -----------------------------------------------------------------------
-- selection classes
-- -----------------------------------------------------------------------

class GUIObject w => HasSelection w where
  clearSelection    :: w -> IO ()

class HasSelectionIndex w i where
  selection         :: i -> Config w
  isSelected        :: w -> i -> IO Bool

class HasSelectionBaseIndex w i where
  getSelection      :: w -> IO (Maybe i)

class HasSelectionIndexRange w i1 i2 where
  selectionRange    :: i1 -> i2 -> Config w

class HasSelectionIndex w i => HasSelectionBaseIndexRange w i where
  getSelectionStart :: w -> IO (Maybe i)
  getSelectionEnd   :: w -> IO (Maybe i)
  getSelectionRange :: w -> IO (Maybe (i,i))
  getSelectionRange w =
    do
      start <- getSelectionStart w
      end <- getSelectionEnd w
      case (start,end) of
        ((Just start), (Just end)) -> return (Just (start,end))
        _ -> return Nothing


-- -----------------------------------------------------------------------
-- handle
-- -----------------------------------------------------------------------

newtype HasSelection w => Selection w = Selection w


-- -----------------------------------------------------------------------
-- instantiations
-- -----------------------------------------------------------------------

instance GUIObject w => GUIObject (Selection w) where
  toGUIObject (Selection w) = toGUIObject w
  cname (Selection w)       = cname w

instance (HasSelection w,Widget w) => HasColour (Selection w) where
  legalColourID = hasForeGroundColour
  setColour w "background" c = cset w "selectbackground" (toColour c)
  setColour w "foreground" c = cset w "selectforeground" (toColour c)
  setColour w _ _            = return w
  getColour w "background"   = cget w "selectbackground"
  getColour w "foreground"   = cget w "selectforeground"
  getColour _ _              = return cdefault

instance (HasSelection w,Widget w) => HasBorder (Selection w) where
  borderwidth s w  = cset w "selectborderwidth" s
  getBorderwidth w = cget w "selectborderwidth"
  relief  _ w      = return w
  getRelief _      = return Raised 
