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
-- A simple <strong>icon bar</strong> containing buttons and separators.
module IconBar (

  module BitMap,

  IconBar,
  newIconBar,

  addSeparator,
  addButton,

  Button,
  Separator,

  getIconButtons,
  getIconBarItems
        
)

where

import HTk
import BitMap
import Image
import ReferenceVariables
import Configuration
import Resources
import Frame
import GUIObject


-- -----------------------------------------------------------------------
-- IconBar Type
-- -----------------------------------------------------------------------

type Separator = Frame

---
-- The <code>IconBar</code> datatype.
data IconBar = IconBar Box (Ref [Either Separator Button])


-- -----------------------------------------------------------------------
-- Commands
-- -----------------------------------------------------------------------

---
-- Creates a new icon bar and returns a handler.
-- @param par     - the parent widget (which has to be a container
--                - widget).
-- @param cnf     - the list of configuration options for this icon bar.
-- @return result - An icon bar.
newIconBar :: Container par => par -> [Config IconBar] -> IO IconBar
newIconBar par cnf =
  do
    b <- newBox par Rigid []
    em <- newRef []
    configure (IconBar b em) cnf


-- -----------------------------------------------------------------------
-- IconBar Instances
-- -----------------------------------------------------------------------

---
-- Internal.
instance Eq IconBar where
---
-- Internal.
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

---
-- Internal.
instance GUIObject IconBar where
---
-- Internal.
  toGUIObject (IconBar b e) = toGUIObject b
---
-- Internal.
  cname _ = "IconBar"

---
-- An icon bar can be destroyed.
instance Destroyable IconBar where
---
-- Destroys an icon bar.
  destroy = destroy . toGUIObject

---
-- An icon bar has a configureable foreground and background colour.
instance HasColour IconBar where
---
-- Internal.
  legalColourID = hasForeGroundColour

---
-- An icon bar has standard widget properties
-- (concerning focus, cursor).
instance Widget IconBar where 
  cursor c ib@(IconBar b pv) =
    synchronize ib
      (do
         configure b [cursor c]
         bts <- getIconButtons ib
         foreach bts (cursor c)
         return ib)

---
-- An icon bar has a configureable size.
instance HasSize IconBar

---
-- An icon bar has a configureable border.
instance HasBorder IconBar

---
-- An icon bar is a stateful widget, it can be enabled or disabled.
instance HasEnable IconBar where
---
-- Sets the icon bar's state.
  state st ib = 
    synchronize ib (do
                      ibs <- getIconButtons ib
                      foreach ibs (\ib -> configure ib [state st])
                      return ib)
---
-- Gets the icon bar's state.
  getState ib = do
                  b <- isEnabled ib
                  if b then return Normal else return Disabled
---
-- <code>True</code> if the icon bar is enabled.
  isEnabled ib =
    synchronize ib (do
                      ibs <- getIconButtons ib
                      sl <- sequence (map getState ibs)
                      return (foldr (||) False (map (/= Disabled) sl)) )

---
-- An icon bar has either a vertical or horizontal orientation.
instance HasOrientation IconBar where 
---
-- Sets the icon bar's orientation.
  orient o sb@(IconBar b bts) =
    do
      orient o b
      return sb
---
-- Gets the icon bar's orientation.
  getOrient (IconBar b bts) = getOrient b

---
-- You can synchronize on an icon bar object.
instance Synchronized IconBar where
---
-- Synchronizes on an icon bar object.
  synchronize w = synchronize (toGUIObject w)


-- -----------------------------------------------------------------------
-- Parent/Child Relationship
-- -----------------------------------------------------------------------

---
-- Adds a separator at the end of the icon bar.
-- @param ib      - the concerned icon bar.
-- @return result - A separator.
addSeparator :: IconBar -> IO Separator
addSeparator ib@(IconBar box _) =
  do
    or <- getOrient ib
    f <- newFrame box [case or of
                         Vertical -> height 5
                         Horizontal -> width 5]
    pack f []
    return f

---
-- Adds a button at the end of the icon bar.
-- @param ib      - the concerned icon bar.
-- @param cnf     - the list of configuration options for the button to
--                - add.
-- @return result - A button.
addButton :: IconBar -> [Config Button] -> IO Button
addButton ib@(IconBar box _) cnf =
  do
    b <- newButton box cnf
    pack b []
    return b


-- -----------------------------------------------------------------------
-- Aux
-- -----------------------------------------------------------------------

---
-- Gets the buttons from an icon bar.
-- @param ib      - the concerned icon bar.
-- @return result - A list of the contained buttons.
getIconButtons :: IconBar -> IO [Button]
getIconButtons ib@(IconBar _ elemsref) =
  do
    elems <- getRef elemsref
    return (map (\ (Right b) -> b) (buttons elems))
  where buttons elems = filter (either (\_ -> False) (\_ -> True)) elems

---
-- Gets the items from an icon bar.
-- @param ib      - the concerned icon bar.
-- @return result - Alist of the contained buttons and separators.
getIconBarItems :: IconBar -> IO [Either Frame Button]
getIconBarItems ib@(IconBar _ elemsref) = getRef elemsref
