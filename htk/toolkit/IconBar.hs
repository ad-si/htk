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
data IconBar = IconBar Box (Ref [Either Separator Button])


-- -----------------------------------------------------------------------
-- Commands
-- -----------------------------------------------------------------------

newIconBar :: Container par => par -> [Config IconBar] -> IO IconBar
newIconBar par cnf =
  do
    b <- newBox par Rigid []
    em <- newRef []
    configure (IconBar b em) cnf


-- -----------------------------------------------------------------------
-- IconBar Instances
-- -----------------------------------------------------------------------

instance Eq IconBar where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject IconBar where
  toGUIObject (IconBar b e) = toGUIObject b
  cname _ = "IconBar"

instance Destroyable IconBar where
  destroy = destroy . toGUIObject

instance HasColour IconBar where
  legalColourID = hasForeGroundColour

instance Widget IconBar where 
  cursor c ib@(IconBar b pv) =
    synchronize ib
      (do
         configure b [cursor c]
         bts <- getIconButtons ib
         foreach bts (cursor c)
         return ib)

--instance ChildWidget IconBar

instance HasSize IconBar

instance HasBorder IconBar

instance HasEnable IconBar where
  state st ib = 
    synchronize ib (do
                      ibs <- getIconButtons ib
                      foreach ibs (\ib -> configure ib [state st])
                      return ib)
  getState ib = do
                  b <- isEnabled ib
                  if b then return Normal else return Disabled

  isEnabled ib =
    synchronize ib (do
                      ibs <- getIconButtons ib
                      sl <- sequence (map getState ibs)
                      return (foldr (||) False (map (/= Disabled) sl)) )

instance HasOrientation IconBar where 
  orient o sb@(IconBar b bts) =
    do
      orient o b
      return sb
  getOrient (IconBar b bts) = getOrient b

instance Synchronized IconBar where
  synchronize w = synchronize (toGUIObject w)


-- -----------------------------------------------------------------------
-- Parent/Child Relationship
-- -----------------------------------------------------------------------

addSeparator :: IconBar -> IO Separator
addSeparator ib@(IconBar box _) =
  do
    or <- getOrient ib
    f <- newFrame box [case or of
                         Vertical -> height 5
                         Horizontal -> width 5]
    pack f []
    return f

addButton :: IconBar -> [Config Button] -> IO Button
addButton ib@(IconBar box _) cnf =
  do
    b <- newButton box cnf
    pack b []
    return b


-- -----------------------------------------------------------------------
-- Aux
-- -----------------------------------------------------------------------

getIconButtons :: IconBar -> IO [Button]
getIconButtons (IconBar _ elemsref) =
  do
    elems <- getRef elemsref
    return (map (\ (Right b) -> b) (buttons elems))
  where buttons elems = filter (either (\_ -> False) (\_ -> True)) elems

getIconBarItems :: IconBar -> IO [Either Frame Button]
getIconBarItems (IconBar _ elemsref) = getRef elemsref
