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

module Space (

  Space,
  newSpace,

) where

import Core
import Frame
import Configuration
import Resources
import GUIObject
import Destructible
import Synchronized
import Geometry
import Computation
import BaseClasses(Widget)
import Packer


-- -----------------------------------------------------------------------
-- Space type
-- -----------------------------------------------------------------------

data Space = Space Distance Frame


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

newSpace :: Container par => par -> Distance -> [Config Space] -> IO Space
newSpace par d ol =
  do
    f <- newFrame par []
    configure (Space d f) (defaults : ol)
  where defaults = orient Vertical


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq Space where
  (Space _ f1) == (Space _ f2) = f1 == f2

instance GUIObject Space where 
  toGUIObject (Space d f) = toGUIObject f
  cname _ = "Space"

instance Destroyable Space where
  destroy   = destroy . toGUIObject

instance Widget Space

instance Synchronized Space where
  synchronize w = synchronize (toGUIObject w)

instance HasColour Space where
  legalColourID = hasBackGroundColour

instance HasOrientation Space where
  orient or s @ (Space d f) =
    configure f (case or of Horizontal -> [{-fill Vertical,-} width d,
                                           height 0]
                            Vertical -> [{-fill Horizontal,-} height d,
                                         width 0]) >> 
    return s
