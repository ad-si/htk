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

module Box (

  Flexibility(..),
  Box,

  newBox,
  newHBox,
  newVBox,
  newHFBox,
  newVFBox        

) where

import Core
import BaseClasses(Widget)
import Configuration
import Resources
import Frame
import Destructible
import Computation
import Synchronized
import ReferenceVariables
import Packer


-- -----------------------------------------------------------------------
-- horizontal/vertical box 
-- -----------------------------------------------------------------------

data Box = Box GUIOBJECT


-- -----------------------------------------------------------------------
-- commands
-- -----------------------------------------------------------------------

newBox :: Container par => par -> Flexibility -> [Config Box] -> IO Box
newBox par fl confs =
  do
    w <- createWidget (toGUIObject par) (BOX cdefault fl)
    configure (Box  w) confs

newHBox :: Container par => par -> [Config Box] -> IO Box
newHBox par ol = newBox par Rigid ((orient Horizontal) : ol)

newVBox :: Container par => par -> [Config Box] -> IO Box
newVBox par ol = newBox par Rigid ((orient Vertical) : ol)

newHFBox :: Container par => par -> [Config Box] -> IO Box
newHFBox par ol = newBox par Flexible ((orient Horizontal) : ol)

newVFBox :: Container par => par -> [Config Box] -> IO Box
newVFBox par ol = newBox par Flexible ((orient Vertical) : ol)


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq Box where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject Box where 
  toGUIObject (Box w) = toGUIObject w
  cname _ = "Box"

instance Destroyable Box where
  destroy   = destroy . toGUIObject

instance Synchronized Box where
  synchronize = synchronize . toGUIObject

instance Widget Box

instance Container Box

instance HasBorder Box

instance HasColour Box where 
  legalColourID = hasBackGroundColour

instance HasOrientation Box where
  orient or box@(Box w) =
    do
      BOX or' fl <- getObjectKind w
      setObjectKind w (BOX or fl)
      return box
  getOrient (Box w) =
    do
      BOX or _ <- getObjectKind w
      return or

instance HasSize Box
