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

module SelectBox (

  SelectBox,
  newSelectBox,

  addButton,
  addSpace,

  getDefault,
  selectDefault

) where

import HTk
import GUIObject
import BaseClasses(Widget)
import Frame
import Button
import Space
import ReferenceVariables
import Packer


-- -----------------------------------------------------------------------
-- SelectBox type
-- -----------------------------------------------------------------------

data SelectBox a = SelectBox Box (Maybe (Frame,Int)) (Ref [Button a])

type Elements a = [Button a]


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

newSelectBox :: Container par =>
                par -> Maybe Int -> [Config (SelectBox a)] ->
                IO (SelectBox a)
newSelectBox par Nothing ol =
  do
    b <- newHBox par []
    pack b [Expand On, Fill X]
    em <- newRef []
    configure (SelectBox b Nothing em) ol
newSelectBox par (Just i) ol =
  do
    b <- newHBox par []
    pack b [Expand On, Fill X]
    em <- newRef []
    f <- newFrame b [relief Sunken, borderwidth 1]
    pack f []
    configure (SelectBox b (Just (f,i)) em) ol


-- -----------------------------------------------------------------------
-- SelectBox instances
-- -----------------------------------------------------------------------

instance Eq (SelectBox a) where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance Destroyable (SelectBox a) where
  destroy = destroy . toGUIObject

instance GUIObject (SelectBox a) where 
  toGUIObject (SelectBox b _ e) = toGUIObject b
  cname _ = "SelectBox"

instance HasColour (SelectBox a) where 
  legalColourID = hasForeGroundColour

instance Widget (SelectBox a)

instance HasSize (SelectBox a)

instance HasBorder (SelectBox a)

instance HasEnable (SelectBox a) where
  state st sb@(SelectBox b _ em) = 
    synchronize sb (do
                      ibs <- getRef em
                      foreach ibs (\ib -> configure ib [state st])
                      return sb)
  getState sb = do
                  b <- isEnabled sb
                  if b then return Normal else return Disabled
  isEnabled sb@(SelectBox b _ em) = 
    synchronize sb (do
                      ibs <- getRef em
                      sl <- sequence (map getState ibs)
                      return (foldr (||) False (map (/= Disabled) sl)))

instance Synchronized (SelectBox a) where
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- selection
-- -----------------------------------------------------------------------

selectDefault :: SelectBox a -> IO ()
selectDefault sb =
  do
    mbt <- getDefault sb
    incase mbt (\bt -> flash bt >> invoke bt)

getDefault :: SelectBox a -> IO (Maybe (Button a))
getDefault (SelectBox b Nothing em) = return Nothing
getDefault (SelectBox b (Just (f,i)) em) =
  do
    bts <- getRef em
    return (Just (bts !! i))


-- -----------------------------------------------------------------------
-- elements
-- -----------------------------------------------------------------------

addSpace :: SelectBox a -> Distance -> IO Space
addSpace sb@(SelectBox b _ em) dist =
  do
    s <- newSpace b dist [orient Horizontal]
    pack s []
    return s

addButton :: SelectBox a -> [Config (Button a)] -> [PackOption] ->
             IO (Button a)
addButton sb@(SelectBox b Nothing em) cnf pcnf =
  synchronize sb (do
                    bt <- newButton b cnf
                    pack bt pcnf
                    changeRef em (\el -> el ++ [bt])
                    return bt)
addButton sb@(SelectBox b (Just (f,i)) em) cnf pcnf =
  synchronize sb (do
                    el <- getRef em
                    let is_default = (i == length el + 1)

                    putStrLn (show (length el) ++"\n")

                    if is_default then putStrLn "default" else done

                    bt <- if is_default then newButton f cnf
                          else newButton b cnf
                    (if is_default then
                       do
                         bt <- newButton f cnf
                         pack bt [Side AtLeft, PadX (cm 0.2),
                                  PadY (cm 0.1)]
                         pack f (pcnf ++ [Side AtLeft, PadX (cm 0.2),
                                          PadY (cm 0.1)])
                     else
                       do
                         bt <- newButton b cnf
                         pack bt (Side AtLeft : pcnf))
                    setRef em (el ++ [bt])
                    return bt)



